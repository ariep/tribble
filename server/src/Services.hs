{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Services where

import           Common
import           Components
import qualified DB
import qualified Export
import           Imports
import           Types

import qualified Web.Channel                    as Ch
import qualified Web.Channel.Server             as S
import qualified Web.Channel.Server.Session     as S

import qualified Data.TCache             as T

import qualified Control.Concurrent.STM.TChan   as TChan
import qualified Control.Concurrent.STM.TVar    as TVar
import qualified Control.Concurrent.MVar        as MVar
import qualified Control.Coroutine.Monadic      as Co
import           Control.Monad           (filterM)
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.Change                    as Change
import qualified Data.Map                       as Map
import           Data.Maybe              (mapMaybe)
import qualified Data.Search.Results            as Results
import qualified Data.Set                       as Set
import qualified Data.TCache.ID                 as ID
import qualified Data.TCache.Index.Map          as IndexMap
import qualified Data.TCache.Index.Text         as IndexText
import qualified Data.Text.Index                as Index
import qualified System.Directory               as Dir
import           System.Random           (newStdGen, randomRs)
import qualified Web.OAuth2.Google              as OAuth

import qualified Data.Text as Text

channels :: Text -> S.Services State LocalState E
channels loginLink_ =
       editingQuestions
    <> ipp ippTests
        (idMap <$> DB.getAll)
        (waitChange . localTestNotify)
        (notifyChange . localTestNotify)
        DB.changeTests
    <> ipp ippQuestions
        (idMap <$> DB.getAll)
        (waitChange . localQuestionNotify)
        (notifyChange . localQuestionNotify)
        DB.changeQuestions
    <> S.Services
    [ const $ S.ServeChannel loginLink $ Co.put loginLink_ Co.>> Co.close
    , \ state -> S.ServeChannel currentUser $
        Co.pre (DB.getCurrentUser state) $ \ user ->
        Co.put user Co.>>
        Co.close
    , const $ S.ServeChannel getCurrentAccount $ Co.repeat $
        Co.pre DB.getCurrentAccount $ \ acc ->
        Co.put acc Co.>>
        Co.pre_ (liftIO . waitForAccountChange =<< S.getLocalState)
        Co.close
    , const $ S.ServeChannel setCurrentAccount $ Co.repeat $
        Co.get Co.>>= \ acc ->
        Co.pre_ (DB.setCurrentAccount acc >>
          (liftIO . signalAccountChange =<< S.getLocalState))
        Co.close
    , \ state -> S.ServeChannel listAccounts $ Co.repeat $
        Co.pre (DB.getCurrentUser state) $ \ user ->
        Co.pre (DB.runIn (globalStore state) $ DB.getAccounts $ ID.__ID user)
          $ \ accounts -> Co.put accounts Co.>>
        Co.pre_ (liftIO $ block)
        Co.close
    , \ state -> S.ServeChannel newTest $ Co.repeat $
        Co.get Co.>>= \ dTest ->
        Co.pre (DB.run state $ ID.addNew dTest) $ \ i ->
        Co.pre_ (do
          lns <- liftIO . MVar.readMVar =<< localNotifications <$> S.getLocalState
          cID <- S.getConnectionID
          liftIO . atomically $ notifyChange (localTestNotify lns)
            (undefined, cID, [Change.MapAdd i (ID.WithID i dTest)])
          ) $
        Co.put i Co.>> Co.close
    , \ state -> S.ServeChannel newQuestion $ Co.repeat $
        Co.get Co.>>= \ dQ ->
        Co.pre (DB.run state $ ID.addNew dQ) $ \ i ->
        Co.pre_ (do
          lns <- liftIO . MVar.readMVar =<< localNotifications <$> S.getLocalState
          cID <- S.getConnectionID
          liftIO . atomically $ notifyChange (localQuestionNotify lns)
            (undefined, cID, [Change.MapAdd i (ID.WithID i dQ)])
          ) $
        Co.put i Co.>> Co.close
    , const $ S.ServeChannel uploadImage $ Co.repeat $
        Co.get Co.>>= \ (Ch.File b) ->
        Co.pre (liftIO $ saveFileRandomName "images/" b) $ \ fn ->
        Co.put (Text.pack fn) Co.>>
        Co.close
    , queryService exportTest $ \ (testID, mode, format) ->
        fmap remoteUrl <$> Export.export mode format testID
    , queryService filterQuestions searchQuestions
    , queryService questionLabels searchQuestionLabel
    ]
    []

queryService :: (SafeCopy a, SafeCopy b) =>
  Ch.Channel (Co.Repeat (a Co.:?: b Co.:!: Co.Eps)) ->
  (a -> DB.DB b) ->
  State ->
  S.ServedChannel LocalState E
queryService c q state = S.ServeChannel c $ Co.repeat $
  Co.get Co.>>= \ a ->
  Co.pre (DB.run state $ q a) $ \ b ->
  Co.put b Co.>> Co.close

editingQuestions :: S.Services State LocalState E
editingQuestions = S.Services
  [ \ state -> S.ServeChannel initialEditing $ Co.repeat $
    Co.pre (liftIO . TVar.readTVarIO . ephPart . ephemeral =<< DB.getStore state) $ \ s ->
    Co.put (fmap (Set.map snd) s) Co.>>
    Co.pre_ (liftIO . waitForAccountChange =<< S.getLocalState)
    Co.close
  , \ state -> S.ServeChannel pullEditing $ Co.repeat $
    Co.pre (do
      ls <- S.getLocalState
      lns <- liftIO . MVar.readMVar $ localNotifications ls
      lID <- S.getConnectionID
      (qID, user, status) <- liftIO . atomically $ TChan.readTChan $ changeChan lns
      return $ editingChanges qID user status
      ) $ \ changes ->
      Co.put changes Co.>> Co.close
  , \ state -> S.ServeChannel pushEditing $ Co.repeat $
    Co.get Co.>>= \ (qID, status) -> Co.pre_ (do
      store <- DB.getStore state
      user <- DB.getCurrentUser state
      conID <- S.getConnectionID
      shouldNotify <- liftIO . atomically $ do
        let tvar = ephPart $ ephemeral store
        let getUsers = fmap (Set.map snd) . Map.lookup qID <$> TVar.readTVar tvar
        old <- getUsers
        TVar.modifyTVar' tvar $ case status of
          True  -> Map.insertWith Set.union qID $ Set.singleton (conID, user)
          False -> Map.adjust (Set.delete (conID, user)) qID
        new <- getUsers
        return $ old /= new
      when shouldNotify $ do
        lns <- liftIO . MVar.readMVar . localNotifications =<< S.getLocalState
        liftIO . atomically $ TChan.writeTChan (changeChan lns) (qID, user, status)
      )
    Co.close
  ]
  [ \ state ls conID -> do
      MVar.tryTakeMVar (localStore ls) >>= \case
        Nothing    -> return ()
        Just store -> do
          let tvar = ephPart . ephemeral $ store
          lns <- MVar.readMVar $ localNotifications ls
          atomically $ do
            let getUsers = Map.map (Set.map snd) <$> TVar.readTVar tvar
            old <- getUsers
            TVar.modifyTVar' tvar $ Map.map $ Set.filter $
              (/= conID) . fst
            new <- getUsers
            let toNotify = Map.differenceWith
                  (\ o n -> let d = Set.difference o n in
                    if Set.null d then Nothing else Just d)
                  old new
            for_ (Map.toList toNotify) $ \ (qID, users) ->
              for_ (Set.toList users) $ \ user ->
                TChan.writeTChan (changeChan lns) (qID, user, False)
  ]
 where
  ephPart :: Ephemeral AccountS -> TVar.TVar EditingQuestionsServer
  ephPart (AccountEphemeral tvar) = tvar
  changeChan = localEditingNotify
  editingChanges qID user True  =
    [ Change.MapModify qID [Change.SetAdd user]
    , Change.MapEnsure qID Set.empty
    ]
  editingChanges qID user False =
    [ Change.MapModify qID [Change.SetDelete user]
    ]

-- Serve an "InitialPushPull" channel.
ipp :: forall a. (Change.Changing a, SafeCopy a, SafeCopy (Changes a)) =>
  Ch.Token (Ch.InitialPushPull a) ->
  DB.DB a ->
  (LocalNotifications -> STM (ChangeInfo a)) ->
  (LocalNotifications -> (ChangeInfo a) -> STM ()) ->
  (Changes a -> DB.DB ()) ->
  S.Services State LocalState E
ipp t getInitial waitChange notifyChange applyChange = S.Services
  [ \ state -> S.ServeChannel (Ch.initial t) $ Co.repeat $
    Co.pre (DB.run state getInitial) $ \ a ->
    Co.put a Co.>>
    Co.pre_ (liftIO . waitForAccountChange =<< S.getLocalState)
    Co.close
  , const $ S.ServeChannel (Ch.pullChange t) $ Co.repeat $
    Co.pre (do
      ls <- S.getLocalState
      lns <- liftIO . MVar.readMVar $ localNotifications ls
      lID <- S.getConnectionID
      (_, i, c) <- liftIO . atomically $ waitChange lns
      -- For now, send out the change even if it originated at this client.
      -- return $ if i == lID
      --   then mempty
      --   else c
      return c
      ) $ \ c ->
      Co.put c Co.>> Co.close
  , \ state -> S.ServeChannel (Ch.pushChange t) $ Co.repeat $
    Co.get Co.>>= \ c -> Co.pre_ (do
      DB.run state (applyChange c)
      ls <- S.getLocalState
      lns <- liftIO . MVar.readMVar $ localNotifications ls
      lID <- S.getConnectionID
      liftIO . atomically $ notifyChange lns (undefined, lID, c)
      )
    Co.close
  ]
  []

-- Serve an "InitialPushPull" channel from ephemeral storage.
ippEphemeral :: forall a. (Show a,
  Change.Changing a, SafeCopy a, SafeCopy (Changes a))
  => State
  -> Ch.Token (Ch.InitialPushPull a)
  -> (Ephemeral AccountS -> TVar.TVar a)
  -> (LocalNotifications -> ChangeChan a)
  -> [S.ServedChannel LocalState E]
ippEphemeral state t ephPart changeChan =
  [ S.ServeChannel (Ch.initial t) $ Co.repeat $
    Co.pre (liftIO . TVar.readTVarIO . ephPart . ephemeral =<< DB.getStore state) $ \ a ->
    Co.put a Co.>>
    Co.pre_ (liftIO . waitForAccountChange =<< S.getLocalState)
    Co.close
  , S.ServeChannel (Ch.pullChange t) $ Co.repeat $
    Co.pre (do
      ls <- S.getLocalState
      lns <- liftIO . MVar.readMVar $ localNotifications ls
      lID <- S.getConnectionID
      (_, i, c) <- liftIO . atomically $ waitChange $ changeChan lns
      -- For now, send out the change even if it originated at this client.
      -- return $ if i == lID
      --   then mempty
      --   else c
      return c
      ) $ \ c ->
      Co.put c Co.>> Co.close
  , S.ServeChannel (Ch.pushChange t) $ Co.repeat $
    Co.get Co.>>= \ c -> Co.pre_ (do
      store <- DB.getStore state
      liftIO . atomically $ TVar.modifyTVar' (ephPart $ ephemeral store) (Change.apply c)
      liftIO . putStrLn . ("New ephemeral: " <>) . show =<<
        liftIO (TVar.readTVarIO $ ephPart $ ephemeral store)
      ls <- S.getLocalState
      lns <- liftIO . MVar.readMVar $ localNotifications ls
      lID <- S.getConnectionID
      liftIO . atomically $ notifyChange (changeChan lns) (undefined, lID, c)
      )
    Co.close
  ]


saveFileRandomName :: FilePath -> BSL.ByteString -> IO FilePath
saveFileRandomName dir b = do
  (f, localFile) <- newName
  BSL.writeFile localFile b
  return f
 where
  randomName = take 16 . randomRs ('a', 'z') <$> newStdGen
  newName = do
    f <- (dir ++) <$> randomName
    let localFile = "./runtime-data/upload/" ++ f
    Dir.doesFileExist localFile >>= \case
      True  -> newName
      False -> return (f, localFile)

mapWeight :: Index.Weight -> Rational
mapWeight = max 0 . (1 -) . (/ 10) . toRational . Index.toDouble

searchQuestions :: QuestionFilter ->
  DB.DB [(ID.ID (Decorated Question), Rational)]
searchQuestions qFilter = case view searchText qFilter of
  Nothing -> fmap (map $ flip (,) 1 . view ID._ID) $
    fmap (filter relevant) . (mapM ID.lookup =<<) $ DB.getAllIDs
  Just t  -> fmap (map $ \ (k, w, _) -> (ID.fromKey k, mapWeight w)) .
    fmap (Results.toList) .
    (Results.filterM (fmap relevant . ID.lookup . ID.fromKey . fst) =<<) $
    IndexText.lookup DB.questionIndex t
 where
  undeleted, rightLabels, relevant :: ID.WithID (Decorated Question) -> Bool
  undeleted = not . isDeleted . view (ID.object . authored . labelled)
  rightLabels = (view labelFilter qFilter `Set.isSubsetOf`) .
    view (ID.object . authored . labels)
  relevant w = undeleted w && rightLabels w

searchQuestionLabel :: Maybe Text -> DB.DB [Label]
searchQuestionLabel Nothing  = map fst <$> IndexMap.listAll DB.questionLabelIndex
searchQuestionLabel (Just t) = map (\ (_, _, l) -> l) . Results.take 10 <$>
  IndexText.lookup DB.questionLabelTextIndex t

