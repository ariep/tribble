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

import Control.Concurrent (threadDelay)
import qualified Data.TCache             as T

import qualified Control.Concurrent.STM.TChan   as TChan
import qualified Control.Concurrent.STM.TVar    as TVar
import qualified Control.Coroutine.Monadic      as Co
import           Control.Monad           (filterM)
import qualified Data.ByteString.Lazy           as BSL
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

channels :: State -> IO [S.ServedChannel LocalState E]
channels state = do
  testChannels <- crud state _SpecificTest TestsOverview crudTests
    (Proxy :: Proxy (Decorated Test))
  questionChannels <- crud state _SpecificQuestion QuestionsOverview crudQuestions
    (Proxy :: Proxy (Decorated Question))
  ln <- atomically $ TChan.dupTChan (notifications state)
  return $ testChannels ++ questionChannels ++
    [ S.ServeChannel currentUser $
        Co.pre (DB.getCurrentUser state) $ \ user ->
        Co.put user Co.>>
        Co.close
    , S.ServeChannel currentAccount $ Co.repeat $
        Co.pre DB.getCurrentAccount $ \ acc ->
        Co.put acc Co.>>
        Co.pre_ (liftIO $ waitFor CurrentAccount ln)
        Co.close
    , S.ServeChannel allTests $ Co.repeat $
        Co.pre  (DB.runNotify state [] DB.getAll) $ \ xs ->
        Co.put xs Co.>>
        Co.pre_ (liftIO $ waitFor TestsOverview ln)
        Co.close
    , S.ServeChannel uploadImage $ Co.repeat $
        Co.get Co.>>= \ (Ch.File b) ->
        Co.pre (liftIO $ saveFileRandomName "images/" b) $ \ fn ->
        Co.put fn Co.>>
        Co.close
    , queryService state exportTest $ \ (testID, mode, format) ->
        fmap remoteUrl <$> Export.export mode format testID
    , queryService state filterQuestions searchQuestions
    , queryService state questionLabels searchQuestionLabel
    ]

queryService :: (SafeCopy a, SafeCopy b) =>
  State ->
  Ch.Channel (Co.Repeat (a Co.:?: b Co.:!: Co.Eps)) ->
  (a -> DB.DB b) ->
  S.ServedChannel LocalState E
queryService state c q = S.ServeChannel c $ Co.repeat $
  Co.get Co.>>= \ a ->
  Co.pre (DB.runNotify state [] $ q a) $ \ b ->
  Co.put b Co.>> Co.close

-- pureService :: (SafeCopy a, SafeCopy b) =>
--   Ch.Channel (Co.Repeat (a Co.:?: b Co.:!: Co.Eps)) ->
--   (a -> b) ->
--   S.ServedChannel
-- pureService c f = S.ServeChannel c $ Co.repeat $
--   Co.get Co.>>= \ a ->
--   Co.put (f a) Co.>> Co.close

crud :: forall a b. (SafeCopy b, Typeable b, a ~ Decorated b)
  => State -> (Prism' Topic (ID.ID a)) -> Topic -> Ch.Token (Ch.CRUD a) -> Proxy a ->
  IO [S.ServedChannel LocalState E]
crud state specific overview t _ = do
  localNotifications <- atomically $ TChan.dupTChan (notifications state)
  return
    [ S.ServeChannel (Ch.deleteID t) $ Co.repeat $
        Co.get Co.>>= \ i ->
        Co.pre_ (DB.runNotify state [overview] $ ID.delete =<< ID.refM i)
        Co.close
    , S.ServeChannel (Ch.createID t) $ Co.repeat $
        Co.get Co.>>= \ (x :: a) ->
        Co.pre (DB.runNotify state [overview] $ ID.addNew x) $ \ i ->
        Co.put i Co.>>
        Co.close
    , queryService state (Ch.lookupID t) ID.lookupMaybe
    , S.ServeChannel (Ch.updateID t) $ Co.repeat $
        Co.get Co.>>= \ (x :: ID.WithID a) ->
        Co.pre_ (DB.runNotify state [overview, review specific $ ID.__ID x] . void $ ID.write x)
        Co.close
    , S.ServeChannel (Ch.markDeletedID t) $ Co.repeat $
        Co.get Co.>>= \ i ->
        Co.pre_ (DB.runNotify state [overview, review specific i] $ do
          d <- liftIO getCurrentTime
          overM (ID._IDLens . authored . labelled) (deleteDated d) i
          return ())
        Co.close
    , S.ServeChannel (Ch.interestID t) $ Co.repeat $
        Co.get Co.>>= \ (i, interested) -> let t' = review specific i in
        Co.pre S.getLocalState $ \ ls ->
        Co.pre_ (liftIO $ atomically $ do
          oldInterests <- TVar.readTVar (localInterests ls)
          TVar.writeTVar (localInterests ls) $ if interested
            then Set.insert t' oldInterests
            else Set.delete t' oldInterests
          when (interested && not (Set.member t' oldInterests)) $
            notify (Set.singleton t') state)
        Co.close
    , S.ServeChannel (Ch.changeID t) $ Co.repeat $
        Co.pre S.getLocalState $ \ ls ->
        Co.pre (liftIO $ waitForSet ls localNotifications specific) $ \ i ->
        Co.pre (DB.runNotify state [] $ mapM ID.lookup i) $ \ w ->
        Co.put w Co.>> Co.close
    ]

waitForSet :: (Ord a) =>
  LocalState -> TChan.TChan (Set.Set Topic) -> Prism' Topic a -> IO [a]
waitForSet ls ln prism = waitFurther where
  waitFurther = look >>= \case
    [] -> waitFurther
    xs -> return xs
  look = atomically $ do
    t <- wait ln
    i <- TVar.readTVar $ localInterests ls
    return $ mapMaybe (preview prism) $
      Set.toList (t `Set.intersection` i)

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

