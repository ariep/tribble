{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module DB
  ( AccountStores
  , Store
  , initialiseGlobal
  , getStore
  , finaliseStore
  , accountStore
  , getCurrentUser
  , getCurrentAccount
  , setCurrentAccount
  , getAccounts

  , changeTests
  , changeQuestions

  , idIndex
  , questionIndex
  , questionLabelIndex
  , questionLabelTextIndex

  , DB
  , runIn
  , run
  , T.Key
  , getAll, getAllIDs
  ) where

import Common
import Components (IDMap)
import Imports
import Types

import qualified Control.Concurrent.MVar      as MVar
import           Control.Monad.Identity (Identity(Identity))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT(ReaderT), runReaderT, ask)
import qualified Data.Change                  as Ch
import qualified Data.Map                     as Map
import           Data.Maybe             (catMaybes, mapMaybe, listToMaybe)
import           Data.Ord               (Down)
import qualified Data.OrdPSQ                  as OrdPSQ
import qualified Data.OrdPSQ.Internal         as OrdPSQ
import qualified Data.SafeCopy                as SC
import qualified Data.Serialize.Get           as C
import qualified Data.Serialize.Put           as C
import qualified Data.Set                     as Set
import qualified Data.TCache                  as T
import qualified Data.TCache.Defs             as T
import qualified Data.TCache.ID               as ID
import qualified Data.TCache.Index            as T
import qualified Data.TCache.Index.Map        as IndexMap
import qualified Data.TCache.Index.Text       as IndexText
import qualified Data.Text                    as Text
import qualified Data.Text.Index              as TextIndex
import qualified Data.TST                     as TST
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import qualified Text.Pandoc                  as Pandoc
import qualified Web.Channel.Server           as S
import qualified Web.Channel.Server.Session   as S
import qualified Web.OAuth2.Google            as OAuth


-- Users and accounts

getCurrentUser :: State -> S.M LocalState E (ID.WithID User)
getCurrentUser state = do
  mvar <- localUser <$> S.getLocalState
  liftIO (MVar.readMVar mvar) >>= \case
    Just u  -> return u
    Nothing -> do
      u <- S.getUser >>= \case
        Just oauthUser -> runIn (globalStore state) $
          lookupUser (GoogleID $ OAuth.id oauthUser) (OAuth.name oauthUser)
        Nothing        -> runIn (globalStore state) $
          lookupUnregistered
      liftIO $ MVar.modifyMVar_ mvar (const $ return $ Just u)
      return u

lookupUnregistered :: DB.DB (ID.WithID User)
lookupUnregistered = do
  uKeys <- IndexMap.lookup userExtIDIndex unregisteredExtID
  i <- if Set.null uKeys
    -- The special unregistered user does not exist, so create it.
    then do
      i <- ID.addNew user
      -- Probably the test account doesn't exist either in this case.
      a <- ID.addNew $ Account $ Text.pack "Testomgeving"
      -- Make the unregistered user an editor of the test account.
      T.newDBRef $ Role i $ Editor a
      return i
    else return $ ID.fromKey $ Set.elemAt 0 uKeys
  return $ ID.WithID i user
 where
  user = User unregisteredExtID (Text.pack "Ongeregistreerd")

lookupUser :: UserExtID -> UserName -> DB.DB (ID.WithID User)
lookupUser uid uName = do
  uKeys <- IndexMap.lookup userExtIDIndex uid
  if Set.null uKeys
    then do
      let u = User uid uName
      i <- ID.addNew u
      autoAccounts <- Set.toList . maybe Set.empty getAutoAccounts <$>
        (T.readDBRef =<< T.getDBRefM "")
      for autoAccounts $ \ a ->
        T.newDBRef $ Role i $ Editor a
      return $ ID.WithID i u
    else do
      let i = ID.fromKey $ Set.elemAt 0 uKeys
      ID.lookup i

getCurrentAccount :: S.M LocalState E (ID.WithID Account)
getCurrentAccount = do
  mvar <- localAccount <$> S.getLocalState
  liftIO (MVar.readMVar mvar)

setCurrentAccount :: ID.WithID Account -> S.M LocalState E ()
setCurrentAccount acc = do
  ls <- S.getLocalState
  ms <- liftIO . MVar.tryTakeMVar $ localStore ls
  case ms of
    Just _  -> liftIO . MVar.tryTakeMVar $ localNotifications ls
    Nothing -> return Nothing
  let la = localAccount ls
  liftIO $ MVar.tryPutMVar la acc >>= \case
    True  -> return ()
    False -> MVar.modifyMVar_ la $ const $ return acc

userExtIDIndex :: IndexMap.Field (ID.WithID User) UserExtID
userExtIDIndex = IndexMap.field (view (ID.object . extID))

accountIndex :: IndexText.Field Account
accountIndex = IndexText.field (\ (Account t) -> t)

roleUserIndex :: IndexMap.Field Role (ID.ID User)
roleUserIndex = IndexMap.field (\ (Role i _) -> i)

getAccounts :: ID.ID User -> T.DB [ID.WithID Account]
getAccounts u = do
  roles <- fmap (map (\ (Role _ p) -> p) . catMaybes) . T.readDBRefs
    =<< mapM T.getDBRefM . Set.toList
    =<< IndexMap.lookup roleUserIndex u
  if GlobalAdministrator `elem` roles
    then ID.listWithID
    else fmap catMaybes . for roles $ \case
      GlobalAdministrator -> return Nothing
      Administrator a     -> r a
      Editor        a     -> r a
 where
  r a = Just <$> ID.lookup a


-- Stores

accountStore :: ID.ID Account -> AccountStores -> IO (AccountStores, Store AccountS)
accountStore i stores = case Map.lookup i stores of
  Just s  -> do
    -- putStrLn $ "Reusing store for: " ++ show a
    return (stores, s)
  Nothing -> do
    -- putStrLn $ "Initialising store for: " ++ show a
    s <- initialiseStore i
    return (Map.insert i s stores, s)

initialiseGlobal :: IO (Store GlobalS)
initialiseGlobal = do
  tc <- T.filePersist "./runtime-data/data/global"
  initialiseIndices tc
  let ns = GlobalNotifications
  return $ Store tc ns GlobalEphemeral
 where
  initialiseIndices :: T.Persist -> IO ()
  initialiseIndices p = do
    T.index p (idIndex :: IdIndex Account)
    T.index p (idIndex :: IdIndex User)
    T.index p userExtIDIndex
    T.index p accountIndex
    T.index p roleUserIndex

initialiseStore :: ID.ID Account -> IO (Store AccountS)
initialiseStore account = do
  tc <- fileStore account
  initialiseIndices tc
  ns <- newAccountNotifications
  eph <- newAccountEphemeral
  return $ Store tc ns eph
 where
  fileStore :: ID.ID Account -> IO T.Persist
  fileStore account = T.filePersist $
    "./runtime-data/data/" ++ accountPath account

  accountPath :: ID.ID Account -> String
  accountPath (ID.ID t) = "account/" ++ Text.unpack t

  initialiseIndices :: T.Persist -> IO ()
  initialiseIndices p = do
    T.index p (idIndex :: IdIndex (Decorated Question))
    T.index p (idIndex :: IdIndex (Decorated Test))
    T.index p questionIndex
    T.index p questionLabelIndex
    T.index p questionLabelTextIndex

finaliseStore :: Store t -> IO ()
finaliseStore = T.syncCache . tcache

-- Changes

changeTests :: Changes (IDMap (Decorated Test)) -> DB ()
changeTests = mapM_ go where
  go (Ch.MapAdd i t)    = ID.addNew t >> return ()
  go (Ch.MapDelete i)   = ID.refM i >>= ID.delete
  go (Ch.MapModify i c) = overM ID._IDLens (Ch.apply c) i >> return ()
changeQuestions :: Changes (IDMap (Decorated Question)) -> DB ()
changeQuestions = mapM_ go where
  go (Ch.MapAdd i t)    = ID.addNew t >> return ()
  go (Ch.MapDelete i)   = ID.refM i >>= ID.delete
  go (Ch.MapModify i c) = overM ID._IDLens (Ch.apply c) i >> return ()

-- Indices

type IdIndex o
  = IndexMap.Field (ID.WithID o) (ID.ID o)

idIndex :: IdIndex o
idIndex = IndexMap.field ID.__ID

questionIndex :: IndexText.Field (ID.WithID (Decorated Question))
questionIndex = IndexText.Fields
  (g . view (ID.object . undecorated))
  "contents"
 where
  g q = renderPlain (view question q) : h (view answer q)
  h (Open r) = [renderPlain r]
  h m        = map (renderPlain . snd) (view choices m)

questionLabelTextIndex :: IndexText.Field (ID.WithID (Decorated Question))
questionLabelTextIndex = IndexText.Fields
  (Set.toList . view (ID.object . authored . labels))
  "labels"

questionLabelIndex :: IndexMap.Fields
  (ID.WithID (Decorated Question)) [] Label
questionLabelIndex = IndexMap.fields
  (Set.toList . view (ID.object . authored . labels))

-- Database

type DB
  = T.DB

runIn :: (MonadIO m) => Store t -> DB a -> m a
runIn s = liftIO . T.atomicallySync (tcache s) . T.runDB (tcache s)

getAll :: forall a. (SC.SafeCopy a, Typeable a) => DB [ID.WithID a]
getAll = do
  set <- Set.unions . map snd <$> IndexMap.listAll (idIndex :: IdIndex a)
  refs <- traverse T.getDBRefM . Set.toList $ set
  catMaybes <$> T.readDBRefs refs

getAllIDs :: forall a. (SC.SafeCopy a, Typeable a) => DB [ID.ID a]
getAllIDs = map fst <$> IndexMap.listAll (idIndex :: IdIndex a)

instance MonadIO DB.DB where
  liftIO = T.stm . T.safeIOToSTM

type M a
  = S.M LocalState E a

run :: State -> DB a -> M a
run state d = do
  store <- getStore state
  runIn store d

getStore :: State -> M (Store AccountS)
getStore state = do
  ls <- S.getLocalState
  ms <- liftIO $ MVar.tryTakeMVar $ localStore ls
  case ms of
    -- We are already connected to the current store.
    Just s  -> return s
    -- We are not yet connected to the current store.
    Nothing -> do
      account <- liftIO $ MVar.readMVar (localAccount ls)
      -- Open the store if necessary, and return it.
      store <- liftIO $ MVar.modifyMVar
        (openStores state)
        (accountStore $ view ID._ID account)
      liftIO $ MVar.tryPutMVar (localStore ls) store
      lns <- liftIO . subscribeNotifications $ notifications store
      liftIO $ MVar.tryPutMVar (localNotifications ls) lns
      return store

-- Storage

instance (SC.SafeCopy a) => T.Serializable a where
  serialize   = C.runPutLazy . SC.safePut
  deserialize = either error id . C.runGetLazy SC.safeGet

instance T.Indexable (Map.Map k v) where
  key = const ""
instance T.Indexable Account where
  key (Account t) = Text.unpack t
instance T.Indexable Role where
  key (Role (ID.ID u) p) = Text.unpack u <> "-" <> show p
instance T.Indexable AutoAccounts where
  key = const ""

SC.deriveSafeCopy 0 'SC.base ''TextIndex.Weight
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.OrdPSQ
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.Elem
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.LTree
SC.deriveSafeCopy 0 'SC.base ''TST.TST
SC.deriveSafeCopy 0 'SC.base ''Privilege
SC.deriveSafeCopy 0 'SC.base ''Role
SC.deriveSafeCopy 0 'SC.base ''AutoAccounts


