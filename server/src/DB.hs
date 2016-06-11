{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module DB
  ( Stores
  , Store
  , initialiseGlobal
  , finalise
  , accountStore
  , getCurrentUser
  , getCurrentAccount

  , idIndex
  , questionIndex
  , questionLabelIndex
  , questionLabelTextIndex

  , DB
  , run
  , runNotify
  , T.Key
  , getAll, getAllIDs
  ) where

import Common
import Imports
import Types

import qualified Control.Concurrent.MVar     as MVar
import           Control.Monad.Identity (Identity(Identity))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT(ReaderT), runReaderT, ask)
import qualified Data.Map                  as Map
import           Data.Maybe             (catMaybes, mapMaybe, listToMaybe)
import           Data.Ord               (Down)
import qualified Data.OrdPSQ               as OrdPSQ
import qualified Data.OrdPSQ.Internal      as OrdPSQ
import qualified Data.SafeCopy             as SC
import qualified Data.Serialize.Get        as C
import qualified Data.Serialize.Put        as C
import qualified Data.Set                  as Set
import qualified Data.TCache               as T
import qualified Data.TCache.Defs          as T
import qualified Data.TCache.ID            as ID
import qualified Data.TCache.Index         as T
import qualified Data.TCache.Index.Map     as IndexMap
import qualified Data.TCache.Index.Text    as IndexText
import qualified Data.Text                 as Text
import qualified Data.Text.Index           as TextIndex
import qualified Data.TST                  as TST
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import qualified Text.Pandoc               as Pandoc
import qualified Web.Channel.Server         as S
import qualified Web.Channel.Server.Session as S
import qualified Web.OAuth2.Google          as OAuth


-- Users and accounts

getCurrentUser :: State -> S.M LocalState E (ID.WithID User)
getCurrentUser state = do
  mvar <- localUser <$> S.getLocalState
  liftIO (MVar.readMVar mvar) >>= \case
    Just u  -> return u
    Nothing -> do
      oauthUser <- S.getUser
      u <- run (globalStore state) $
        lookupUser (GoogleID $ OAuth.id oauthUser) (OAuth.name oauthUser)
      liftIO $ MVar.modifyMVar_ mvar (const $ return $ Just u)
      return u

lookupUser :: UserExtID -> UserName -> DB.DB (ID.WithID User)
lookupUser uid uName = do
  uKeys <- IndexMap.lookup userExtIDIndex uid
  if Set.null uKeys
    then do
      let u = User uid uName
      i <- ID.addNew u 
      return $ ID.WithID i u
    else do
      let i = ID.fromKey $ Set.elemAt 0 uKeys
      ID.lookup i

getCurrentAccount :: S.M LocalState E (Maybe (ID.WithID Account))
getCurrentAccount = do
  mvar <- localAccount <$> S.getLocalState
  liftIO (MVar.readMVar mvar)

userExtIDIndex :: IndexMap.Field (ID.WithID User) UserExtID
userExtIDIndex = IndexMap.field (view (ID.object . extID))

accountIndex :: IndexText.Field Account
accountIndex = IndexText.field (\ (Account t) -> t)

roleUserIndex :: IndexMap.Field Role (ID.ID User)
roleUserIndex = IndexMap.field (\ (Role i _) -> i)

lookupDefaultAccount :: ID.ID User -> T.DB (Maybe (ID.WithID Account))
lookupDefaultAccount u = do
  roles <- fmap (map (\ (Role _ p) -> p) . catMaybes) . T.readDBRefs
    =<< mapM T.getDBRefM . Set.toList
    =<< IndexMap.lookup roleUserIndex u
  let accs = flip mapMaybe roles $ \case
        GlobalAdministrator -> Nothing
        Administrator a     -> Just a
        Editor        a     -> Just a
  case listToMaybe accs of
    Nothing  -> return Nothing
    Just aID -> Just <$> ID.lookup aID


-- Stores

accountStore :: ID.ID Account -> Stores -> IO (Stores, Store)
accountStore i stores = case Map.lookup i stores of
  Just s  -> do
    -- putStrLn $ "Reusing store for: " ++ show a
    return (stores, s)
  Nothing -> do
    -- putStrLn $ "Initialising store for: " ++ show a
    s <- initialiseAccount i
    return (Map.insert i s stores, s)

initialiseGlobal :: IO Store
initialiseGlobal = do
  store <- T.filePersist "./runtime-data/data/global"
  initialiseIndices store
  return store
 where
  initialiseIndices :: Store -> IO ()
  initialiseIndices store = do
    T.index store (idIndex :: IdIndex Account)
    T.index store (idIndex :: IdIndex User)
    T.index store userExtIDIndex
    T.index store accountIndex
    T.index store roleUserIndex

initialiseAccount :: ID.ID Account -> IO Store
initialiseAccount account = do
  store <- fileStore account
  initialiseIndices store
  return store
 where
  fileStore :: ID.ID Account -> IO Store
  fileStore account = T.filePersist $
    "./runtime-data/data/" ++ accountPath account
  
  accountPath :: ID.ID Account -> String
  accountPath (ID.ID t) = "account/" ++ Text.unpack t

  initialiseIndices :: Store -> IO ()
  initialiseIndices store = do
    T.index store (idIndex :: IdIndex (Decorated Question))
    T.index store (idIndex :: IdIndex (Decorated Test))
    T.index store questionIndex
    T.index store questionLabelIndex
    T.index store questionLabelTextIndex

finalise :: Store -> IO ()
finalise store = T.syncCache store

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

run :: (MonadIO m) => Store -> DB a -> m a
run s = liftIO . T.atomicallySync s . T.runDB s

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

runNotify :: State -> [Topic] -> DB a -> M a
runNotify state topics d = do
  user <- getCurrentUser state
  aMVar <- localAccount <$> S.getLocalState
  cAccount <- liftIO $ MVar.readMVar aMVar
  account <- case cAccount of
    Just a  -> return a
    Nothing -> run (globalStore state)
      (lookupDefaultAccount $ view ID._ID user) >>= \case
        Just a  -> do
          liftIO $ MVar.modifyMVar_ aMVar (const $ return $ Just a)
          liftIO .atomically $ notify (Set.singleton CurrentAccount) state
          return a
        Nothing -> S.except NoAccount
  store <- liftIO $ MVar.modifyMVar
    (openStores state)
    (accountStore $ view ID._ID account)
  result <- run store d
  liftIO . atomically $ notify (Set.fromList topics) state
  return result

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

SC.deriveSafeCopy 0 'SC.base ''TextIndex.Weight
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.OrdPSQ
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.Elem
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.LTree
SC.deriveSafeCopy 0 'SC.base ''TST.TST

SC.deriveSafeCopy 0 'SC.base ''Privilege
SC.deriveSafeCopy 0 'SC.base ''Role


