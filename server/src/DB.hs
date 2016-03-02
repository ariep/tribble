{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module DB
  ( Accounts
  , Account(Account)
  , userAccount

  , Stores
  , Store
  , initialise
  , finalise
  , accountStore

  , idIndex
  , questionIndex
  , questionLabelIndex
  , questionLabelTextIndex

  , DB
  , run
  , T.Key
  , getAll, getAllIDs
  ) where

import Imports
import Types

import           Control.Monad.Identity (Identity(Identity))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT(ReaderT), runReaderT, ask)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import           Data.Ord               (Down)
import qualified Data.OrdPSQ            as OrdPSQ
import qualified Data.OrdPSQ.Internal   as OrdPSQ
import qualified Data.SafeCopy          as SC
import qualified Data.Serialize.Get     as C
import qualified Data.Serialize.Put     as C
import qualified Data.Set               as Set
import qualified Data.TCache            as T
import qualified Data.TCache.Defs       as T
import qualified Data.TCache.ID         as ID
import qualified Data.TCache.Index      as T
import qualified Data.TCache.Index.Map  as IndexMap
import qualified Data.TCache.Index.Text as IndexText
import qualified Data.Text              as Text
import qualified Data.Text.Index        as TextIndex
import qualified Data.TST               as TST
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import qualified Text.Pandoc            as Pandoc


-- Accounts

data Account
  = Account String
  deriving (Generic, Typeable, Eq, Ord, Show)
SC.deriveSafeCopy 0 'SC.base ''Account

type Accounts
  = Map.Map UserID Account

type UserID
  = Text

type UserName
  = Text

userAccount :: (UserID, UserName) -> Accounts -> (Accounts, Account)
userAccount (uid, uName) m = case Map.lookup uid m of
  Just a  -> (m, a)
  Nothing -> (Map.insert uid newAccount m, newAccount)
 where
  newAccount = Account $ Text.unpack uName ++ "-" ++ Text.unpack uid

fromString :: String -> Account
fromString = Account

-- Stores

type Store
  = T.Persist

type Stores
  = Map.Map Account Store

accountStore :: Account -> Stores -> IO (Stores, Store)
accountStore a stores = case Map.lookup a stores of
  Just s  -> do
    -- putStrLn $ "Reusing store for: " ++ show a
    return (stores, s)
  Nothing -> do
    -- putStrLn $ "Initialising store for: " ++ show a
    s <- initialise a
    return (Map.insert a s stores, s)

initialise :: Account -> IO Store
initialise account = do
  store <- fileStore account
  initialiseIndices store
  return store
 where
  fileStore :: Account -> IO Store
  fileStore account = T.filePersist $ "data/" ++ accountPath account
  
  accountPath :: Account -> String
  accountPath (Account a) = "account/" ++ a

finalise :: Store -> IO ()
finalise store = T.syncCache store

initialiseIndices :: Store -> IO ()
initialiseIndices store = do
  T.index store (idIndex :: IdIndex (Decorated Question))
  T.index store (idIndex :: IdIndex (Decorated Test))
  T.index store questionIndex
  T.index store questionLabelIndex
  T.index store questionLabelTextIndex

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
  (Set.toList . view (ID.object . labels))
  "labels"

questionLabelIndex :: IndexMap.Fields
  (ID.WithID (Decorated Question)) [] Label
questionLabelIndex = IndexMap.fields (Set.toList . view (ID.object . labels))

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

-- Storage

instance (SC.SafeCopy a) => T.Serializable a where
  serialize   = C.runPutLazy . SC.safePut
  deserialize = either error id . C.runGetLazy SC.safeGet

instance T.Indexable (Map.Map k v) where
  key = const ""

SC.deriveSafeCopy 0 'SC.base ''TextIndex.Weight
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.OrdPSQ
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.Elem
SC.deriveSafeCopy 0 'SC.base ''OrdPSQ.LTree
SC.deriveSafeCopy 0 'SC.base ''TST.TST
