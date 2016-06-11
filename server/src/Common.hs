{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Common where

import           Imports
import           Types

import qualified Control.Concurrent.STM.TChan   as TChan
import qualified Control.Concurrent.STM.TVar    as TVar
import qualified Control.Concurrent.MVar        as MVar
import           Control.Lens           (makePrisms)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LB
import qualified Data.ID                 as ID
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import qualified Data.TCache             as T
import qualified Data.TCache.Defs        as T
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text

-- Users and accounts

data Privilege
  = GlobalAdministrator
  | Administrator (ID.ID Account)
  | Editor (ID.ID Account)
  deriving (Generic, Typeable, Show)

data Role
  = Role (ID.ID User) Privilege
  deriving (Generic, Typeable, Show)

-- Internal state

type Store
  = T.Persist

type Stores
  = Map.Map (ID.ID Account) Store

data State
  = State
    { globalStore   :: Store
    , openStores    :: MVar.MVar Stores
    , notifications :: TChan.TChan (Set.Set Topic)
    }

data Topic
  = CurrentAccount
  | SpecificQuestion (ID.ID (Decorated Question))
  | QuestionsOverview
  | SpecificTest     (ID.ID (Decorated Test    ))
  | TestsOverview
  deriving (Eq, Ord, Show)
makePrisms ''Topic

notify :: Set.Set Topic -> State -> STM ()
notify topics state = TChan.writeTChan (notifications state) topics

wait :: TChan.TChan (Set.Set Topic) -> STM (Set.Set Topic)
wait = TChan.readTChan

waitFor :: Topic -> TChan.TChan (Set.Set Topic) -> IO ()
waitFor topic c = waitFurther where
  waitFurther = do
    t <- atomically $ TChan.readTChan c
    if topic `Set.member` t
      then return ()
      else waitFurther

data LocalState
  = LocalState
    { localUser      :: MVar.MVar (Maybe (ID.WithID User))
    , localInterests :: TVar.TVar (Set.Set Topic)
    , localAccount   :: MVar.MVar (Maybe (ID.WithID Account))
    }

newLocalState :: State -> IO LocalState
newLocalState state = LocalState
  <$> MVar.newMVar Nothing
  <*> TVar.newTVarIO Set.empty
  <*> MVar.newMVar Nothing

-- Exceptions

data E
  = NoAccount
  deriving (Show)

-- Miscellaneous

utf8ByteString :: String -> LB.ByteString
utf8ByteString = B.toLazyByteString . Text.encodeUtf8Builder . Text.pack

