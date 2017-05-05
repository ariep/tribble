{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module Common where

import           Components
import           Imports
import           Types

import qualified Control.Concurrent.Event       as Event
import qualified Control.Concurrent.STM.TChan   as TChan
import qualified Control.Concurrent.STM.TVar    as TVar
import qualified Control.Concurrent.MVar        as MVar
import qualified Data.ByteString                as B
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ID                        as ID
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Data.TCache                    as T
import qualified Data.TCache.Defs               as T
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Web.Channel.Server             as S
import qualified Web.Channel.Server.Session     as S

-- Users and accounts

data Privilege
  = GlobalAdministrator
  | Administrator (ID.ID Account)
  | Editor (ID.ID Account)
  deriving (Generic, Typeable, Eq, Show)

data Role
  = Role (ID.ID User) Privilege
  deriving (Generic, Typeable, Show)

newtype AutoAccounts
  = AutoAccounts { getAutoAccounts :: Set.Set (ID.ID Account) }
  deriving (Generic, Typeable, Show)

-- Internal state

data State
  = State
    { globalStore   :: Store GlobalS
    , openStores    :: MVar.MVar AccountStores
    }

data StoreType
  = GlobalS
  | AccountS

data Store t
  = Store
    { tcache        :: T.Persist
    , notifications :: Notifications t
    , ephemeral     :: Ephemeral t
    }

type AccountStores
  = Map.Map (ID.ID Account) (Store AccountS)

-- Notifications let different clients be aware of eachother's changes.

type ChangeInfo a
  = (Proxy a, S.ConnectionID, Changes a)

type ChangeChan a
  = TChan.TChan (ChangeInfo a)

waitChange :: ChangeChan a -> STM (ChangeInfo a)
waitChange = TChan.readTChan

notifyChange :: ChangeChan a -> ChangeInfo a -> STM ()
notifyChange = TChan.writeTChan

type EditingChan
  = TChan.TChan (ID.ID (Decorated Question), ID.WithID User, Bool)

data Notifications :: StoreType -> * where
  AccountNotifications ::
    { questionNotify :: ChangeChan (IDMap (Decorated Question))
    , testNotify     :: ChangeChan (IDMap (Decorated Test    ))
    , editingNotify  :: EditingChan
    } -> Notifications AccountS
  GlobalNotifications :: Notifications GlobalS

newAccountNotifications :: IO (Notifications AccountS)
newAccountNotifications = AccountNotifications
  <$> TChan.newBroadcastTChanIO
  <*> TChan.newBroadcastTChanIO
  <*> TChan.newBroadcastTChanIO

data LocalNotifications
  = LocalNotifications
    { localQuestionNotify :: ChangeChan (IDMap (Decorated Question))
    , localTestNotify     :: ChangeChan (IDMap (Decorated Test))
    , localEditingNotify  :: EditingChan
    }

subscribeNotifications :: Notifications AccountS -> IO LocalNotifications
subscribeNotifications ns = LocalNotifications
  <$>  atomically (TChan.dupTChan (questionNotify ns))
  <*>  atomically (TChan.dupTChan (testNotify ns))
  <*>  atomically (TChan.dupTChan (editingNotify ns))

-- Ephemeral storage

data Ephemeral t where
  GlobalEphemeral  :: Ephemeral GlobalS
  AccountEphemeral :: TVar.TVar EditingQuestionsServer -> Ephemeral AccountS

newAccountEphemeral :: IO (Ephemeral AccountS)
newAccountEphemeral = AccountEphemeral <$> TVar.newTVarIO Map.empty

type EditingQuestionsServer
  = Map.Map (ID.ID (Decorated Question)) (Set.Set (S.ConnectionID, ID.WithID User))

-- "Local state" is the state per connection

data LocalState
  = LocalState
    { localUser          :: MVar.MVar (Maybe (ID.WithID User))
    , localAccount       :: MVar.MVar (ID.WithID Account)
    , localAccountE      :: Event.Event
    , localStore         :: MVar.MVar (Store AccountS)
    , localNotifications :: MVar.MVar LocalNotifications
    }
instance S.IsLocalState LocalState where
  type LoginPolicy LocalState = S.LoginOptional

newLocalState :: State -> IO LocalState
newLocalState state = LocalState
  <$> MVar.newMVar Nothing
  <*> MVar.newEmptyMVar
  <*> Event.new
  <*> MVar.newEmptyMVar
  <*> MVar.newEmptyMVar

waitForAccountChange :: LocalState -> IO ()
waitForAccountChange = Event.wait . localAccountE

signalAccountChange :: LocalState -> IO ()
signalAccountChange = Event.signal . localAccountE

-- Exceptions

data E
  = NoAccount
  deriving (Show)

-- Miscellaneous

utf8ByteString :: String -> LB.ByteString
utf8ByteString = B.toLazyByteString . Text.encodeUtf8Builder . Text.pack

block :: IO ()
block = MVar.takeMVar =<< MVar.newEmptyMVar
