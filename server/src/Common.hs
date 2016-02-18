{-# LANGUAGE TypeSynonymInstances #-}
module Common where

import qualified DB
import           Imports

import qualified Web.Channel.Server             as S
import qualified Web.Channel.Server.Session     as S

import qualified Control.Concurrent.MVar        as MVar
import qualified Control.Concurrent.Notify      as Notify
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Map                       as Map
import qualified Data.TCache             as T
import qualified Data.TCache.Defs        as T
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Web.OAuth2.Google              as OAuth

data State
  = State
    { stores   :: MVar.MVar DB.Stores
    , accounts :: MVar.MVar DB.Accounts
    , changed  :: Notify.Notify
    }

initState = do
  s <- MVar.newMVar Map.empty
  a <- MVar.newMVar Map.empty
  n <- Notify.new
  return $ State s a n

instance MonadIO DB.DB where
  liftIO = T.stm . T.safeIOToSTM

dbRun :: State -> Bool -> DB.DB a -> S.M a
dbRun state notify d = do
  user <- S.getUser
  account <- liftIO $ MVar.modifyMVar (accounts state)
    (return . DB.userAccount (OAuth.id user, OAuth.name user))
  store <- liftIO $ MVar.modifyMVar (stores state) (DB.accountStore account)
  result <- DB.run store d
  when notify . liftIO $ Notify.notify $ changed state
  return result

utf8ByteString :: String -> LB.ByteString
utf8ByteString = B.toLazyByteString . Text.encodeUtf8Builder . Text.pack

