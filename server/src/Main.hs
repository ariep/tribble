{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Common
import           Config
import           Imports
import qualified Services

import qualified Web.Channel.Server             as S
import qualified Web.Channel.Server.Session     as S

import qualified Control.Concurrent.MVar        as MVar
import           Control.Exception       (bracket)
import           Control.Monad           (join, when)
import qualified Data.Acid.Local                as Acid
import qualified Data.ByteString                as BS
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Data.TCache.Index.Map          as IndexMap
import qualified Data.Text                      as Text
import qualified Data.Vault.Lazy                as Vault
import           Network.URI             (URI, uriAuthority, uriPort)
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Session            as Wai
import qualified WaiAppStatic.Types             as Wai
import qualified Web.OAuth2.Google              as OAuth
import qualified Web.ServerSession.Core         as Session
import qualified Web.ServerSession.Backend.Acid as Acid
import qualified Web.ServerSession.Frontend.Wai as Wai

main :: IO ()
main = do
  config <- readConfig
  let host = config >>. (Proxy :: Proxy '["host"])
      port = maybe 80 (read . drop 1 . uriPort) $ uriAuthority host
  state <- initState
  withStorage $ \ storage -> do
    (sessionState, static) <- setup config storage
    app <- S.application
      (S.Config sessionState host)
      <$> Services.channels state
    Warp.runSettings
      (Warp.setPort port Warp.defaultSettings)
      $ S.withWebSocketApp static app

type SD
  = Session.SessionMap

withStorage :: (Acid.AcidStorage SD -> IO a) -> IO a
withStorage = bracket
  (Acid.AcidStorage <$> Acid.openLocalState Acid.emptyState)
  (Acid.createCheckpointAndClose . Acid.acidState)

setup :: Tagged Config -> Acid.AcidStorage SD ->
  IO (Wai.State (Acid.AcidStorage SD), Wai.Application)
setup config storage = do
  vaultKey <- Vault.newKey
  (sessionState, withSession) <- S.withServerSession vaultKey id storage
  let oauth = OAuth.specifyGoogleKey
        (Text.unpack $ config >>. (Proxy :: Proxy '["oauth2","id"]))
        (Text.unpack $ config >>. (Proxy :: Proxy '["oauth2","secret"]))
        (show $ config >>. (Proxy :: Proxy '["host"]))
  return . (,) sessionState
    $ dirOr "./upload/"
    $ dirOr "./download/"
    $ dirOr "../client/static/"
    $ withSession . S.requireSession vaultKey oauth
    $ Static.staticApp $ (serveDir
      "../client/dist/build/tribble-client/tribble-client.jsexe/")
      {
        Wai.ssIndices = [Wai.unsafeToPiece "index.html"]
      }
 where
  serveDir = Static.defaultWebAppSettings 
  fallback s a = s { Static.ss404Handler = Just a }
  dirOr dir or = Static.staticApp (serveDir dir `fallback` or)

