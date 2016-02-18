{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Config
  (
    Config
  , readConfig
  , (>>.)
  , Tagged
  ) where

import Data.Aeson (FromJSON, parseJSON)
import Data.Configifier
  ( (:>)
  , (:*>)
  , (:>:)
  , (>>.)
  , ToConfigCode
  , Tagged
  , configify
  , defaultSources
  )
import Data.Text   (Text)
import Network     (PortNumber)
import Network.URI (URI, parseURI)


readConfig :: IO (Tagged Config)
readConfig = configify =<< defaultSources ["./config.yaml"]

type Config
  = ToConfigCode Config'

type Config'
  =   "host"   :> URI
        :>: "Hostname of the server (used for redirects and header check), including protocol and port."
  :*> "oauth2" :> OAuth2Config'

type OAuth2Config'
  =   "id"     :> Text :>: "ID for the OAuth2 API"
  :*> "secret" :> Text :>: "Secret for the OAuth2 API"

instance FromJSON URI where
  parseJSON v = do
    s <- parseJSON v
    case parseURI s of
      Nothing -> fail $ "string fails to parse as URI: " ++ s
      Just u  -> return u
