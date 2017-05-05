{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Accounts
import           Common
import qualified Components
import           Imports
import           Questions
import           Test
import           Types

import qualified Web.Channel        as Ch
import qualified Web.Channel.Client as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache
import           Web.Widgets
import qualified Web.Widgets.Modal  as Modal

import qualified Data.ID                  as ID
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Data.Time.Clock         (getCurrentTime,utctDay)
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common

-- Configuration

debug :: Bool
debug = True

-- Main

main :: IO ()
main = mainWidgetWithHead headSection $ do
  server <- getWebsocketServer
  when debug $ liftIO . putStrLn $ "connecting to websocket server at: " ++ Text.unpack server
  Ch.runC debug server $ do
    maybeUser <- (holdDyn Nothing =<<) . (fmap Just <$>) $
      Ch.get Components.currentUser
    mAccount <- holdDyn Nothing =<<
      (fmap Just <$> Ch.getMany Components.getCurrentAccount)
    loginLink <- holdDyn "(login link not available)" =<<
      Ch.get Components.loginLink
    accounts <- holdDyn [] =<< Ch.getMany Components.listAccounts
    void . dyn $ ffor maybeUser $ \case
      Nothing   -> text "no user"
      Just user -> do
        header user mAccount loginLink
        void . dyn $ ffor mAccount $ \case
          Nothing      -> do
            pb <- getPostBuild
            (Ch.sendMany Components.setCurrentAccount =<<) $ Modal.modal pb $
              \ h b f () -> do
                h . el "h3" $ text "In welke gebruikersgroep wil je werken?"
                accountE <- b $ do
                  pickAccountE <- pickAccount accounts
                  return $ leftmost [pickAccountE]
                return (accountE, never)
            return ()
          Just account -> divClass "container" . divClass "row" $ mdo
            qCache <- Cache.createIPP Components.ippQuestions
            tCache <- Cache.createIPP Components.ippTests
            -- TODO: use 'filterDeleted' around here somewhere
            currentTest <- divClass "col-md-7 col-lg-8" $
              displayTest user tCache qCache
            divClass "col-md-5 col-lg-4" $ questions user currentTest tCache qCache
            return ()

headSection :: forall t m. (DomBuilder t m) => m ()
headSection = do
  stylesheet
    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  stylesheet "/main.css"
 where
  stylesheet s = elAttr "link"
    (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

header :: forall t m. (MonadWidget t m) =>
  ID.WithID User -> Dynamic t (Maybe (ID.WithID Account)) -> Dynamic t Text -> C t m ()
header user account loginLink = elClass "nav" "navbar navbar-inverse" $ do
  divClass "navbar-header" $ do
    elAttr "img" (Map.fromList [("src","logo_klein.png")]) blank
    linkClass "Tribble" "navbar-brand"
  divClass "navbar-right" $ do
    elClass "p" "navbar-text" $ do
      showUser user
      el "br" blank
      (dynText =<<) . forDyn account $ maybe ""
        ((\ (Account t) -> t) . view ID.object)
 where
  showUser :: (MonadWidget t m) =>
    ID.WithID User -> C t m ()
  showUser wu
    | view extID u == unregisteredExtID = do
      attrs <- mapDyn (\ ll -> "href" =: ll) loginLink
      elDynAttr "a" attrs (text "Inloggen")
    | otherwise                         =
        text $ view userName $ u
   where
    u = view ID.object wu :: User

-- noUser :: ID.WithID User
-- noUser = ID.WithID undefined $ User undefined (Text.pack "no user")
