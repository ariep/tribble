{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Control.Lens            (view)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
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
  liftIO . putStrLn $ "connecting to websocket server at: " ++ server
  Ch.runC debug server $ do
    user <- holdDynInit (constDyn noUser) =<<
      Ch.get Components.currentUser
    mAccount <- holdDyn Nothing =<<
      (fmap Just <$> Ch.getMany Components.getCurrentAccount)
    accounts <- holdDyn [] =<< Ch.getMany Components.listAccounts
    header user mAccount
    forDynM_ mAccount $ \case
      Nothing      -> do
        pb <- getPostBuild
        (Ch.sendMany Components.setCurrentAccount =<<) $ Modal.modal pb $
          \ h b f () -> do
            h . el "h3" $ text "In welke gebruikersgroep wil je werken?"
            accountE <- b $ do
              -- newAccountE <- newTest user
              pickAccountE <- pickAccount accounts
              return $ leftmost [pickAccountE]
            -- cancel <- f $ buttonClass "btn btn-default" "Annuleer"
            return (accountE, never)
        return ()
      Just account -> divClass "container" . divClass "row" $ mdo
        questionCache <- Cache.createEmpty
          (Ch.changeID Components.crudQuestions)
        -- testCache <- Cache.createEmpty (Ch.changeID crudTests)
        tests <- Ch.getDyn Components.allTests [] >>= mapDyn filterDeleted
        currentTest <- divClass "col-md-7 col-lg-8" $
          displayTest user tests questionCache
        divClass "col-md-5 col-lg-4" $ questions user currentTest questionCache
        return ()

headSection = do
  stylesheet
    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  stylesheet "/main.css"
 where
  stylesheet s = elAttr "link"
    (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

header :: (MonadWidget t m) =>
  CurrentUser t -> Dynamic t (Maybe (ID.WithID Account)) -> C t m ()
header user account = elClass "nav" "navbar navbar-inverse" $ do
  divClass "navbar-header" $ do
    elAttr "img" (Map.fromList [("src","logo_klein.png")]) blank
    linkClass "Tribble" "navbar-brand"
  divClass "navbar-right" $ do
    elClass "p" "navbar-text" $ do
      dynText =<< mapDyn (Text.unpack . view (ID.object . userName)) user
      el "br" blank
      (dynText =<<) . forDyn account $ maybe "no account"
        (Text.unpack . (\ (Account t) -> t) . view ID.object)

noUser :: ID.WithID User
noUser = ID.WithID undefined $ User undefined (Text.pack "no user")
