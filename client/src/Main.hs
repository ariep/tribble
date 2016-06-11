{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Common
import qualified Components
import Questions
import Test
import Types

import qualified Web.Channel        as Ch
import qualified Web.Channel.Client as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache
import           Web.Widgets
import           Web.Widgets.Upload (file)

import           Control.Lens            (view)
import           Control.Monad.IO.Class  (MonadIO,liftIO)
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
  server <- (\ h -> "ws://" ++ h ++ "/") <$> getHost
  Ch.runC debug server $ do
    user <- holdDynInit (constDyn noUser) =<<
      Ch.get Components.currentUser
    account <- holdDyn Nothing =<< Ch.getMany Components.currentAccount
    header user account
    divClass "container" . divClass "row" $ mdo
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
