{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where


import Components
import Question
import Test
import Types

import qualified Web.Channel as Ch
import Web.Channel.Client (sendMany, sendManyReceipt, get, getDyn, sendFileReceipt)
import Web.Widgets
import Web.Widgets.Edit
import Web.Widgets.Upload (file)

import           Control.Lens            (view)
import           Control.Monad.Exception (onException)
import           Control.Monad.IO.Class  (MonadIO,liftIO)
import           Control.Monad.Reader    (ReaderT, runReaderT, ask)
import qualified Data.ID                  as ID
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Data.Time.Clock         (getCurrentTime,utctDay)
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common

main :: IO ()
main = mainWidgetWithHead headSection $ do
  ws <- WS.connect (Text.pack "ws://localhost:8000/")
    `onException` (text "Error: the server could not be contacted.") 
  runReaderT page ws
 where
  page = do
    header
    divClass "container" . divClass "row" $ mdo
      currentTest <- divClass "col-md-7 col-lg-8" $ displayTest qs
      qs <- divClass "col-md-5 col-lg-4" $ questions currentTest
      return ()

headSection = do
  stylesheet
    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
  stylesheet "/main.css"
 where
  stylesheet s = elAttr "link"
    (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

header = elClass "nav" "navbar navbar-inverse" $ do
  divClass "navbar-header" $ do
    elAttr "img" (Map.fromList [("src","logo_klein.png")]) blank
    linkClass "Tribble" "navbar-brand"
  divClass "navbar-right" . elClass "p" "navbar-text" $ do
    dynText =<< holdDyn "…" . fmap Text.unpack =<< get showUser

