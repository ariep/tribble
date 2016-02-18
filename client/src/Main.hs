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
    divClass "main" $ mdo
      qs <- questions currentTest
      currentTest <- tests
      displayTest currentTest qs

headSection = do
  -- stylesheet "/style.css"
  stylesheet
    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
 where
  stylesheet s = elAttr "link"
    (Map.fromList [("rel", "stylesheet"), ("href", s)]) $ return ()

header = divClass "header" $ do
  elAttr "img" (Map.fromList [("src","logo_klein.png")]) blank
  text "Tribble"
  divClass "user" $ do
    dynText =<< holdDyn "…" . fmap Text.unpack =<< get showUser

