{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Common
import qualified Components
import Imports
import Types

import qualified Web.Channel        as Ch
import qualified Web.Channel.Client as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache
import           Web.Widgets
import qualified Web.Widgets.Modal  as Modal

import           Control.Lens            (view, over, mapped)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Change              as Change
import           Data.Foldable           (for_)
import qualified Data.ID                  as ID
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common


changeTest :: (MonadWidget t m, MonadIO (PushM t)) =>
  ID.WithID User -> TestCache t -> C t m (Event t (ID.WithID (Decorated Test)))
changeTest user tests = do
  open <- buttonClass "btn btn-default" "Beheer toetsen"
  Modal.modal open $ \ h b f () -> do
    h . el "h3" $ text "Beheer toetsen"
    testE <- b $ do
      newTestE <- newTest user
      pickTestE <- pickTest tests
      return $ leftmost [newTestE, pickTestE]
    cancel <- f $ buttonClass "btn btn-default" "Annuleer"
    return (testE, cancel)

newTest :: (MonadWidget t m, MonadIO (PushM t))
  => ID.WithID User -> C t m (Event t (ID.WithID (Decorated Test)))
newTest user = divClass "form-group" $ do
  newTestButton <- buttonClass "btn btn-primary" "Maak een nieuwe toets"
  newTestE <- Modal.dialogue newTestButton "Annuleer" "Sla op" "btn-success"
    (el "h3" $ text "Nieuwe toets")
    (\ () -> mapDyn Right =<< testForm emptyTest)
  let ldqE = flip push newTestE $ \ q -> do
        dq <- date q
        return . Just $ Labelled Set.empty dq
  let decE = (Authored $ view ID._ID user) <$> ldqE
  decD <- holdDyn undefined decE
  newTestID <- Ch.sendManyReceipt (Components.newTest) decE
  return $ attachDynWith (flip ID.WithID) decD newTestID

editTest :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => TestCache t
  -> Event t (ID.WithID (Decorated Test))
  -> C t m (Event t ())
editTest tests open = do
  newTest <- Modal.dialogue open "Annuleer" "Sla op" "btn-success"
    (el "h3" $ text "Bewerk toets")
    (alwaysValid . editLens (ID.object . undecorated) testForm)
  Cache.update tests $ changeTest <$> newTest
  return $ () <$ newTest
 where
  changeTest :: ID.WithID (Decorated Test) -> Change.Changes (Components.IDMap (Decorated Test))
  changeTest w = [Change.MapModify (ID.__ID w) (mempty, (mempty, (mempty,
    WholeTestChange (view (ID.object . undecorated) w)
    )))]

testForm :: (MonadWidget t m, MonadIO (PushM t))
  => Edit t m Test
testForm t = divClass "form-group" $ do
  el "label" $ text "Naam:"
  name_ <- editText (view name t)
  return (constDyn Test)
    >>= combineDyn (flip ($)) name_
    >>= combineDyn (flip ($)) (constDyn $ view elements t)

pickTest :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  TestCache t -> C t m (Event t (ID.WithID (Decorated Test)))
pickTest testCache = do
  tests <- mapDyn (maybe [] Map.elems) $ Cache.current testCache
  picksE <- simpleList tests testChoice
  fmap switchPromptlyDyn . mapDyn leftmost $ picksE
 where
  testChoice :: Dynamic t (ID.WithID (Decorated Test))
    -> C t m (Event t (ID.WithID (Decorated Test)))
  testChoice dTest = dynEvent $ ffor dTest $ \ t -> divClass "buttonRow" $ do
    pickE <- buttonClass "btn btn-default fill" $
      (view (ID.object . undecorated . name)) t
    buttonClassM "btn btn-default btn-xs"
      (elClass "span" "glyphicon glyphicon-edit" blank)
      >>= editTest testCache . fmap (const t)
    buttonClassM "btn btn-danger btn-xs"
      (elClass "span" "glyphicon glyphicon-remove" blank)
      >>= Modal.confirm "Weet je zeker dat je deze toets wilt wissen?"
        "Annuleer" "Wis" "btn-danger"
        >>= \ e -> do
          deleteChange <- return . Change.MapModify (ID.__ID t) <$> delete (Proxy :: Proxy Test)
          Cache.update testCache (deleteChange <$ e)
    return $ t <$ pickE
