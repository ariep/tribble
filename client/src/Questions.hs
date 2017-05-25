{-# LANGUAGE OverloadedStrings #-}
module Questions where

import Common
import qualified Components
import Components (IDMap)
import Imports
import Label
import Types
import Question

import qualified Web.Channel          as Ch
import qualified Web.Channel.Client   as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache
import           Web.Widgets
import qualified Web.Widgets.Modal    as Modal
import           Web.Widgets.Multi
import qualified Web.Widgets.Paginate as Paginate

import           Control.Lens            (from, traverse, toListOf, preview, review)
import           Control.Lens.Extras     (is)
import           Control.Monad           (join, when, foldM, (<=<))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (ReaderT, runReaderT, ask)
import           Data.Bool               (bool)
import qualified Data.Change              as Change
import qualified Data.ID                  as ID
import           Data.List               (find, findIndex, splitAt)
import qualified Data.Map                 as Map
import           Data.Maybe              (mapMaybe, catMaybes)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common
import           Text.Printf             (printf)


questions :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  ID.WithID User -> CurrentTest t -> TestCache t -> QuestionCache t -> C t m ()
questions user currentTest tCache qCache = divClass "panel panel-default" $ do
  divClass "panel-heading" $ el "h4" $ text "Vragen"
  questions <- divClass "panel-body" $ do
    qs <- do
      qFilter <- questionFilter
      qIDs <- Ch.sendManyReceipt Components.filterQuestions $ updated qFilter
      afterBuildAsync $ do
        (Ch.sendOnce_ Components.filterQuestions Nothing emptyFilter :: C t IO ())
      holdDyn [] qIDs
    elClass "table" "table" . el "tbody" $ Paginate.paginate 20 qs $
      el "tr" . listQuestion currentTest tCache qCache
    return ()
  divClass "panel-footer" $ newQuestion user
  return ()

questionFilter :: (MonadWidget t m) => Make t m QuestionFilter
questionFilter = divClass "form-group" $ do
  term <- fmap (fmap $ view $ from maybeNull) . editTextPlaceholder $
    "Zoek binnen alle vragen"
  labels_ <- divClass "form-group" $ do
    el "label" $ text "Filter:"
    editLens (setIso . maybes)
      (editMulti True editLabel Nothing) Set.empty
  return $ QuestionFilter <$> labels_ <*> term

listQuestion :: (MonadWidget t m, MonadIO (PushM t))
  => CurrentTest t -> TestCache t -> QuestionCache t
  -> Dynamic t (ID.ID (Decorated Question), Rational)
  -> C t m ()
listQuestion currentTest tCache qCache dynQW = void . dyn $ ffor dynQW $ \ (i, w) -> do
  let maybeQ = (Map.lookup i =<<) <$> Cache.current qCache
  void . dyn $ ffor maybeQ $ \case
    Nothing -> text "Loading..."
    Just q  -> do
      let o = ID._object q
      let attrs = "style" =: weighedOpacity w
      elAttr "td" attrs $ do
        -- dynText =<< mapDyn ((<> " ") . Text.pack . show . snd) dynQW
        text $ (view (undecorated . title . titleText)) o
      elClass "td" "buttonRow" $ do
        dyn $ ffor currentTest $ \case
          Just t | not $ i `elem` toListOf
            (ID.object . undecorated . elements . traverse . _TestQuestion) t -> do
            addE <- buttonClassM "btn btn-primary btn-xs"
              (elClass "span" "glyphicon glyphicon-plus" blank)
            addQuestion currentTest tCache (addE $> view ID._ID q)
          _ -> blank
        buttonClassM "btn btn-default btn-xs"
          (elClass "span" "glyphicon glyphicon-edit" blank)
          >>= editQuestion qCache . fmap (const q)
      blank

addQuestion :: (MonadWidget t m) =>
   CurrentTest t -> TestCache t -> Event t (ID.ID (Decorated Question)) -> C t m ()
addQuestion currentTest tCache qE = do
  let changedTestE = attachDynWithMaybe f currentTest qE
  Cache.update tCache changedTestE
 where
  f Nothing   _ = Nothing
  f (Just ct) q = Just [Change.MapModify (ID.__ID ct) (mempty, (mempty, (mempty,
    TestElementsChange [Change.ListAdd (-1) (TestQuestion q)]
    )))]

newQuestion :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => ID.WithID User -> C t m (Event t (ID.ID (Decorated Question)))
newQuestion user = do
  newQuestionButton <- buttonClass "btn btn-primary" "Nieuwe vraag"
  newQuestionE <- Modal.dialogue newQuestionButton
    "Annuleer" "Sla op" "btn-success"
    (el "h3" $ text "Nieuwe vraag")
    (\ () -> do
      dq <- date emptyQuestion
      let ldq = Labelled Set.empty dq
          decorated = Authored (view ID._ID user) ldq
      alwaysValid $ questionForm decorated
    )
  Ch.sendManyReceipt Components.newQuestion (snd <$> newQuestionE)

editQuestion :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => QuestionCache t
  -> Event t (ID.WithID (Decorated Question))
  -> C t m (Event t ())
editQuestion questions open = do
  newQuestionE <- Modal.dialogueExtraFooter
    open deleteButton "Annuleer" "Sla op" "btn-success"
    (el "h3" $ text "Bewerk vraag")
    (\ wq -> do
      d <- questionForm $ ID._object wq
      return $ Right . (\ (c, _) -> (ID.__ID wq, c)) <$> d
    )
  Cache.update questions $ editChange <$> newQuestionE
  return $ () <$ newQuestionE
 where
  editChange (i, c) = [Change.MapModify i c]
  deleteButton q = buttonClass "btn btn-danger"
    "Wis vraag"
    >>= Modal.confirm "Weet je zeker dat je deze vraag wilt wissen?"
      "Annuleer" "Wis" "btn-danger"
      >>= \ e -> do
        deleteChange <- return . Change.MapModify (view ID._ID q) <$>
          delete (Proxy :: Proxy Question)
        Cache.update questions (deleteChange <$ e)
        return e

weighedOpacity :: Rational -> Text
weighedOpacity = ("opacity: " <>) . Text.pack . printf "%.2f" . toDouble .
  (+ (1 / 4)) . (* (3 / 4)) . (^ 2) . toUnitInterval
 where
  toUnitInterval = max 0 . (1 -) . (/ 10)
  toDouble :: Rational -> Double
  toDouble = fromRational
