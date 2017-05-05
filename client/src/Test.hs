{-# LANGUAGE OverloadedStrings #-}
module Test where

import Common
import qualified Components
import Imports
import Question  (questionForm)
import Questions (editQuestion, newQuestion, addQuestion)
import Tests     (changeTest)
import Types

import qualified Web.Channel        as Ch
import qualified Web.Channel.Client as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache
import           Web.Widgets
import           Web.Widgets.DragAndDrop
import qualified Web.Widgets.Modal as Modal

import           Control.Lens            (view, over, mapped)
import           Control.Monad           (join, (<=<))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Change              as Change
import           Data.Foldable           (for_)
import qualified Data.ID                  as ID
import           Data.List               (find, transpose)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           GHCJS.DOM               (currentWindow)
import           GHCJS.DOM.EventM        (on, event, preventDefault, target)
import           GHCJS.DOM.Storage       (getItem, setItem)
import           GHCJS.DOM.Window        (getLocalStorage)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common

displayTest :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  ID.WithID User -> TestCache t -> QuestionCache t ->
  C t m (CurrentTest t)
displayTest user tCache qCache = divClass "panel panel-default" $ do
  currentTest <- divClass "panel-heading" $ testHeader user tCache
  editingCache <- Cache.create Components.initialEditing Components.pullEditing Components.pushEditing
  dyn $ ffor currentTest $ maybe blank $ \ t -> do
    divClass "panel-body" . el "ol" $ do
      dnd <- dragAndDrop
      let test = view (ID.object . undecorated) t
      let tElements = map (displayElement currentTest editingCache dnd) .
            zip [0 :: Int ..] . view elements $ test
      let dndFilter dragI dropI = dragI /= dropI && succ dragI /= dropI
          dropzones = map (dropzoneFilter dnd dndFilter)
            [0 .. length tElements]
      dndE <- fmap leftmost . sequence . concat . transpose $
        [dropzones, map (>> return never) tElements]
      _changedTest <- Cache.update tCache $
        attachDynWith (maybe (const mempty) moveQuestion) currentTest dndE
      blank
    divClass "panel-footer" $ leftRightAlign
      (newQuestion user >>= addQuestion currentTest tCache)
      (exportButton t)
    blank
  return currentTest
 where
  displayElement currentTest editingCache dnd (i, te) = mdo
    d <- elDynAttr "li" attrs $ toggleModes (viewMode i te) (editMode i te)
    let attrs = ffor d $ Map.singleton "class" . \case
          Left ()  -> "question viewMode"
          Right () -> "question editMode"
    case te of
      TestText _       -> return () -- TODO: track editing of text elements?
      TestQuestion qID -> Cache.update editingCache $ ffor (updated d) $ \case
        -- Entering view mode, so signal that we're not editing this question.
        Left ()  -> (qID, False)
        -- Entering edit mode, so signal that we are now editing this question.
        Right () -> (qID, True)
    return ()
   where
    viewMode i (TestText rt)      = (>> return never) $ html $ rtToHtml rt
    viewMode i (TestQuestion qId) = dynEvent $ ffor (lookupQuestion qId) $ \case
      Nothing -> (>> return never) $ text "Loading..."
      Just q  -> clickable . dragzone dnd i $ Entire $ \ makeDrag -> makeDrag $ do
        let editedD = (Map.lookup (ID.__ID q) =<<) <$> Cache.current editingCache
        dyn $ ffor editedD $ \case
          Just users
            | not (Set.null users) -> divClass "warning" $ text $
              ("Deze vraag wordt bewerkt door " <>) $
              Text.intercalate "," $ map (showUser user) $ Set.toList users
          _ -> blank
        displayQuestion . view (ID.object . undecorated) $ q
    editMode :: Int -> TestElement -> C t m (Event t ())
    editMode i (TestText rt)      = html (rtToHtml rt) >> return never
    editMode i (TestQuestion qId) = dynEvent $ ffor (lookupQuestion qId) $ \case
      Nothing -> text "Loading..." >> return never
      Just q  -> do
        changeD <- fmap fst <$> questionForm (ID._object q)
        let newQuestionChange = ffor changeD $ \ c -> [Change.MapModify (ID.__ID q) c]
        cancelE <- buttonClass "btn btn-default" "Annuleer"
        do
          removeE <- buttonClass "btn btn-warning" "Haal weg uit toets"
          let changedTest = (maybe mempty $ removeQuestion i) <$> currentTest
          Cache.update tCache $ tagDyn changedTest removeE
        savedE <- do
          e <- buttonClass "btn btn-success" "Sla op"
          Cache.update qCache $ tagDyn newQuestionChange e
          return $ () <$ e
        return $ leftmost [cancelE, savedE]
    showUser :: ID.WithID User -> ID.WithID User -> Text
    showUser me u
      | unregistered = "een ongeregistreerde gebruiker"
      | me == u      = "jezelf (in een ander venster)"
      | otherwise    = view (ID.object . userName) u
      where unregistered = view (ID.object . extID) u == unregisteredExtID
  displayQuestion q = do
    divClass "questionText" $ html . rtToHtml $ view question q
    divClass "questionAnswer" $ case view answer q of
      Open a                -> blank
      a@(MultipleChoice {}) -> elClass "ol" "multiple-choice" $
        for_ (view choices a `orderBy` view order a) $
          \ (_correct, rt) -> el "li" $ html . rtToHtml $ rt
  removeQuestion :: Int -> ID.WithID (Decorated Test)
    -> Change.Changes (Components.IDMap (Decorated Test))
  removeQuestion i t = [Change.MapModify (ID.__ID t) (mempty, (mempty, (mempty, TestElementsChange [Change.ListDelete i])))]
  moveQuestion :: ID.WithID (Decorated Test) -> (Int, Int)
    -> Change.Changes (Components.IDMap (Decorated Test))
  moveQuestion t (i, j) = [Change.MapModify (ID.__ID t) (mempty, (mempty, (mempty, TestElementsChange [Change.ListReorder i j])))]
  exportButton t = do
    exportWindow <- buttonClass "btn btn-primary" "Exporteer"
    exportE <- Modal.dialogue exportWindow
      "Annuleer" "Exporteer" "btn-primary"
      (el "h3" $ text "Exporteer toets") $ \ () -> divClass "form" $ do
        mode <- divClass "form-group" $ mapDyn (maybe OnlyQuestions id) =<<
          value <$> bootstrapButtonGroup
            (constDyn $ map (\ a -> (a, showMode a))
              [OnlyQuestions, WithAnswers]) def
        format <- divClass "form-group" $ do
          el "label" $ text "Formaat:"
          mapDyn (maybe PDF id) =<< value <$>
            bootstrapButtonGroup (constDyn $ map (\ a -> (a, Text.pack $ show a))
              [PDF, Word, LaTeX, Show]) def
        let request = (,,) (view ID._ID t)
              <$> mode
              <*> format
        return $ Right <$> request
    result <- Ch.sendManyReceipt Components.exportTest exportE
    let url = push (return . either (const Nothing) Just) result
    let exportError = push (return . either Just (const Nothing)) result
    consumeEvent $ openExternal "export" url
    Modal.info exportError "Ga door" (el "h3" $ text "Fout bij exporteren") text
    return ()
  lookupQuestion :: ID.ID (Decorated Question)
    -> Dynamic t (Maybe (ID.WithID (Decorated Question)))
  lookupQuestion i = (Map.lookup i =<<) <$> Cache.current qCache

testHeader :: (MonadWidget t m, MonadIO (PushM t))
  => ID.WithID User -> TestCache t -> C t m (CurrentTest t)
testHeader user tCache = divClass "testHeader" $ mdo
  showCurrent currentTest
  testE <- changeTest user tCache
  storageTest <- getStorageTest
  currentTestID <- holdDyn storageTest $ Just . ID.__ID <$> testE
  dyn $ maybe (return ()) setStorageTest <$> currentTestID
  currentTest <- combineDyn lookupTest currentTestID (Cache.current tCache)
  return currentTest
 where
  lookupTest :: Maybe (ID.ID (Decorated Test))
    -> Maybe (Components.IDMap (Decorated Test))
    -> Maybe (ID.WithID (Decorated Test))
  lookupTest mi mm = join $ Map.lookup <$> mi <*> mm

showCurrent :: (MonadWidget t m)
  => CurrentTest t -> C t m ()
showCurrent currentTest = void $ dyn $ ffor currentTest $ maybe blank $ \ t ->
  el "h3" . text $ view (ID.object . undecorated . name) t


showMode :: ExportMode -> Text
showMode OnlyQuestions = "Toets"
showMode WithAnswers   = "Antwoorden"

orderBy :: [a] -> AnswerOrder -> [a]
orderBy l = const l
-- orderBy l = map (l !!)

-- moveListElement :: Int -> Int -> [a] -> [a]
-- moveListElement i j xs
--   | j <= i    = insertAt j m ys ++ zs
--   | otherwise = ys ++ insertAt (j - i - 1) m zs
--  where
--   (ys, m : zs) = splitAt i xs
--   insertAt i x xs = case splitAt i xs of (ys, zs) -> ys ++ x : zs

getStorageTest :: (MonadWidget t m)
  => m (Maybe (ID.ID (Decorated Test)))
getStorageTest = liftIO currentWindow >>= \case
  Nothing -> return Nothing
  Just w  -> getLocalStorage w >>= \case
    Nothing -> return Nothing
    Just s  -> fmap ID.ID <$> getItem s currentTestKey
setStorageTest :: (MonadWidget t m)
  => ID.ID (Decorated Test) -> m ()
setStorageTest (ID.ID i) = liftIO currentWindow >>= \case
  Nothing -> return ()
  Just w  -> getLocalStorage w >>= \case
    Nothing -> return ()
    Just s  -> setItem s currentTestKey i
currentTestKey :: Text
currentTestKey = "currentTest"
