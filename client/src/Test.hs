module Test where

import Common
import qualified Components
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
import           Data.Foldable           (for_)
import qualified Data.ID                  as ID
import           Data.List               (find, transpose)
import qualified Data.Map                 as Map
import qualified Data.Text                as Text
import           GHCJS.DOM               (currentWindow)
import           GHCJS.DOM.EventM        (on, event, preventDefault, target)
import           GHCJS.DOM.Storage       (getItem, setItem)
import           GHCJS.DOM.Window        (getLocalStorage)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common

displayTest :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  CurrentUser t -> Tests t -> QuestionCache t m ->
  C t m (CurrentTest t)
displayTest user tests questionCache = divClass "panel panel-default" $ do
  currentTest <- divClass "panel-heading" $ testHeader user tests
  forDynM_ currentTest . maybe blank $ \ t -> do
    divClass "panel-body" . el "ol" $ do
      dnd <- dragAndDrop
      let test = view (ID.object . undecorated) t
      let tElements = map (displayElement currentTest dnd) .
            zip [0 :: Int ..] . view elements $ test
      let dndFilter dragI dropI = dragI /= dropI && succ dragI /= dropI
          dropzones = map (dropzoneFilter dnd dndFilter)
            [0 .. length tElements]
      dndE <- fmap leftmost . sequence . concat . transpose $
        [dropzones, map (>> return never) tElements]
      changedTest <- Ch.sendMany (Ch.updateID Components.crudTests) $
        fmapMaybe id $ attachDynWith moveQuestion currentTest dndE
      blank
    divClass "panel-footer" $ leftRightAlign
      (newQuestion user >>= addQuestion currentTest)
      (exportButton t)
    blank
  return currentTest
 where
  displayElement currentTest dnd (i, te) = mdo
    (e, d) <- elDynAttr' "li" attrs $ toggleModes
      (viewMode i te >> return (domEvent Click e))
      (editMode i te)
    attrs <- flip mapDyn d $ Map.singleton "class" . \case
      Left ()  -> "question viewMode"
      Right () -> "question editMode"
    return ()
   where
    viewMode i (TestText rt)      = html $ rtToHtml rt
    viewMode i (TestQuestion qId) = lookupQuestion qId
      >>= \ qD -> forDynM_ qD $ maybe (text "Loading...") $ \ q ->
        dragzone dnd i $ Entire $ \ makeDrag -> makeDrag $
          displayQuestion . view (ID.object . undecorated) $ q
    editMode i (TestText rt)      = html (rtToHtml rt) >> return never
    editMode i (TestQuestion qId) = lookupQuestion qId >>= \ qD ->
      forDynEvent qD $ maybe (text "Loading..." >> return never) $ \ q -> do
        newQ <- editLens ID.object questionForm q
        cancelE <- buttonClass "btn btn-default" "Annuleer"
        do
          removeE <- buttonClass "btn btn-warning" "Haal weg uit toets"
          changedTest <- mapDyn (removeQuestion i) currentTest
          Ch.sendMany (Ch.updateID Components.crudTests) $
            fmapMaybe id $ tagDyn changedTest removeE
        savedE <- buttonClass "btn btn-success" "Sla op"
          >>= Ch.sendMany (Ch.updateID Components.crudQuestions) . tagDyn newQ
        return $ leftmost [cancelE, savedE]
  displayQuestion q = do
    divClass "questionText" $ html . rtToHtml $ view question q
    divClass "questionAnswer" $ case view answer q of
      Open a                -> blank
      a@(MultipleChoice {}) -> elClass "ol" "multiple-choice" $
        for_ (view choices a `orderBy` view order a) $
          \ (_correct, rt) -> el "li" $ html . rtToHtml $ rt
  removeQuestion :: Int
    -> Maybe (ID.WithID (Decorated Test))
    -> Maybe (ID.WithID (Decorated Test))
  removeQuestion i = over (mapped . ID.object . undecorated . elements) $
    (\ xs -> take i xs ++ drop (succ i) xs)
  moveQuestion :: Maybe (ID.WithID (Decorated Test)) -> (Int, Int) ->
    Maybe (ID.WithID (Decorated Test))
  moveQuestion t (i, j) = over (mapped . ID.object . undecorated . elements)
    (moveListElement i j) t
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
            bootstrapButtonGroup (constDyn $ map (\ a -> (a, show a))
              [PDF, Word, LaTeX, Show]) def
        request <- return (constDyn $ (,,) $ view ID._ID t)
          >>= combineDyn (flip ($)) mode
          >>= combineDyn (flip ($)) format
        mapDyn Right request
    result <- Ch.sendManyReceipt Components.exportTest exportE
    let url = push (return . either (const Nothing) Just) result
    let exportError = push (return . either Just (const Nothing)) result
    consumeEvent $ openExternal url
    Modal.info exportError "Ga door" (el "h3" $ text "Fout bij exporteren")
      (text . Text.unpack)
    return ()
  lookupQuestion :: ID.ID (Decorated Question)
    -> Make t m (Maybe (ID.WithID (Decorated Question)))
  lookupQuestion i = do
    Cache.interestedNow (Ch.interestID Components.crudQuestions) i
    Cache.current questionCache ID.__ID i

testHeader :: (MonadWidget t m, MonadIO (PushM t))
  => CurrentUser t -> Tests t -> C t m (CurrentTest t)
testHeader user tests = divClass "testHeader" $ mdo
  showCurrent currentTest
  testE <- changeTest user tests
  storageTest <- getStorageTest
  currentTestID <- holdDyn storageTest $ Just . ID.__ID <$> testE
  forDynM_ currentTestID $ maybe (return ()) setStorageTest
  currentTest <- combineDyn lookupTest currentTestID tests
  return currentTest
 where
  lookupTest :: Maybe (ID.ID (Decorated Test))
    -> [ID.WithID (Decorated Test)]
    -> Maybe (ID.WithID (Decorated Test))
  lookupTest mi l = flip (maybe Nothing) mi $ \ i ->
    find ((==) i . view ID._ID) l

showCurrent :: (MonadWidget t m)
  => CurrentTest t -> C t m ()
showCurrent currentTest = forDynM_ currentTest $ maybe blank $ \ t ->
  el "h3" . text . Text.unpack $ view (ID.object . undecorated . name) t


type CurrentQuestions t
  = Dynamic t [ID.WithID (Decorated Question)]


showMode :: ExportMode -> String
showMode OnlyQuestions = "Toets"
showMode WithAnswers   = "Antwoorden"

orderBy :: [a] -> AnswerOrder -> [a]
orderBy l = const l
-- orderBy l = map (l !!)

moveListElement :: Int -> Int -> [a] -> [a]
moveListElement i j xs
  | j <= i    = insertAt j m ys ++ zs
  | otherwise = ys ++ insertAt (j - i - 1) m zs
 where
  (ys, m : zs) = splitAt i xs
  insertAt i x xs = case splitAt i xs of (ys, zs) -> ys ++ x : zs

getStorageTest :: (MonadWidget t m)
  => m (Maybe (ID.ID (Decorated Test)))
getStorageTest = liftIO currentWindow >>= \case
  Nothing -> return Nothing
  Just w  -> getLocalStorage w >>= \case
    Nothing -> return Nothing
    Just s  -> fmap ID.ID <$> getItem s "currentTest"
setStorageTest :: (MonadWidget t m)
  => ID.ID (Decorated Test) -> m ()
setStorageTest (ID.ID i) = liftIO currentWindow >>= \case
  Nothing -> return ()
  Just w  -> getLocalStorage w >>= \case
    Nothing -> return ()
    Just s  -> setItem s "currentTest" i
