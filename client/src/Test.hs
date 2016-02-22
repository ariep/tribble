module Test where

import Common
import Components
import Question (editQuestion, questionForm)
import Types

import qualified Web.Channel as Ch
import Web.Channel.Client (sendMany, sendManyReceipt, get, getDyn, sendFileReceipt)
import Web.Widgets
import Web.Widgets.DragAndDrop
import Web.Widgets.Edit

import           Control.Concurrent.MVar (newMVar, readMVar, modifyMVar_)
import           Control.Lens            (view, over, mapped)
import           Control.Monad           ((<=<))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Foldable           (for_)
import qualified Data.ID                  as ID
import           Data.List               (find, transpose)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           GHCJS.DOM               (currentWindow)
import           GHCJS.DOM.DataTransfer  (setData, getData)
import           GHCJS.DOM.Element       (mouseUp, mouseDown, setAttribute)
import           GHCJS.DOM.EventM        (on, event, preventDefault, target)
import           GHCJS.DOM.MouseEvent    (getDataTransfer)
import           GHCJS.DOM.Node          (Node, contains, getNodeName, getNodeType, getNodeValue)
import           GHCJS.DOM.NodeList      (item)
import           GHCJS.DOM.Storage       (getItem, setItem)
import           GHCJS.DOM.Window        (getLocalStorage)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common

-- schedulePostBuild: useful for focus on multiple choice options?

displayTest :: (MonadWidget t m, MonadIO (PushM t)) =>
  CurrentQuestions t -> C t m (CurrentTest t)
displayTest qs = divClass "panel panel-default" $ do
  currentTest <- divClass "panel-heading" $ tests
  forDynM_ currentTest . maybe blank $ \ t -> do
    divClass "panel-body" $ do
      dnd <- dragAndDrop
      let test = view (ID.object . undecorated) t
      let tElements = map (displayElement currentTest dnd) .
            zip [0 :: Int ..] . view elements $ test
      let dndFilter dragI dropI = dragI /= dropI && succ dragI /= dropI
          dropzones = map (dropzoneFilter dnd dndFilter)
            [0 .. length tElements]
      dndE <- fmap leftmost . sequence . concat . transpose $
        [dropzones, map (>> return never) tElements]
      changedTest <- sendMany (Ch.update crudTests) $ fmapMaybe id $
        attachDynWith moveQuestion currentTest dndE
      blank
    divClass "panel-footer" $ exportButton t >> blank
  return currentTest
 where
  displayElement currentTest dnd (i, te) = mdo
    (e, d) <- elDynAttr' "div" attrs $ toggleModes
      (viewMode te >> return (domEvent Click e))
      (editMode e i te)
    attrs <- flip mapDyn d $ Map.singleton "class" . \case
      True  -> "question viewMode"
      False -> "question editMode"
    return ()
   where
    viewMode (TestText rt)    = html $ rtToHtml rt
    viewMode (TestQuestion i) = mapDyn (find ((==) i . ID.__ID)) qs
      >>= \ qD -> forDynM_ qD $ maybe (text "Question not found") $
        displayQuestion . view (ID.object . undecorated)
    editMode e i (TestText rt)      = html (rtToHtml rt) >> return never
    editMode e i (TestQuestion qId) = mapDyn (find ((==) qId . ID.__ID)) qs
      >>= \ qD -> forDynEvent qD $ maybe (text "Question not found" >> return never) $ \ q -> do
        dragHandle dnd i
        newQ <- editLens (ID.object . undecorated) questionForm q
        cancelE <- buttonClass "btn btn-default" "Annuleer"
        savedE <- buttonClass "btn btn-success" "Sla op"
          >>= sendMany (Ch.update crudQuestions) . tagDyn newQ
        do
          removeE <- buttonClass "btn btn-default" "Haal weg uit toets"
          changedTest <- mapDyn (removeQuestion i) currentTest
          sendMany (Ch.update crudTests) $
            fmapMaybe id $ tagDyn changedTest removeE
        return $ leftmost [cancelE, savedE]
  displayQuestion q = do
    divClass "questionText" $ html . rtToHtml $ view question q
    divClass "questionAnswer" $ case view answer q of
      Open a                -> blank
      a@(MultipleChoice {}) -> for_ (view choices a `orderBy` view order a) $
        \ (_correct, rt) -> divClass "choice" $ html . rtToHtml $ rt
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
    exportWindow <- buttonClass "btn btn-info" "Exporteer"
    modal exportWindow $ \ () -> do
      el "h3" $ text "Exporteer toets"
      mode <- selectFromInitial OnlyQuestions
        (constDyn [OnlyQuestions, WithAnswers])
        (dynText <=< mapDyn showMode)
      format <- divClass "formpart" $ text "Formaat:" >> selectFromInitial PDF
        (constDyn [PDF, Word, LaTeX, Show])
        (dynText <=< mapDyn show)
      request <- return (constDyn $ (,,) $ view ID._ID t)
        >>= combineDyn (flip ($)) mode
        >>= combineDyn (flip ($)) format
      cancel <- buttonClass "btn" "Annuleer"
      export <- tagDyn request <$> buttonClass "btn" "Exporteer"
      result <- sendManyReceipt exportTest export
      let url = push (return . either (const Nothing) Just) result
      el "div" $ dynText =<< holdDyn ""
        (push (return . either (Just . Text.unpack) (const Nothing)) result)
      let opened = openExternal url
      return (opened, cancel)

tests :: (MonadWidget t m, MonadIO (PushM t))
  => C t m (CurrentTest t)
tests = divClass "row" $ mdo
  divClass "col-md-6" $ showCurrent currentTest
  testE <- divClass "col-md-6 text-right" $ do
    newTestE <- newTest
    pickTestE <- pickTest
    return $ leftmost [newTestE, pickTestE]
  storageTest <- getStorageTest
  currentTestID <- holdDyn storageTest $ Just . ID.__ID <$> testE
  forDynM_ currentTestID $ maybe (return ()) setStorageTest
  currentTest <- combineDyn lookupTest currentTestID
    =<< getDyn (Ch.list crudTests) []
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

newTest :: (MonadWidget t m, MonadIO (PushM t))
  => C t m (Event t (ID.WithID (Decorated Test)))
newTest = do
  newTestButton <- buttonClass "btn btn-info" "Maak een nieuwe toets"
  newTestE <- dialogue newTestButton "Annuleer" "Sla op"
    (el "h3" $ text "Nieuwe toets")
    (\ () -> mapDyn Right =<< testForm emptyTest)
  let decE = flip push newTestE $ \ q -> do
        dq <- date q
        return . Just $ Labelled Set.empty dq
  decD <- holdDyn undefined decE
  newTestID <- sendManyReceipt (Ch.create crudTests) decE
  return $ attachDynWith (flip ID.WithID) decD newTestID

testForm :: (MonadWidget t m, MonadIO (PushM t))
  => Edit t m Test
testForm _t = el "label" $ do
  text "Naam:"
  name_ <- mapDyn Text.pack =<< value <$> textInput def
  return (constDyn Test)
    >>= combineDyn (flip ($)) name_
    >>= combineDyn (flip ($)) (constDyn [])

pickTest :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => C t m (Event t (ID.WithID (Decorated Test)))
pickTest = do
  changeTestButton <- buttonClass "btn btn-info" "Kies een andere toets"
  tests <- getDyn (Ch.list crudTests) []
  modal changeTestButton $ \ () -> do
    el "h3" $ text "Kies een andere toets"
    pick <- fmap switchPromptlyDyn . mapDyn leftmost =<<
      simpleList tests testChoice
    cancel <- buttonClass "btn" "Annuleer"
    return (pick, cancel)
 where
  testChoice :: Dynamic t (ID.WithID (Decorated Test))
    -> C t m (Event t (ID.WithID (Decorated Test)))
  testChoice dTest = do
    (e, _) <- elAttr' "div" (Map.fromList [("class", "pick")]) $
      dynText =<< mapDyn
        (Text.unpack . view (ID.object . undecorated . name))
        dTest
    return $ tagDyn dTest $ domEvent Click e

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
