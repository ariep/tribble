module Test where

import Common
import Components
import Question (editQuestion)
import Types

import qualified Web.Channel as Ch
import Web.Channel.Client (sendMany, sendManyReceipt, get, getDyn, sendFileReceipt)
import Web.Widgets
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
import           GHCJS.DOM.Element       (mouseUp, mouseDown, dragStart, dragEnter, dragLeave, dragOver, drop, getElementsByClassName, setAttribute)
import           GHCJS.DOM.EventM        (on, event, preventDefault, target)
import           GHCJS.DOM.MouseEvent    (getDataTransfer)
import           GHCJS.DOM.Node          (Node, contains, getNodeName, getNodeType, getNodeValue)
import           GHCJS.DOM.NodeList      (item)
import           GHCJS.DOM.Storage       (getItem, setItem)
import           GHCJS.DOM.Window        (getLocalStorage)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common

import Prelude hiding (drop)


tests :: (MonadWidget t m, MonadIO (PushM t))
  => C t m (CurrentTest t)
tests = divClass "tests element" $ mdo
  showCurrent currentTest
  newTestE <- newTest
  pickTestE <- pickTest
  storageTest <- getStorageTest
  currentTestID <- holdDyn storageTest $ Just . ID.__ID
    <$> leftmost [newTestE, pickTestE]
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
  elClass "span" "currentTest" $
    text . Text.unpack $ view (ID.object . undecorated . name) t

newTest :: (MonadWidget t m, MonadIO (PushM t))
  => C t m (Event t (ID.WithID (Decorated Test)))
newTest = do
  newTestButton <- button "Maak een nieuwe toets"
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
  changeTestButton <- button "Kies een andere toets"
  tests <- getDyn (Ch.list crudTests) []
  modal changeTestButton $ \ () -> do
    el "h3" $ text "Kies een andere toets"
    pick <- fmap switchPromptlyDyn . mapDyn leftmost =<<
      simpleList tests testChoice
    cancel <- button "Annuleer"
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

-- schedulePostBuild: useful for focus on multiple choice options?

displayTest :: (MonadWidget t m, MonadIO (PushM t)) =>
  CurrentTest t -> CurrentQuestions t -> C t m ()
displayTest currentTest qs = forDynM_ currentTest . maybe blank $ \ t -> do
  let test = view (ID.object . undecorated) t
  let tElements = map displayElement . zip [0 :: Int ..] . view elements $ test
  let dropzones = map dropzone [0 .. length tElements]
  sequence_ . concat . transpose $ [dropzones, tElements]
  exportButton t
  blank
 where
  showMode :: ExportMode -> String
  showMode OnlyQuestions = "Toets"
  showMode WithAnswers   = "Antwoorden"
  displayElement (i, te) = do
    (e, ()) <- elAttr' "div" (Map.singleton "class" "testElement")
      $ case te of
        TestText rt    -> html $ rtToHtml rt
        TestQuestion i -> do
          qD <- mapDyn (find ((==) i . ID.__ID)) qs
          forDynM_ qD $ maybe (text "Question not found") $ \ q -> do
            do
              removeE <- buttonClass "smallRight" "−"
              changedTest <- mapDyn (removeQuestion i) currentTest
              sendMany (Ch.update crudTests) $
                fmapMaybe id $ tagDyn changedTest removeE
            buttonClass "smallRight" "✎" >>= editQuestion . (q <$)
            divClass "dnd-handle" $ text "↕"
            displayQuestion . view (ID.object . undecorated) $ q
    (consumeEvent =<<) . wrapDomEvent (_el_element e) (`on` mouseDown) $ do
      t :: Maybe Node <- target
      Just nl <- getElementsByClassName (_el_element e) "dnd-handle"
      Just handle <- item nl 0
      contains handle t >>= setAttribute (_el_element e) "draggable" . show
    (consumeEvent =<<) . wrapDomEvent (_el_element e) (`on` dragStart) $ do
      (event >>= getDataTransfer >>=) . flip for_ $ \ dt ->
        setData dt "text/plain" $ show i
  displayQuestion q = do
    divClass "questionText" $ html . rtToHtml $ view question q
    divClass "questionAnswer" $ case view answer q of
      Open a                -> blank
      a@(MultipleChoice {}) -> for_ (view choices a `orderBy` view order a) $
        \ (_correct, rt) -> divClass "choice" $ html . rtToHtml $ rt
  removeQuestion :: ID.ID (Decorated Question)
    -> Maybe (ID.WithID (Decorated Test))
    -> Maybe (ID.WithID (Decorated Test))
  removeQuestion i = over (mapped . ID.object . undecorated . elements) $
    filter $ \case
      TestQuestion i' -> i /= i'
      _               -> True
  dropzone i = mdo
    (e, ()) <- elDynAttr' "div" attrs $ blank
    (consumeEvent =<<) . wrapDomEvent (_el_element e) (`on` dragOver) $
      preventDefault
    enterE <- wrapDomEvent (_el_element e) (`on` dragEnter) $ return ()
    leaveE <- wrapDomEvent (_el_element e) (`on` dragLeave) $ return ()
    overD <- holdDyn False $ leftmost [True <$ enterE, False <$ leaveE]
    attrs <- flip mapDyn overD $ (Map.singleton "class") . \case
      False -> "dnd-dropzone"
      True  -> "dnd-dropzone dnd-over"
    (consumeEvent =<<) . wrapDomEvent (_el_element e) (`on` drop) $ do
      preventDefault
      (event >>= getDataTransfer >>=) . mapM_ $ \ dt -> do
        d <- getData dt "text/plain"
        liftIO . putStrLn $
          "Move element of index " ++ d ++ " to slot " ++ show i
  exportButton t = do
    exportWindow <- button "Exporteer"
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
      cancel <- button "Annuleer"
      export <- tagDyn request <$> button "Exporteer"
      result <- sendManyReceipt exportTest export
      let url = push (return . either (const Nothing) Just) result
      el "div" $ dynText =<< holdDyn ""
        (push (return . either (Just . Text.unpack) (const Nothing)) result)
      let opened = openExternal url
      return (opened, cancel)

orderBy :: [a] -> AnswerOrder -> [a]
orderBy l = const l
-- orderBy l = map (l !!)

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
