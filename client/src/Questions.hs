module Questions where

import Common
import qualified Components
import Imports
import Label
import Types
import Question

import qualified Web.Channel        as Ch
import qualified Web.Channel.Client as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache
import           Web.Widgets
import qualified Web.Widgets.Modal as Modal
import           Web.Widgets.Multi

import           Control.Lens            (from, traverse, toListOf, preview, review)
import           Control.Lens.Extras     (is)
import           Control.Monad           (join, when, foldM, (<=<))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (ReaderT, runReaderT, ask)
import           Data.Bool               (bool)
import qualified Data.ID                  as ID
import           Data.List               (find, findIndex, splitAt)
import qualified Data.Map                 as Map
import           Data.Maybe              (mapMaybe, catMaybes)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common


questions :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  CurrentUser t -> CurrentTest t -> QuestionCache t m -> C t m ()
questions user currentTest qCache = divClass "panel panel-default" $ do
  divClass "panel-heading" $ el "h4" $ text "Vragen"
  questions <- divClass "panel-body" $ do
    qs <- do
      qFilter <- questionFilter
      qIDs <- Ch.sendManyReceipt Components.filterQuestions $ updated qFilter
      afterBuildAsync $ do
        (Ch.sendOnce_ Components.filterQuestions Nothing emptyFilter :: C t IO ())
      -- onEvent (updated qFilter) $ \ t -> liftIO $ putStrLn $
      --   "qFilter updated: " ++ show t
      -- onEvent qIDs $ \ ids -> liftIO $ putStrLn $
      --   "new qIDs: " ++ show ids
      holdDyn [] $ take 25 <$> qIDs
    elClass "table" "table" . el "tbody" $ simpleList qs $
      el "tr" . listQuestion currentTest qCache
    return ()
  divClass "panel-footer" $ newQuestion user
  return ()

questionFilter :: (MonadWidget t m) => Make t m QuestionFilter
questionFilter = divClass "form-group" $ do
  term <- (mapDyn (view $ from maybeNull) =<<) . editTextPlaceholder $
    Text.pack "Zoek binnen alle vragen"
  labels_ <- divClass "form-group" $ do
    el "label" $ text "Filter:"
    editLens (setIso . maybes)
      (editMulti True editLabel Nothing) Set.empty
  return (constDyn QuestionFilter)
    >>= combineDyn (flip ($)) labels_
    >>= combineDyn (flip ($)) term

listQuestion :: (MonadWidget t m, MonadIO (PushM t))
  => CurrentTest t -> QuestionCache t m
  -> Dynamic t (ID.ID (Decorated Question), Rational) ->
  C t m ()
listQuestion currentTest questionCache dynQW = do
  dynID <- mapDyn fst dynQW
  mdynQ <- (fmap joinDyn . holdDyn (constDyn Nothing) =<<) . forDynM dynID $
    \ i -> do
      Cache.interestedNow (Ch.interestID Components.crudQuestions) i
      Cache.current questionCache ID.__ID i
  forDynM_ mdynQ $ maybe blank $ \ q -> do
    let o = ID._object q
    let i = ID.__ID q
    attrs <- mapDyn (\ (_, w) -> "style" =: weighedOpacity w) dynQW
    elDynAttr "td" attrs $ do
      -- dynText =<< mapDyn (("Weight: " ++) . (" " ++) . show . snd) dynQW
      text $ (Text.unpack . view (undecorated . title . titleText)) o
    elClass "td" "buttonRow" $ do
      forDynM currentTest $ \case
        Just t | not $ i `elem` toListOf
          (ID.object . undecorated . elements . traverse . _TestQuestion) t -> do
          addE <- buttonClassM "btn btn-primary btn-xs"
            (elClass "span" "glyphicon glyphicon-plus" blank)
          addQuestion currentTest (addE $> view ID._ID q)
        _ -> blank
      buttonClassM "btn btn-default btn-xs"
        (elClass "span" "glyphicon glyphicon-edit" blank)
        >>= editQuestion . fmap (const q)
    blank

addQuestion :: (MonadWidget t m) =>
   CurrentTest t -> Event t (ID.ID (Decorated Question)) -> C t m ()
addQuestion currentTest qE = do
  let changedTestE = attachDynWithMaybe (flip f) currentTest qE
  Ch.sendMany (Ch.updateID Components.crudTests) changedTestE
  return ()
 where
  f q = over (mapped . ID.object . undecorated . elements)
    (++ [TestQuestion q])

newQuestion :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => CurrentUser t -> C t m (Event t (ID.ID (Decorated Question)))
newQuestion user = do
  newQuestionButton <- buttonClass "btn btn-primary" "Nieuwe vraag"
  newQuestionE <- Modal.dialogue newQuestionButton
    "Annuleer" "Sla op" "btn-success"
    (el "h3" $ text "Nieuwe vraag")
    (\ () -> do
      dq <- date emptyQuestion
      let ldq = Labelled Set.empty dq
      decorated <- forDyn user (\ u -> Authored (view ID._ID u) ldq)
      alwaysValid $ editDynamic questionForm decorated
    )
  Ch.sendManyReceipt (Ch.createID Components.crudQuestions) newQuestionE

editQuestion :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Event t (ID.WithID (Decorated Question))
  -> C t m (Event t ())
editQuestion open = do
  newQuestionE <- Modal.dialogueExtraFooter
    open deleteButton "Annuleer" "Sla op" "btn-success"
    (el "h3" $ text "Bewerk vraag")
    (alwaysValid . editLens ID.object questionForm)
  Ch.sendMany (Ch.updateID Components.crudQuestions) newQuestionE
 where
  deleteButton q = buttonClass "btn btn-danger"
    "Wis vraag"
    >>= Modal.confirm "Weet je zeker dat je deze vraag wilt wissen?"
      "Annuleer" "Wis" "btn-danger"
      >>= Ch.sendMany (Ch.markDeletedID Components.crudQuestions) . ($> (view ID._ID q))

-- filterIDs :: [(ID.ID a, w)] -> [ID.WithID a] -> [(ID.WithID a, w)]
-- filterIDs ids xs = mapMaybe
--   (\ (i, w) -> fmap (flip (,) w) $ find ((== i) . ID.__ID) xs) ids

weighedOpacity :: Rational -> String
weighedOpacity = ("opacity: " ++) . showRational 2 .
  (+ (1 / 4)) . (* (3 / 4)) . (^ 2)

showRational :: Int -> Rational -> String
showRational n r =
  let d = round (abs r * 10^n)
      s = show (d :: Integer)
      s' = replicate (n - length s + 1) '0' ++ s
      (h, f) = splitAt (length s' - n) s'
  in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
