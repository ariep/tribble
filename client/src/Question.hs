{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Question where

import Common
import Components
import Types

import qualified Web.Channel as Ch
import Web.Channel.Client (sendMany, sendManyReceipt, get, getDyn, sendFileReceipt)
import Web.Widgets
import Web.Widgets.Edit
import Web.Widgets.Upload (file)

import           Control.Lens            (view, over, mapped, traverse, toListOf, preview, review)
import           Control.Lens.Extras     (is)
import           Control.Monad           (when, foldM, (<=<))
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (ReaderT, runReaderT, ask)
import           Data.Either             (isRight)
import qualified Data.ID                  as ID
import           Data.List               (find)
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Map                 as Map
import           Data.Maybe              (mapMaybe)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           Data.Time.Clock         (getCurrentTime,utctDay)
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common

import Reflex.Dom.Contrib.Utils (alertEvent)

questions :: (MonadWidget t m, MonadIO (PushM t))
  => CurrentTest t -> Make t m [ID.WithID (Decorated Question)]
questions currentTest = divClass "panel panel-default" $ do
  divClass "panel-heading" $ el "h4" $ text "Vragen"
  questions <- divClass "panel-body" $ do
    questions <- getDyn (Ch.list crudQuestions) []
    qs <- do
      qFilter <- editTextPlaceholder $ Text.pack "Zoek binnen alle vragen"
      qIDs <- sendManyReceipt filterQuestions (updated qFilter)
      filteredQuestions <- holdDynInit questions $ attachDynWith
        (flip filterIDs) questions qIDs
      both <- combineDyn (,) filteredQuestions questions
      combineDyn (\ (fqs, qs) f -> if Text.null f then qs else fqs) both qFilter
    elClass "table" "table" . el "tbody" $ simpleList qs $
      el "tr" . listQuestion currentTest
    return questions
  divClass "panel-footer" $ newQuestion
  return questions

newQuestion :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => C t m (Event t (ID.ID (Decorated Question)))
newQuestion = do
  newQuestionButton <- buttonClass "btn btn-info" "Nieuwe vraag"
  newQuestionE <- dialogue
    newQuestionButton 
    "Annuleer"
    "Sla op"
    (el "h3" $ text "Nieuwe vraag")
    (\ () -> alwaysValid $ questionForm emptyQuestion)
  let decQ = flip push newQuestionE $ \ q -> do
        dq <- date q
        return . Just $ Labelled Set.empty dq
  sendManyReceipt (Ch.create crudQuestions) decQ

-- maybe no longer necessary?
editQuestion :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Event t (ID.WithID (Decorated Question))
  -> C t m (Event t ())
editQuestion open = do
  newQuestionE <- dialogue open "Annuleer" "Sla op"
    (el "h3" $ text "Bewerk vraag")
    (alwaysValid . editLens (ID.object . undecorated) questionForm)
  sendMany (Ch.update crudQuestions) newQuestionE

questionForm :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Edit t m Question
questionForm q = do
  question <- divClass "form-group" . el "label" $ do
    text "Vraag:"
    mapDyn htmlToRt =<< editRichText (rtToHtml $ view question q)
  answer <- divClass "form-group" $ tabs
    [ Tab "Open" (is _Open $ view answer q) . el "label" $ do
      text "Antwoord:"
      mapDyn (Open . htmlToRt) =<< editRichText
        (maybe "" rtToHtml $ preview (answer . _Open) q)
    , Tab "Meerkeuze" (is _MultipleChoice $ view answer q) . el "label" $ do
      text "Antwoorden:"
      mapDyn (review _MultipleChoice) =<< editMultipleChoice
        (maybe ([], []) id $ preview (answer . _MultipleChoice) q)
    ]
  title <- mapDyn generateTitle question
  return (constDyn Question)
    >>= combineDyn (flip ($)) question
    >>= combineDyn (flip ($)) answer
    >>= combineDyn (flip ($)) title
 where
  editMultipleChoice :: Edit t m ([Choice], AnswerOrder)
  editMultipleChoice (initialChoices, order) = mdo
    inputs :: Dynamic t (Map.Map Int
      (Dynamic t Choice, Event t (Map.Map Int (Maybe Choice))))
      <- listWithKeyShallowDiff
        (Map.fromList . zip [1 ..] $ initialChoices)
        (leftmost [addChoice, removeChoice])
        $ \ k c _changed -> el "div" $ do
          dc <- editChoice (k > length initialChoices) c
          inputChange <- (Map.singleton k Nothing <$) <$>
            buttonClass "btn btn-default btn-xs" "-"
          return (dc, inputChange)

    newChoice <- buttonClass "btn btn-default btn-xs" "+"
    newKey <- mapDyn (+ length initialChoices) =<< count newChoice
    let addChoice = attachDynWith
          (\ k () -> Map.singleton k $ Just emptyChoice) newKey newChoice

    removeChoice <- switchPromptlyDyn <$>
      mapDyn (leftmost . map snd . Map.elems) inputs

    dChoices <- mapDyn Map.elems . joinDynThroughMap =<< mapDyn (fmap fst) inputs
    order <- mapDyn (zipWith const [1 ..]) dChoices
    combineDyn (,) dChoices order
   where
    editChoice :: Bool -> Edit t m Choice
    editChoice autoFocus (iCorrect, iText) = do
      correct <- mdo
        correct <- toggle iCorrect click
        click <- buttonClassM "btn btn-default btn-xs" $
          dynText =<< mapDyn (\case
            True  -> "✓"
            False -> "✗") correct
        return correct
      (dString, e) <- editText' $ renderPlain iText
      when autoFocus $ focus e
      combineDyn (,) correct =<< mapDyn plainRich dString
    emptyChoice :: Choice
    emptyChoice = (False, plainRich Text.empty)

listQuestion :: (MonadWidget t m, MonadIO (PushM t))
  => CurrentTest t -> Dynamic t (ID.WithID (Decorated Question)) -> C t m ()
listQuestion currentTest dynQ = do
  l <- mapDyn ID._object dynQ
  dynI <- mapDyn ID.__ID dynQ
  el "td" . dynText =<<
    mapDyn (Text.unpack . view (undecorated . title . titleText)) l
  elClass "td" "text-right" $ do
    forDynM dynI $ \ i -> forDynM currentTest $ \case
      Just t | not $ i `elem` toListOf
        (ID.object . undecorated . elements . traverse . _TestQuestion) t -> do
        addE <- buttonClass "btn btn-default btn-sm" "+"
        changedTest <- combineDyn addQuestion dynQ currentTest
        let changedTestE = fmapMaybe id $ tagDyn changedTest addE
        sendMany (Ch.update crudTests) changedTestE
        blank
      _ -> blank
    buttonClass "btn btn-default btn-sm" "✎" >>= editQuestion . tagDyn dynQ
    buttonClass "btn btn-default btn-sm" "×" >>= confirm
      "Weet je zeker dat je deze vraag wilt wissen?"
      "Annuleer"
      "Wis"
      >>= sendMany (Ch.delete crudQuestions) . tagDyn dynI
  blank
 where
  addQuestion :: ID.WithID (Decorated Question)
    -> Maybe (ID.WithID (Decorated Test))
    -> Maybe (ID.WithID (Decorated Test))
  addQuestion q = over (mapped . ID.object . undecorated . elements)
    (++ [TestQuestion $ ID.__ID q])

alwaysValid :: (MonadWidget t m)
  => m (Dynamic t a) -> m (Dynamic t (Either e a))
alwaysValid = (mapDyn Right =<<)

editRichText :: (MonadWidget t m, MonadIO (PushM t))
  => Edit t m Html
editRichText html = el "div" $ do
  richText
    (Just $ \ open -> dialogue open "Annuleren" "Kiezen" blank $ \ () -> do
      url <- divClass "imageSelect" $ do
        f <- file
        upload <- buttonClass "btn btn-success" "Upload"
        ws <- ask
        sendFileReceipt uploadImage $ onlySingle $ tagDyn f upload
      dUrl <- holdDyn (Left "No image uploaded") $ Right <$> url
      dynText =<< mapDyn (either ("No url: " ++) ("Url: " ++)) dUrl
      divClass "imagePreview" $ do
        flip (elDynAttr "img") blank
          =<< mapDyn (Map.singleton "src" . either (const "") id) dUrl  
      return dUrl
    )
    html
 where
  onlySingle :: (Reflex t) => Event t [a] -> Event t a
  onlySingle = fmapMaybe $ \case
    [x] -> Just x
    _   -> Nothing

filterIDs :: [ID.ID a] -> [ID.WithID a] -> [ID.WithID a]
filterIDs ids xs = mapMaybe (\ i -> find ((== i) . ID.__ID) xs) ids

