{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Question where

import Common
import qualified Components
import Imports
import Label
import Types

import qualified Web.Channel as Ch
import           Web.Channel.Client (sendMany, sendManyReceipt, get, getDyn, sendFileReceipt)
import           Web.Widgets
import qualified Web.Widgets.Modal as Modal
import           Web.Widgets.Multi  (editMulti)
import           Web.Widgets.RichText
import           Web.Widgets.Upload (file)

import           Control.Lens            (set, traverse, toListOf, preview, review)
import           Control.Lens.Extras     (is)
import           Control.Monad           (when, foldM, (<=<))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (ReaderT, runReaderT, ask)
import           Data.Bool               (bool)
import qualified Data.Change              as Ch
import qualified Data.ID                  as ID
import           Data.List               (find, findIndex, splitAt)
import qualified Data.Map                 as Map
import           Data.Maybe              (mapMaybe)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom hiding (Key(Tab))
import           Reflex.Dom.Contrib.Widgets.Common


questionForm :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Decorated Question -> Make t m (Ch.Changes (Decorated Question), Decorated Question)
questionForm dq = do
  let q = view undecorated dq
  questionTextD <- divClass "form-group" $ do
    -- el "label" $ text "Vraag:"
    fmap htmlToRt <$> editRichText (rtToHtml $ view question q)
  aD <- divClass "form-group multipleChoiceAnswers" $ tabs
    [ Tab "Open" (is _Open $ view answer q) $ do
      -- el "label" $ text "Antwoord:"
      fmap (Open . htmlToRt) <$> editRichText
        (maybe "" rtToHtml $ preview (answer . _Open) q)
    , Tab "Meerkeuze" (is _MultipleChoice $ view answer q) $ do
      -- el "label" $ text "Antwoorden:"
      fmap (review _MultipleChoice) <$> editMultipleChoice
        (maybe (map emptyChoice [True, False], [1, 2]) id $
          preview (answer . _MultipleChoice) q)
    ]
  newLabelsD <- divClass "form-group" $ do
    el "label" $ text "Labels:"
    editLens (setIso . maybes) (editMulti True editLabel Nothing) $
      view (authored . labels) dq
  let titleD = generateTitle <$> questionTextD
      newQuestionD = Question <$> questionTextD <*> aD <*> titleD
  return $ do
    newQuestion <- newQuestionD
    newLabels <- newLabelsD
    return
      ( (mempty, (Ch.setDiff (view (authored . labels) dq) newLabels, (mempty, Ch.replace newQuestion)))
      , set (authored . labels) newLabels . set undecorated newQuestion $ dq
      )
 where
  emptyChoice :: Bool -> Choice
  emptyChoice correct = (correct, plainRich Text.empty)
  editMultipleChoice :: Edit t m ([Choice], AnswerOrder)
  editMultipleChoice = editLens trivialOrder $
    editMulti False editChoice (emptyChoice False)
   where
    editChoice :: Bool -> Edit t m Choice
    editChoice autofocus (iCorrect, iText) = do
      correct <- mdo
        correct <- toggle iCorrect click
        let attrs = ffor correct $ Map.singleton "class" .
              ("btn btn-default btn-xs mc-button " <>) .
              bool "mc-false" "mc-true"
        click <- buttonDynAttr attrs $ dynText $ bool "✗" "✓" <$> correct
        return correct
      (dString, e) <- editTextAutofocus' autofocus $ renderPlain iText
      return $ (,) <$> correct <*> (plainRich <$> dString)

setIso :: (Ord a) => Iso' (Set.Set a) [a]
setIso = iso Set.toList Set.fromList

trivialOrder :: Iso' ([a], [Int]) [a]
trivialOrder = iso fst (\ x -> (x, zipWith const [1 ..] x))

editRichText :: (MonadWidget t m, MonadIO (PushM t))
  => Edit t m Html
editRichText html = el "div" $ flip richText html $ Just $ \ open ->
  Modal.dialogue open "Annuleren" "Kiezen" "btn-primary" blank $ \ () -> do
    url <- divClass "imageSelect" $ do
      f <- file
      upload <- buttonClass "btn btn-success" "Upload"
      ws <- ask
      sendFileReceipt Components.uploadImage $ onlySingle $ tagDyn f upload
    dUrl <- holdDyn (Left "No image uploaded") $ Right <$> url
    dynText $ either ("No url: " <>) ("Url: " <>) <$> dUrl
    divClass "imagePreview" $ do
      flip (elDynAttr "img") blank
        $ fmap (Map.singleton "src" . either (const "") id) dUrl
    return dUrl
 where
  onlySingle :: (Reflex t) => Event t [a] -> Event t a
  onlySingle = fmapMaybe $ \case
    [x] -> Just x
    _   -> Nothing

