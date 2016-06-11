{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.ID                  as ID
import           Data.List               (find, findIndex, splitAt)
import qualified Data.Map                 as Map
import           Data.Maybe              (mapMaybe)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified JavaScript.WebSockets.Reflex.WebSocket as WS
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common


questionForm :: forall t m. (MonadWidget t m, MonadIO (PushM t))
  => Edit t m (Decorated Question)
questionForm dq = do
  let q = view undecorated dq
  question <- divClass "form-group" $ do
    -- el "label" $ text "Vraag:"
    mapDyn htmlToRt =<< editRichText (rtToHtml $ view question q)
  answer <- divClass "form-group multipleChoiceAnswers" $ tabs
    [ Tab "Open" (is _Open $ view answer q) $ do
      -- el "label" $ text "Antwoord:"
      mapDyn (Open . htmlToRt) =<< editRichText
        (maybe "" rtToHtml $ preview (answer . _Open) q)
    , Tab "Meerkeuze" (is _MultipleChoice $ view answer q) $ do
      -- el "label" $ text "Antwoorden:"
      mapDyn (review _MultipleChoice) =<< editMultipleChoice
        (maybe (map emptyChoice [True, False], [1, 2]) id $
          preview (answer . _MultipleChoice) q)
    ]
  newLabels <- divClass "form-group" $ do
    el "label" $ text "Labels:"
    editLens (setIso . maybes) (editMulti True editLabel Nothing) $
      view (authored . labels) dq
  title <- mapDyn generateTitle question
  newQ <- return (constDyn Question)
    >>= combineDyn (flip ($)) question
    >>= combineDyn (flip ($)) answer
    >>= combineDyn (flip ($)) title
  dq' <- combineDyn
    (\ q labels_ -> set (authored . labels) labels_ . set undecorated q $ dq)
    newQ newLabels
  return dq'
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
        attrs <- mapDyn (Map.singleton "class" .
          ("btn btn-default btn-xs mc-button " ++) .
          bool "mc-false" "mc-true") correct
        click <- buttonDynAttr attrs $ dynText =<<
          mapDyn (bool "✗" "✓") correct
        return correct
      (dString, e) <- editTextAutofocus' autofocus $ renderPlain iText
      combineDyn (,) correct =<< mapDyn plainRich dString

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
    dynText =<< mapDyn (either ("No url: " ++) ("Url: " ++)) dUrl
    divClass "imagePreview" $ do
      flip (elDynAttr "img") blank
        =<< mapDyn (Map.singleton "src" . either (const "") id) dUrl  
    return dUrl
 where
  onlySingle :: (Reflex t) => Event t [a] -> Event t a
  onlySingle = fmapMaybe $ \case
    [x] -> Just x
    _   -> Nothing

