module Label where

import Common
import Components
import Imports
import Types

import qualified Web.Channel        as Ch
import qualified Web.Channel.Client as Ch
import           Web.Widgets

import           Control.Lens            (traverse, toListOf, preview, review)
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
import           GHCJS.DOM.Element       (mouseDown)
import           GHCJS.DOM.EventM        (on, preventDefault)
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common

editLabel :: (MonadWidget t m) => Bool -> Edit t m (Maybe Label)
-- editLabel autofocus = editLens maybeNull $ editTextAutofocus autofocus
editLabel autofocus i = do
  eitherD <- toggleModesDyn
    (maybe (Left ()) (Right . Just) i) (Left i) editMode viewMode
  mapDyn (either id id) eitherD
 where
  editMode () = do
    (d, ti) <- editTextAutofocus' autofocus $ maybe Text.empty id i
    let active = _textInput_hasFocus ti
    let kp = _textInput_keypress ti
        enter = ffilter (== 13) kp
        up    = ffilter (== 38) kp
        down  = ffilter (== 40) kp
    autocompletion <- autocomplete active d
    autoLabel <- holdDyn Nothing (Just <$> autocompletion)
    manualLabel <- mapDyn Just d
    let finished = leftmost
          [ tagDyn autoLabel   autocompletion
          , tagDyn manualLabel enter
          ]
    newLabel <- holdDyn Nothing finished
    return (finished, newLabel)
  viewMode ml = do
    maybe blank showLabel ml
    return (never, constDyn ml)

showLabel :: (MonadWidget t m) => Label -> m ()
showLabel l = elClass "span" "label label-default" $ text $ Text.unpack l 

autocomplete :: (MonadWidget t m) =>
  Dynamic t Bool -> Dynamic t Text -> C t m (Event t Text)
autocomplete parentActive input = mdo
  suggestions <- Ch.sendManyReceipt questionLabels (Just <$> updated input)
  afterBuildAsync (Ch.sendOnce_ questionLabels Nothing Nothing :: C t IO ())
  list <- holdDyn [] suggestions
  -- active <- focussed u
  -- showD <- combineDyn (||) active parentActive
  attrs <- mapDyn (("class" =:) . (++ "typeahead dropdown-menu ") .
    bool "hidden " "show ") parentActive
  (u, e) <- elDynAttr' "ul" attrs $
    fmap switchPromptlyDyn . (mapDyn leftmost =<<) $ simpleList list $
      flip forDynEvent $ \ x -> mdo
        attrs <- mapDyn (("class" =:) . bool "" "active") =<< hovered li
        (li, l) <- elDynAttr' "li" attrs . link . Text.unpack $ x
        (consumeEvent =<<) . wrapDomEvent (_el_element li) (`on` mouseDown) $
          preventDefault
        return . (x <$) . _link_clicked $ l
  return e
