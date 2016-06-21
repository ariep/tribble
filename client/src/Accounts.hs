module Accounts where

import           Common
import qualified Components
import           Types

import           Control.Lens            (view)
import           Control.Monad.IO.Class  (MonadIO)
import qualified Data.ID                  as ID
import qualified Data.Text                as Text
import           Reflex.Dom
import qualified Web.Channel.Client as Ch
import           Web.Widgets


pickAccount :: forall t m. (MonadWidget t m, MonadIO (PushM t)) =>
  Dynamic t [ID.WithID Account] -> C t m (Event t (ID.WithID Account))
pickAccount accounts = fmap switchPromptlyDyn . mapDyn leftmost =<<
  simpleList accounts accountChoice
 where
  accountChoice :: Dynamic t (ID.WithID Account)
    -> C t m (Event t (ID.WithID Account))
  accountChoice dAccount = forDynEvent dAccount $
    \ a -> divClass "buttonRow" $ do
      pickE <- buttonClass "btn btn-default fill" $
        ((\ (Account t) -> Text.unpack t) . view ID.object) a
      return $ a <$ pickE
