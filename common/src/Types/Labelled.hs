{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Types.Labelled
  (
    Labelled(..), labels, labelled
  , Label
  ) where

import           Control.Lens    (over)
import           Control.Lens.TH (makeLenses)
import           Data.Change     (Changing(Changes, apply))
import qualified Data.Set     as Set
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)


data Labelled x
  = Labelled
    {
      _labels   :: Set.Set Label
    , _labelled :: x
    }
  deriving (Generic, Typeable, Show)

type Label
  = Text

makeLenses ''Labelled

instance (Changing x) => Changing (Labelled x) where
  type Changes (Labelled x) =
    ( Changes (Set.Set Label)
    , Changes x
    )
  apply (c₁, c₂)  = over labels (apply c₁) . over labelled (apply c₂)
