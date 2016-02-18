module Common where

import Types

import qualified Data.ID as ID
import           Reflex.Dom

type CurrentTest t
  = Dynamic t (Maybe (ID.WithID (Decorated Test)))

