module OldData where

import Types
import Types.Dated    (Dated(..))
import Types.Labelled (Labelled(..))

import Data.ID            (ID(..), WithID(..))
import Data.Set           (fromList)
import Text.Pandoc hiding (Pandoc(Pandoc))

tests :: [WithID (Labelled (Dated Test))]
tests = [] 

questions :: [WithID (Labelled (Dated Question))]
questions = []
