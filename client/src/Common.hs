module Common where

import qualified Components
import Imports
import Types

import qualified Data.ID   as ID
import           Data.Hashable (Hashable)
import           Data.Maybe    (catMaybes)
import qualified Data.Text as Text
import           Reflex.Dom
import qualified Web.Channel              as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache

type CurrentTest t
  = Dynamic t (Maybe (ID.WithID (Decorated Test)))

type CurrentUser t
  = Dynamic t (ID.WithID User)

type Tests t
  = Dynamic t [ID.WithID (Decorated Test)]

-- type TestCache t m
--   = Cache.Cache t m
--     (ID.ID (Decorated Test))
--     (ID.WithID (Decorated Test))

type QuestionCache t m
  = Cache.Cache t m
    (ID.ID (Decorated Question))
    (ID.WithID (Decorated Question))

filterDeleted :: [ID.WithID (Decorated a)] -> [ID.WithID (Decorated a)]
filterDeleted = filter $
  not . isDeleted . view (ID.object . authored . labelled)

maybes :: Iso' [a] [Maybe a]
maybes = iso (map Just) catMaybes

maybeNull :: Iso' (Maybe Text) Text
maybeNull = iso (maybe Text.empty id) $ \ t -> if Text.null t
  then Nothing else Just t

alwaysValid :: (MonadWidget t m)
  => m (Dynamic t a) -> m (Dynamic t (Either e a))
alwaysValid = (mapDyn Right =<<)

instance Hashable (ID.ID x)
