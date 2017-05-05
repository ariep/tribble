module Common where

import qualified Components
import Imports
import Types

import qualified Data.Change as Change
import qualified Data.ID     as ID
import           Data.Hashable (Hashable)
import           Data.Maybe    (catMaybes)
import qualified Data.Text   as Text
import           Data.Time     (getCurrentTime)
import           Reflex.Dom
import qualified Web.Channel              as Ch
import qualified Web.Channel.Client.Cache.ReadWrite as Cache

type CurrentTest t
  = Dynamic t (Maybe (ID.WithID (Decorated Test)))

-- type CurrentUser t
--   = Dynamic t (ID.WithID User)

type TestCache t
  = Cache.CacheIPP t (Components.IDMap (Decorated Test))

type QuestionCache t
  = Cache.CacheIPP t (Components.IDMap (Decorated Question))

filterDeleted :: [ID.WithID (Decorated a)] -> [ID.WithID (Decorated a)]
filterDeleted = filter $
  not . isDeleted . view (ID.object . authored . labelled)

delete :: forall m a. (MonadIO m, Change.Changing a)
  => Proxy a -> m (Change.Changes (Decorated a))
delete _ = do
  t <- liftIO getCurrentTime
  let c₁ = (mempty, mempty, Change.replace (Just t)) :: Change.Changes (Dates)
      c₂ = (c₁, mempty) :: Change.Changes (Dated a)
      c₃ = (mempty, (mempty, c₂)) :: Change.Changes (Decorated a)
  return c₃

maybes :: Iso' [a] [Maybe a]
maybes = iso (map Just) catMaybes

maybeNull :: Iso' (Maybe Text) Text
maybeNull = iso (maybe Text.empty id) $ \ t -> if Text.null t
  then Nothing else Just t

alwaysValid :: (MonadWidget t m)
  => m (Dynamic t a) -> m (Dynamic t (Either e a))
alwaysValid = fmap (fmap Right)

instance Hashable (ID.ID x)
