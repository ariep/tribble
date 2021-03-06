{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Types.Dated
  (
    Dated(..), dates, dated
  , Dates(..), creationDate, modificationDate, deletionDate
  , date
  , isDeleted
  , deleteDated
  , undeleteDated
  , updateDated
  , getCurrentTime
  ) where

import           Control.Lens           (view, over)
import           Control.Lens.TH        (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Change as Change
import           Data.Maybe             (isJust)
import           Data.Time              (UTCTime, getCurrentTime)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)


data Dated x
  = Dated
    {
      _dates :: Dates
    , _dated :: x
    }
  deriving (Generic, Typeable, Show)

data Dates
  = Dates
    {
      _creationDate     :: UTCTime
    , _modificationDate :: UTCTime
    , _deletionDate     :: Maybe UTCTime
    }
  deriving (Generic, Typeable, Show)

makeLenses ''Dated
makeLenses ''Dates

date :: (MonadIO m) => x -> m (Dated x)
date x = do
  t <- liftIO getCurrentTime
  let d = Dates t t Nothing
  return $ Dated d x

isDeleted :: Dated x -> Bool
isDeleted = isJust . view (dates . deletionDate)

deleteDated :: UTCTime -> Dated x -> Dated x
deleteDated t = over (dates . deletionDate) $ maybe (Just t) Just

undeleteDated :: Dated x -> Dated x
undeleteDated = over (dates . deletionDate) $ const Nothing

updateDated :: UTCTime -> Dated x -> Dated x
updateDated = over (dates . modificationDate) . const

-- instance Changing UTCTime where
--   apply = replacing

instance Change.Changing Dates where
  type Changes Dates =
    ( Change.Replace UTCTime
    , Change.Replace UTCTime
    , Change.Replace (Maybe UTCTime)
    )
  apply (c₁, c₂, c₃) =
      over creationDate     (Change.replacing c₁)
    . over modificationDate (Change.replacing c₂)
    . over deletionDate     (Change.replacing c₃)

instance (Change.Changing x) => Change.Changing (Dated x) where
  type Changes (Dated x) = (Change.Changes Dates, Change.Changes x)
  apply (c₁, c₂) = over dates (Change.apply c₁) . over dated (Change.apply c₂)
