{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Types
  ( Question(..), question, answer, title
  , emptyQuestion
  , Answer(..), choices, order, _Open, _MultipleChoice
  , Choice, AnswerOrder
  , Title(..), titleText, generated
  , generateTitle
  , Test(..), name, elements
  , emptyTest
  , User(..), userName, extID
  , UserExtID(GoogleID), unregisteredExtID
  , UserName
  , Account(..)
  , Authored(..), author, authored, Author
  , ExportMode(..), Format(..)
  , QuestionFilter(..), labelFilter, searchText
  , emptyFilter
  , TestElement(..), _TestQuestion, _TestText
  , Html
  , RichText(..), plainText, plainRich, renderPlain, rtToHtml, htmlToRt
  , module Types.Dated
  , module Types.Labelled
  , Decorated
  , undecorated
  , TestChange(..)

  , Text
  ) where

import qualified Data.ID as ID
import           Types.Dated
import           Types.Labelled

import           Control.Lens  (makeLenses, makePrisms, Lens', Iso', iso, over)
import qualified Data.Change    as Change
import           Data.Monoid   ((<>))
import qualified Data.SafeCopy  as SC
import qualified Data.Serialize.Get as C
import qualified Data.Serialize.Put as C
import qualified Data.Set       as Set
import           Data.Text     (Text)
import qualified Data.Text      as Text
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

-- import qualified Text.Pandoc as Pandoc


data Question
  = Question
    { _question      :: RichText
    , _answer        :: Answer
    , _title         :: Title
    }
  deriving (Generic, Typeable, Show)

data Answer
  = Open RichText
  | MultipleChoice
    { _choices :: [Choice]
    , _order   :: AnswerOrder
    }
  deriving (Generic, Typeable, Show)

type Choice
  = (Bool, RichText)

type AnswerOrder
  = [Int]

data Title
  = Title
    { _titleText :: Text
    , _generated :: Bool
    }
    deriving (Generic, Typeable, Show)

generateTitle :: RichText -> Title
generateTitle = flip Title True . Text.take maxTitleLength . renderPlain
 where
  maxTitleLength = 120

data Test
  = Test
    { _name     :: Text
    , _elements :: [TestElement]
    }
  deriving (Generic, Typeable, Show)

data TestElement
  = TestQuestion (ID.ID (Decorated Question))
  | TestText RichText
  deriving (Generic, Typeable, Show)

type Decorated x
  = Authored (Labelled (Dated x))

-- Users and accounts

data User
  = User
    { _extID      :: UserExtID
    , _userName   :: UserName
    }
  deriving (Generic, Typeable, Show)
instance Eq User where
  u₁ == u₂ = _extID u₁ == _extID u₂
instance Ord User where
  compare u₁ u₂ = compare (_extID u₁) (_extID u₂)

data UserExtID
  = GoogleID Text
  deriving (Generic, Typeable, Show, Eq, Ord)

unregisteredExtID :: UserExtID
unregisteredExtID = GoogleID $ Text.pack ""

type UserName
  = Text

data Account
  = Account Text
  deriving (Generic, Typeable, Show)

data Authored x
  = Authored
    {
      _author   :: Author
    , _authored :: x
    }
  deriving (Generic, Typeable, Show)

type Author
  = ID.ID User

-- Exporting tests

data ExportMode
  = OnlyQuestions
  | WithAnswers
  deriving (Eq)

data Format
  = PDF
  | LaTeX
  | Markdown
  | Show
  | Word
  deriving (Eq, Show)

-- Searching

data QuestionFilter
  = QuestionFilter
    { _labelFilter :: Set.Set Label
    , _searchText  :: Maybe Text
    }
  deriving (Generic, Typeable, Show)

-- Empty objects

emptyQuestion :: Question
emptyQuestion = Question
  rt
  (Open rt)
  (generateTitle rt)
 where
  rt = plainRich Text.empty

emptyTest :: Test
emptyTest = Test Text.empty []

emptyFilter :: QuestionFilter
emptyFilter = QuestionFilter (Set.empty) Nothing

-- Rich text conversions

newtype RichText
  = RichText Html
  deriving (Monoid, Generic, Typeable, Show)

plainRich :: Text -> RichText
plainRich = RichText

renderPlain :: RichText -> Text
renderPlain (RichText s) = stripTags $ s
 where
  -- TODO: user a proper parser for this
  stripTags :: Text -> Text
  stripTags t = case Text.uncons t of
    Nothing          -> t
    Just ('<', rest) -> stripTags $ Text.drop 1 $ Text.dropWhile (/= '>') t
    Just (c  , rest) -> Text.cons c $ stripTags rest

plainText :: Iso' RichText Text
plainText = iso renderPlain plainRich

type Html
  = Text

rtToHtml :: RichText -> Html
rtToHtml (RichText h) = h

htmlToRt :: Html -> RichText
htmlToRt = RichText

-- Lenses

makeLenses ''Question
makeLenses ''Answer
makePrisms ''Answer
makeLenses ''Title
makeLenses ''Test
makePrisms ''TestElement
makeLenses ''QuestionFilter
makeLenses ''User
makeLenses ''Authored

undecorated :: Lens' (Decorated x) x
undecorated = authored . labelled . dated

-- Changing

instance Change.Changing User where
instance Change.Changing TestElement where
instance Change.Changing Question where

instance (Change.Changing x) => Change.Changing (Authored x) where
  type Changes (Authored x) =
    ( Change.Replace Author
    , Change.Changes x
    )
  apply (c₁, c₂) = over author (Change.replacing c₁) . over authored (Change.apply c₂)

instance (Change.Changing a) => Change.Changing (ID.WithID a) where
  type Changes (ID.WithID a) = Change.Changes a
  apply c = over ID.object $ Change.apply c

instance Change.Changing Test where
  type Changes Test = TestChange
  apply (TestElementsChange c) = over elements $ Change.apply c
  apply (WholeTestChange t)    = const t
data TestChange
  = TestElementsChange (Change.Changes [TestElement])
  | WholeTestChange    Test
instance Monoid TestChange where
  mempty = TestElementsChange mempty
  mappend   (WholeTestChange t)     _                       = WholeTestChange t
  mappend c@(TestElementsChange _)  (WholeTestChange t)     = WholeTestChange (Change.apply c t)
  mappend   (TestElementsChange c₁) (TestElementsChange c₂) =
    TestElementsChange (mappend c₁ c₂)


-- Serialisation

SC.deriveSafeCopy 0 'SC.base ''UserExtID
SC.deriveSafeCopy 0 'SC.base ''User
SC.deriveSafeCopy 0 'SC.base ''Account
SC.deriveSafeCopy 0 'SC.base ''Title
SC.deriveSafeCopy 0 'SC.base ''Question
SC.deriveSafeCopy 0 'SC.base ''Answer
SC.deriveSafeCopy 0 'SC.base ''Test
SC.deriveSafeCopy 0 'SC.base ''TestElement
SC.deriveSafeCopy 0 'SC.base ''ExportMode
SC.deriveSafeCopy 0 'SC.base ''Format
SC.deriveSafeCopy 0 'SC.base ''QuestionFilter
SC.deriveSafeCopy 0 'SC.base ''Dated
SC.deriveSafeCopy 0 'SC.base ''Labelled
SC.deriveSafeCopy 0 'SC.base ''Authored
SC.deriveSafeCopy 0 'SC.base ''Dates
SC.deriveSafeCopy 0 'SC.base ''ID.ID
SC.deriveSafeCopy 0 'SC.base ''ID.WithID
SC.deriveSafeCopy 1 'SC.base ''RichText

SC.deriveSafeCopy 0 'SC.base ''TestChange
SC.deriveSafeCopy 0 'SC.base ''Change.Replace
instance (SC.SafeCopy a, SC.SafeCopy (Change.Changes a)) =>
  SC.SafeCopy (Change.MaybeChange a) where
  putCopy Change.SetNothing = SC.contain $ do
    C.putWord8 0
    return ()
  putCopy (Change.SetJust a) = SC.contain $ do
    C.putWord8 1
    put_a <- SC.getSafePut
    put_a a
    return ()
  putCopy (Change.MapMaybe c) = SC.contain $ do
    C.putWord8 2
    put_c <- SC.getSafePut
    put_c c
    return ()
  getCopy = SC.contain $ do
    tag <- C.getWord8
    case tag of
      0 -> return Change.SetNothing
      1 -> do
        get_a <- SC.getSafeGet
        return Change.SetJust <*> get_a
      2 -> do
        get_c <- SC.getSafeGet
        return Change.MapMaybe <*> get_c
  version = 0
  kind = SC.base

SC.deriveSafeCopy 0 'SC.base ''Change.SetChange

instance (SC.SafeCopy k, SC.SafeCopy v, SC.SafeCopy (Change.Changes v)) =>
  SC.SafeCopy (Change.MapChange k v) where
  putCopy (Change.MapAdd k v) = SC.contain $ do
    C.putWord8 0
    put_k <- SC.getSafePut
    put_v <- SC.getSafePut
    put_k k
    put_v v
    return ()
  putCopy (Change.MapEnsure k v) = SC.contain $ do
    C.putWord8 1
    put_k <- SC.getSafePut
    put_v <- SC.getSafePut
    put_k k
    put_v v
    return ()
  putCopy (Change.MapModify k c) = SC.contain $ do
    C.putWord8 2
    put_k <- SC.getSafePut
    put_c <- SC.getSafePut
    put_k k
    put_c c
    return ()
  putCopy (Change.MapDelete k) = SC.contain $ do
    C.putWord8 3
    put_k <- SC.getSafePut
    put_k k
    return ()
  getCopy = SC.contain $ do
    tag <- C.getWord8
    case tag of
      0 -> do
        get_k <- SC.getSafeGet
        get_v <- SC.getSafeGet
        return Change.MapAdd <*> get_k <*> get_v
      1 -> do
        get_k <- SC.getSafeGet
        get_v <- SC.getSafeGet
        return Change.MapEnsure <*> get_k <*> get_v
      2 -> do
        get_k <- SC.getSafeGet
        get_c <- SC.getSafeGet
        return Change.MapModify <*> get_k <*> get_c
      3 -> do
        get_k <- SC.getSafeGet
        return Change.MapDelete <*> get_k
  version = 0
  kind = SC.base

instance (SC.SafeCopy a, SC.SafeCopy (Change.Changes a)) =>
  SC.SafeCopy (Change.ListChange a) where
  putCopy (Change.ListAdd i a) = SC.contain $ do
    C.putWord8 0
    put_i <- SC.getSafePut
    put_a <- SC.getSafePut
    put_i i
    put_a a
    return ()
  putCopy (Change.ListDelete i) = SC.contain $ do
    C.putWord8 1
    put_i <- SC.getSafePut
    put_i i
    return ()
  putCopy (Change.ListChange i c) = SC.contain $ do
    C.putWord8 2
    put_i <- SC.getSafePut
    put_c <- SC.getSafePut
    put_i i
    put_c c
    return ()
  putCopy (Change.ListReorder i j) = SC.contain $ do
    C.putWord8 3
    put_i <- SC.getSafePut
    put_j <- SC.getSafePut
    put_i i
    put_j j
    return ()
  getCopy = SC.contain $ do
    tag <- C.getWord8
    case tag of
      0 -> do
        get_i <- SC.getSafeGet
        get_a <- SC.getSafeGet
        return Change.ListAdd <*> get_i <*> get_a
      1 -> do
        get_i <- SC.getSafeGet
        return Change.ListDelete <*> get_i
      2 -> do
        get_i <- SC.getSafeGet
        get_c <- SC.getSafeGet
        return Change.ListChange <*> get_i <*> get_c
      3 -> do
        get_i <- SC.getSafeGet
        get_j <- SC.getSafeGet
        return Change.ListReorder <*> get_i <*> get_j
  version = 0
  kind = SC.base
