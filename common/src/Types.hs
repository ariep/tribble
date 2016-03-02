{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Types
  ( Question(..), question, answer, title
  , emptyQuestion
  , Answer(..), choices, order, _Open, _MultipleChoice
  , Choice, AnswerOrder
  , Title(..), titleText, generated
  , generateTitle
  , Test(..), name, elements
  , emptyTest
  , ExportMode(..), Format(..)
  , QuestionFilter(..), labelFilter, searchText
  , emptyFilter
  , TestElement(..), _TestQuestion, _TestText
  , RichText(..), plainText, plainRich, renderPlain, rtToHtml, htmlToRt
  , module Types.Dated
  , module Types.Labelled
  , Decorated
  , undecorated

  , Text
  ) where

import qualified Data.ID as ID
import           Types.Dated
import           Types.Labelled

import           Control.Lens (makeLenses, makePrisms, Lens', Iso', iso)
import qualified Data.SafeCopy  as SC
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import qualified Text.Pandoc    as Pandoc


data Question
  = Question
    {
      _question      :: RichText
    , _answer        :: Answer
    , _title         :: Title
    }
  deriving (Generic, Typeable, Show)

data Answer
  = Open RichText
  | MultipleChoice
    {
      _choices :: [Choice]
    , _order   :: AnswerOrder
    }
  deriving (Generic, Typeable, Show)

type Choice
  = (Bool, RichText)

type AnswerOrder
  = [Int]

data Title
  = Title
    {
      _titleText :: Text
    , _generated :: Bool
    }
    deriving (Generic, Typeable, Show)

generateTitle :: RichText -> Title
generateTitle = flip Title True . Text.take maxTitleLength . renderPlain
 where
  maxTitleLength = 120

data Test
  = Test
    {
      _name     :: Text
    , _elements :: [TestElement]
    }
  deriving (Generic, Typeable, Show)

data TestElement
  = TestQuestion (ID.ID (Decorated Question))
  | TestText RichText
  deriving (Generic, Typeable, Show)

type Decorated x
  = Labelled (Dated x)

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
  = Pandoc [Pandoc.Block]
  deriving (Monoid, Generic, Typeable, Show)

plainRich :: Text -> RichText
plainRich = Pandoc . (: []) . Pandoc.Plain . (: []) . Pandoc.Str . Text.unpack

renderPlain :: RichText -> Text
renderPlain (Pandoc ps) = Text.pack $ Pandoc.writePlain Pandoc.def $
  Pandoc.Pandoc Pandoc.nullMeta ps

plainText :: Iso' RichText Text
plainText = iso renderPlain plainRich

type Html
  = String

rtToHtml :: RichText -> Html
rtToHtml (Pandoc p) = Pandoc.writeHtmlString
  Pandoc.def
  (Pandoc.Pandoc Pandoc.nullMeta p)

-- TODO: do something more sensible with parse errors here.
htmlToRt :: Html -> RichText
htmlToRt = either (plainRich . Text.pack . show) r .
  Pandoc.readHtml Pandoc.def
    -- Gives a runtime error in the browser!
    -- (Pandoc.def
    --   { Pandoc.readerSmart = True
    --   }
    -- :: Pandoc.ReaderOptions)
 where
  r (Pandoc.Pandoc _ p) = Pandoc p

-- Lenses

makeLenses ''Question
makeLenses ''Answer
makePrisms ''Answer
makeLenses ''Title
makeLenses ''Test
makePrisms ''TestElement
makeLenses ''QuestionFilter

undecorated :: Lens' (Decorated x) x
undecorated = labelled . dated

-- Serialisation

SC.deriveSafeCopy 0 'SC.base ''Pandoc.Format
SC.deriveSafeCopy 0 'SC.base ''Pandoc.Citation
SC.deriveSafeCopy 0 'SC.base ''Pandoc.CitationMode
SC.deriveSafeCopy 0 'SC.base ''Pandoc.Alignment
SC.deriveSafeCopy 0 'SC.base ''Pandoc.MathType
SC.deriveSafeCopy 0 'SC.base ''Pandoc.QuoteType
SC.deriveSafeCopy 0 'SC.base ''Pandoc.Inline
SC.deriveSafeCopy 0 'SC.base ''Pandoc.ListNumberDelim
SC.deriveSafeCopy 0 'SC.base ''Pandoc.ListNumberStyle
SC.deriveSafeCopy 0 'SC.base ''Pandoc.Block
SC.deriveSafeCopy 0 'SC.base ''RichText
SC.deriveSafeCopy 0 'SC.base ''Title
SC.deriveSafeCopy 1 'SC.extension ''Question
SC.deriveSafeCopy 0 'SC.base ''Answer
SC.deriveSafeCopy 0 'SC.base ''Test
SC.deriveSafeCopy 0 'SC.base ''TestElement
SC.deriveSafeCopy 0 'SC.base ''ExportMode
SC.deriveSafeCopy 0 'SC.base ''Format
SC.deriveSafeCopy 0 'SC.base ''QuestionFilter
SC.deriveSafeCopy 0 'SC.base ''Dated
SC.deriveSafeCopy 0 'SC.base ''Labelled
SC.deriveSafeCopy 0 'SC.base ''Dates
SC.deriveSafeCopy 0 'SC.base ''ID.ID
SC.deriveSafeCopy 0 'SC.base ''ID.WithID

data Question_v0
  = Question_v0
    {
      _question_v0      :: RichText
    , _answer_v0        :: Answer
    }
  deriving (Generic, Typeable, Show)
SC.deriveSafeCopy 0 'SC.base ''Question_v0
instance SC.Migrate Question where
  type MigrateFrom Question = Question_v0
  migrate (Question_v0 { .. }) = Question
    { _question = _question_v0
    , _answer   = _answer_v0
    , _title    = generateTitle _question_v0
    }
