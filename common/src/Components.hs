{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Components where

import Types

import Web.Channel (Channel(Channel), File, Token(Token), InitialPushPull)

import           Control.Coroutine.Monadic (Session, Eps, (:!:), (:?:), (:&:), Repeat)
import           Control.Lens         (over)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Change as Change
import qualified Data.ID     as ID
import qualified Data.Map    as Map
import           Data.Monoid ((<>))
import qualified Data.Set    as Set
import qualified Data.Text   as Text
import           Data.Text   (Text)
import qualified Network.URI as URI


type LoginLink
  = Text :!: Eps

loginLink :: Channel LoginLink
loginLink = Channel [0]

type IDMap a
  = Map.Map (ID.ID a) (ID.WithID a)

idMap :: [ID.WithID a] -> IDMap a
idMap = Map.fromList . map (\ w -> (ID.__ID w,w))

ippTests :: Token (InitialPushPull (IDMap (Decorated Test)))
ippTests = Token [1]

type NewTest
  = Repeat (Decorated Test :?: ID.ID (Decorated Test) :!: Eps)

newTest :: Channel NewTest
newTest = Channel [2]

ippQuestions :: Token (InitialPushPull (IDMap (Decorated Question)))
ippQuestions  = Token [3]

type NewQuestion
  = Repeat (Decorated Question :?: ID.ID (Decorated Question) :!: Eps)

newQuestion :: Channel NewQuestion
newQuestion = Channel [4]

type UploadImage
  = Repeat (File :?: (Text :!: Eps))

uploadImage    :: Channel UploadImage
uploadImage    = Channel [5]

type CurrentUser
  = ID.WithID User :!: Eps

currentUser    :: Channel CurrentUser
currentUser    = Channel [6]

type ListAccounts
  = Repeat ([ID.WithID Account] :!: Eps)

listAccounts :: Channel ListAccounts
listAccounts = Channel [7]

type SetCurrentAccount
  = Repeat (ID.WithID Account :?: Eps)

setCurrentAccount :: Channel SetCurrentAccount
setCurrentAccount = Channel [8]

type GetCurrentAccount
  = Repeat (ID.WithID Account :!: Eps)

getCurrentAccount :: Channel GetCurrentAccount
getCurrentAccount = Channel [9]

type ExportTest
  = Repeat (
    (ID.ID (Decorated Test), ExportMode, Format) :?:
    Either Text Text :!:
    Eps)

exportTest    :: Channel ExportTest
exportTest    = Channel [10]

-- The rational should be a number between 0 and 1, with higher numbers
-- denoting better matches.
type FilterQuestions
  = Repeat (QuestionFilter :?:
    [(ID.ID (Decorated Question), Rational)] :!:
    Eps)

filterQuestions :: Channel FilterQuestions
filterQuestions = Channel [11]

type QuestionLabels
  = Repeat (Maybe Text :?: [Label] :!: Eps)

questionLabels :: Channel QuestionLabels
questionLabels = Channel [12]

type EditingQuestions
  = Map.Map (ID.ID (Decorated Question)) (Set.Set (ID.WithID User))

-- Keep track of which users are editing which questions.

type InitialEditing
  = Repeat (EditingQuestions :!: Eps)

type PullEditing
  = Repeat (Change.Changes EditingQuestions :!: Eps)

type PushEditing
  = Repeat ((ID.ID (Decorated Question), Bool) :?: Eps)

initialEditing :: Channel InitialEditing
initialEditing = Channel $ [13] ++ [0]

pullEditing :: Channel PullEditing
pullEditing = Channel $ [13] ++ [1]

pushEditing :: Channel PushEditing
pushEditing = Channel $ [13] ++ [2]

-- Helper types and functions.

data ServedFileName
  = ServedFileName
    { localPrefix :: FilePath
    , visiblePath :: Text
    }

localFileName :: ServedFileName -> FilePath
localFileName s = localPrefix s <> Text.unpack (visiblePath s)

-- remoteUrl :: ServedFileName -> URI.URI -> URI.URI
-- remoteUrl s domain = domain { URI.uriPath = visiblepath s }
remoteUrl :: ServedFileName -> Text
remoteUrl s = visiblePath s

downloadFile :: Text -> ServedFileName
downloadFile = ServedFileName "./runtime-data/download"

uploadedFile :: Text -> ServedFileName
uploadedFile = ServedFileName "./runtime-data/upload"

exportedTest :: ID.ID (Decorated Test) -> ExportMode -> Format ->
  ServedFileName
exportedTest (ID.ID i) mode format = downloadFile $
  "/export/" <> i <> "-" <> modeString mode <> extension format

modeString :: ExportMode -> Text
modeString mode = case mode of
  OnlyQuestions -> "Toets"
  WithAnswers   -> "Antwoorden"
extension format = case format of
  PDF       -> ".pdf"
  LaTeX     -> ".latex"
  Markdown  -> ".md"
  Show      -> ".text"
  Word      -> ".docx"

