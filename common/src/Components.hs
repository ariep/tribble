{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Components where

import Types

import Web.Channel (Channel(Channel), File, Token(Token), CRUD)

import Control.Coroutine.Monadic (Session, Eps, (:!:), (:?:), (:&:), Repeat)
import qualified Data.ID     as ID

import Data.ByteString.Lazy (ByteString)
import qualified Data.Text   as Text
import qualified Network.URI as URI

type AllTests
  = Repeat ([ID.WithID (Decorated Test)] :!: Eps)

allTests :: Channel AllTests
allTests = Channel [0]

crudTests :: Token (CRUD (Decorated Test))
crudTests = Token [1]

crudQuestions  :: Token (CRUD (Decorated Question))
crudQuestions  = Token [2]


type UploadImage
  = Repeat (File :?: (String :!: Eps))

uploadImage    :: Channel UploadImage
uploadImage    = Channel [3]

type CurrentUser
  = ID.WithID User :!: Eps

currentUser    :: Channel CurrentUser
currentUser    = Channel [4]

type ListAccounts
  = Repeat ([ID.WithID Account] :!: Eps)

listAccounts :: Channel ListAccounts
listAccounts = Channel [5]

type SetCurrentAccount
  = Repeat (ID.WithID Account :?: Eps)

setCurrentAccount :: Channel SetCurrentAccount
setCurrentAccount = Channel [6]

type GetCurrentAccount
  = Repeat (ID.WithID Account :!: Eps)

getCurrentAccount :: Channel GetCurrentAccount
getCurrentAccount = Channel [7]

type ExportTest
  = Repeat (
    (ID.ID (Decorated Test), ExportMode, Format) :?:
    Either Text String :!:
    Eps)

exportTest    :: Channel ExportTest
exportTest    = Channel [8]

-- The rational should be a number between 0 and 1, with higher numbers
-- denoting better matches.
type FilterQuestions
  = Repeat (QuestionFilter :?:
    [(ID.ID (Decorated Question), Rational)] :!:
    Eps)

filterQuestions :: Channel FilterQuestions
filterQuestions = Channel [9]

type QuestionLabels
  = Repeat (Maybe Text :?: [Label] :!: Eps)

questionLabels :: Channel QuestionLabels
questionLabels = Channel [10]

-- Helper types and functions.

data ServedFileName
  = ServedFileName
    { localPrefix :: FilePath
    , visiblepath :: String
    }

localFileName :: ServedFileName -> FilePath
localFileName s = localPrefix s ++ visiblepath s

-- remoteUrl :: ServedFileName -> URI.URI -> URI.URI
-- remoteUrl s domain = domain { URI.uriPath = visiblepath s }
remoteUrl :: ServedFileName -> String
remoteUrl s = visiblepath s

downloadFile :: String -> ServedFileName
downloadFile = ServedFileName "./runtime-data/download"

uploadedFile :: String -> ServedFileName
uploadedFile = ServedFileName "./runtime-data/upload"

exportedTest :: ID.ID (Decorated Test) -> ExportMode -> Format ->
  ServedFileName
exportedTest (ID.ID i) mode format = downloadFile $
  "/export/" ++ Text.unpack i ++ "-" ++ modeString mode ++ extension format

modeString mode = case mode of
  OnlyQuestions -> "Toets"
  WithAnswers   -> "Antwoorden"
extension format = case format of
  PDF       -> ".pdf"
  LaTeX     -> ".latex"
  Markdown  -> ".md"
  Show      -> ".text"
  Word      -> ".docx"
  -- Unknown s -> ""

