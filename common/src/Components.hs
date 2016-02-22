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

crudTests :: Token (CRUD (Decorated Test))
crudTests = Token [1]

crudQuestions :: Token (CRUD (Decorated Question))
crudQuestions = Token [2]


type UploadImage
  = Repeat (File :?: (String :!: Eps))

uploadImage   :: Channel UploadImage
uploadImage   = Channel [3]

type ShowUser
  = Text :!: Eps

showUser      :: Channel ShowUser
showUser      = Channel [4]

type ExportTest
  = Repeat (
    (ID.ID (Decorated Test), ExportMode, Format) :?:
    Either Text String :!:
    Eps)

exportTest    :: Channel ExportTest
exportTest    = Channel [5]

type FilterQuestions
  = Repeat (Text :?: [ID.ID (Decorated Question)] :!: Eps)

filterQuestions :: Channel FilterQuestions
filterQuestions = Channel [6]


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
downloadFile = ServedFileName "./download"

uploadedFile :: String -> ServedFileName
uploadedFile = ServedFileName "./upload"

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

