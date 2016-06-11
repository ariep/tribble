module Export
  ( export
  ) where

import           Common
import           Components
import           Export.Common (Container(Container), Result)
import qualified Export.LaTeX    as LaTeX
import qualified Export.Markdown as Markdown
import qualified Export.Pandoc   as Pandoc
import qualified Export.PDF      as PDF
import qualified Export.Word     as Word
import qualified DB
import           Imports
import           Types

import qualified Data.ByteString.Lazy       as B
import qualified Data.TCache.ID             as ID
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           System.Directory        (withCurrentDirectory)


export :: ExportMode -> Format -> ID.ID (Decorated Test) ->
  DB.DB (Either Text ServedFileName)
export mode format testID = do
  ID.WithID itest dtest <- ID.lookup testID
  let test = view undecorated dtest
  Pandoc.build mode dtest >>= convert >>= \case
    Left e  -> return . Left . Text.decodeUtf8 . B.toStrict $ e
    Right b -> do
      let f = exportedTest itest mode format
      liftIO $ B.writeFile (localFileName f) b
      return $ Right f
 where
  convert :: Pandoc.Pandoc  -> DB.DB Result
  convert = ($ container) $
    case format of
      PDF      -> PDF.export
      LaTeX    -> LaTeX.export
      Markdown -> Markdown.export
      Show     -> Pandoc.export
      Word     -> Word.export
  container :: Container
  container = Container (withCurrentDirectory "./runtime-data/export-wd")

suggestedFileName :: Test -> ExportMode -> Format -> String
suggestedFileName test mode format = prefix ++ title ++ extension format where
  prefix = modeString mode ++ "."
  title = Text.unpack $ view name test

-- readFormat :: String -> Format
-- readFormat "pdf"      = PDF
-- readFormat "latex"    = LaTeX
-- readFormat "markdown" = Markdown
-- readFormat "show"     = Show
-- readFormat "word"     = Word
-- readFormat s          = Unknown s
