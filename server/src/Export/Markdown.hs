module Export.Markdown
  ( export
  ) where

import           Common        (utf8ByteString)
import           Export.Common (Container, Result)
import           Imports

import           Data.ByteString.Lazy         (ByteString)
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.Options        as P


export :: (Monad m) =>
  Container -> P.Pandoc -> m Result
export _ = return . Right . utf8ByteString .
  P.writeMarkdown P.def
