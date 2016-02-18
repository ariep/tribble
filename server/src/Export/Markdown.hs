module Export.Markdown
  (
    export
  ) where

import           Common  (utf8ByteString)
import           Imports

import           Data.ByteString.Lazy         (ByteString)
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.Options        as P


export:: (Monad m) => P.Pandoc -> m (Either ByteString ByteString)
export = return . Right . utf8ByteString . P.writeMarkdown P.def
