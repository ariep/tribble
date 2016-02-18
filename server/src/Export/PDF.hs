module Export.PDF
  (
    export
  ) where

import           Imports
import qualified Export.LaTeX as LaTeX

import           Data.ByteString.Lazy (ByteString)
import qualified Text.Pandoc          as P
import qualified Text.Pandoc.Builder  as P
import qualified Text.Pandoc.Options  as P
import qualified Text.Pandoc.PDF      as P


export:: (MonadIO m) => P.Pandoc -> m (Either ByteString ByteString)
export = liftIO . P.makePDF "xelatex" P.writeLaTeX
  P.def
    {
      P.writerStandalone = True
    , P.writerTemplate   = LaTeX.template
    } 
