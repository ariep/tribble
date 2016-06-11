module Export.Word
  (
    export
  ) where

import           Export.Common (Container, contain, Result)
import           Imports

import           Data.ByteString.Lazy         (ByteString)
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.Options        as P


export :: (MonadIO m) =>
  Container -> P.Pandoc -> m Result
export c = fmap Right . liftIO . contain c . P.writeDocx P.def
