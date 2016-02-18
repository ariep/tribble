module Export.Word
  (
    export
  ) where

import           Imports

import           Data.ByteString.Lazy         (ByteString)
import qualified Text.Pandoc                as P
import qualified Text.Pandoc.Options        as P


export:: (MonadIO m) => P.Pandoc -> m (Either ByteString ByteString)
export = fmap Right . liftIO . P.writeDocx P.def
