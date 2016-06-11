module Export.Common
  ( Container(Container, contain)
  , Result
  ) where

import           Data.ByteString.Lazy (ByteString)

newtype Container
  = Container
    { contain :: forall a. IO a -> IO a }

type Result
  = Either ByteString ByteString
