module Cache where

import qualified Web.Client.Channel.Cache as Cache


data Cache
  = Cache
    { qs :: IDCache (Decorated Question)
    }

type IDCache x = Cache.Cache (ID.ID x) (ID.WithID x)

questions :: Cache.Query 

initialise :: IO Cache
initialise = do
  
