module Control.Concurrent.Notify
  ( Notify
  , new
  , subscribe
  , Listen
  , notify
  , wait
  ) where

import           Control.Concurrent
  ( forkIO
  , Chan
  , newChan
  , dupChan
  , writeChan
  , readChan
  )

newtype Notify
  = Notify (Chan ())

new:: IO Notify
new = Notify <$> newChan

subscribe :: Notify -> IO Listen
subscribe (Notify c) = fmap Listen $ dupChan c

newtype Listen
  = Listen (Chan ())

notify :: Notify -> IO ()
notify (Notify c) = writeChan c ()

wait :: Listen -> IO ()
wait (Listen c) = readChan c
