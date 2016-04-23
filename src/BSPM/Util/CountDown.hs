module BSPM.Util.CountDown
  ( CountDown ()
  , newCountDown
  , decCountDown
  , waitCountDown
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM

newtype CountDown = CountDown { _count :: TVar Int }

newCountDown :: Int -> IO CountDown
newCountDown c = do
  count <- newTVarIO c
  return CountDown
    { _count = count
    }

decCountDown :: CountDown -> IO ()
decCountDown cd = atomically $ do
  c <- readTVar $ _count cd
  when (c /= 0) $ writeTVar (_count cd) (c -1)

waitCountDown :: CountDown -> IO ()
waitCountDown cd = atomically $ do
  c <- readTVar $ _count cd
  when (c /= 0) retry
