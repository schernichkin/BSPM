module BSP.Util.Barrier
  ( Barrier
  , newBarrier
  , passBarrier
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM

newtype Barrier = Barrier (TVar Int)

newBarrier :: Int -> IO Barrier
newBarrier = fmap Barrier . newTVarIO

passBarrier :: Barrier -> IO ()
passBarrier (Barrier barrierVar) = do
  passed <- atomically $ do
    waitCount <- pred <$> readTVar barrierVar
    when (waitCount >= 0) $ writeTVar barrierVar waitCount
    return $ waitCount == 0
  unless passed $ atomically $ do
    waitCount <- readTVar barrierVar
    when (waitCount >= 0) retry
