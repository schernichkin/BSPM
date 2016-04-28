module BSPM.Util.CriticalSection
  ( CriticalSection ()
  , newCriticalSection
  , modifyCriticalSection
  , withCriticalSection
  ) where

import Control.Concurrent.STM.TMVar
import Control.Exception
import Control.Monad.STM

newtype CriticalSection a = CriticalSection { unCriticalSection :: TMVar a }

{-# INLINE newCriticalSection #-}
newCriticalSection :: a -> IO (CriticalSection a)
newCriticalSection a = CriticalSection <$> newTMVarIO a

{-# INLINE modifyCriticalSection #-}
modifyCriticalSection :: CriticalSection a -> (a -> IO (a, b)) -> IO b
modifyCriticalSection section action = bracketOnError acquire release $ \x -> do
  (a, b) <- action x
  release a
  return b
  where
    acquire = atomically $ takeTMVar $ unCriticalSection section
    release = atomically . putTMVar (unCriticalSection section)

{-# INLINE withCriticalSection #-}
withCriticalSection :: CriticalSection a -> (a -> IO b) -> IO b
withCriticalSection section action = modifyCriticalSection section $ \x -> do
  b <- action x
  return (x, b)
