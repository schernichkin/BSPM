module BSPM.Util.RunOnceSet
  ( RunOnceSet ()
  , newRunOnce
  , getRunOnce
  ) where

import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Hashable
import Data.HashTable.IO as H

data RunOnceSet m k v = RunOnceSet
  { _table :: !(TMVar (BasicHashTable k v))
  , _initializer :: !(k -> m v)
  }

newRunOnce :: ( MonadIO m ) => (k -> m v) -> IO (RunOnceSet m k v)
newRunOnce initializer = do
  table <- new >>= newTMVarIO
  return RunOnceSet
    { _table = table
    , _initializer = initializer
    }

{-# INLINE getRunOnce #-}
getRunOnce :: ( MonadIO m, Eq k, Hashable k ) => RunOnceSet m k v -> k -> m v
getRunOnce once key = do --TODO: mask async exceptions, protect with try-finally
  table <- liftIO $ atomically $ takeTMVar (_table once)
  maybeValue <- liftIO $ H.lookup table key
  value <- case maybeValue of
    Just v -> return v
    Nothing -> do
      v <- _initializer once key
      liftIO $ insert table key v
      return v
  liftIO $ atomically $ putTMVar (_table once) table
  return value
