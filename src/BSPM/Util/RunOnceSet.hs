{-# LANGUAGE FlexibleContexts #-}

module BSPM.Util.RunOnceSet
  ( RunOnceSet ()
  , newRunOnce
  , getRunOnce
  ) where

import Control.Concurrent.STM.TMVar
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Control
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

getRunOnce :: ( MonadBase IO m, Eq k, Hashable k ) => RunOnceSet m k v -> k -> m v
getRunOnce once key = bracket acquire release initialize
  where
    acquire = liftBase $ atomically $ takeTMVar (_table once)
    release = liftBase . atomically . putTMVar (_table once)
    initialize table = do
      maybeValue <- liftBase $ H.lookup table key
      case maybeValue of
        Just v -> return v
        Nothing -> do
          v <- _initializer once key
          liftBase $ insert table key v
          return v
