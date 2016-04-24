module BSPM.Util.RunOnceSet
  ( RunOnceSet ()
  , newRunOnce
  , getRunOnce
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Hashable
import Data.HashTable.IO as H

data RunOnceSet m k v = RunOnceSet
  { _token :: !(TVar (RunOnceToken k v) )
  , _initializer :: !(k -> m v)
  }

data RunOnceToken k v = Locked | HasValue (BasicHashTable k v)

newRunOnce :: ( MonadIO m ) => (k -> m v) -> IO (RunOnceSet m k v)
newRunOnce initializer = do
  table <- new
  token <- newTVarIO $ HasValue table
  return RunOnceSet
    { _token = token
    , _initializer = initializer
    }

{-# INLINE getRunOnce #-}
getRunOnce :: ( MonadIO m, Eq k, Hashable k ) => RunOnceSet m k v -> k -> m v
getRunOnce once key = do --TODO: mask async exceptions, protect with try-finally
  table <- liftIO $ atomically $ do
    token <- readTVar $ _token once
    case token of
      HasValue v -> do
        writeTVar (_token once) Locked
        return v
      Locked -> retry
  maybeValue <- liftIO $ H.lookup table key
  value <- case maybeValue of
    Just v -> return v
    Nothing -> do
      v <- _initializer once key
      liftIO $ insert table key v
      return v
  liftIO $ atomically $ writeTVar (_token once) $ HasValue table
  return value
