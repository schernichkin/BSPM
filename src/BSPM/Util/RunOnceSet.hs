module BSPM.Util.RunOnceSet
  ( RunOnceSet ()
  , newRunOnce
  , getRunOnce
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Hashable
import Data.HashTable.IO as H

data RunOnceSet k v = RunOnceSet
  { _token :: !(TVar (RunOnceToken k v) )
  , _initializer :: !(k -> IO v)
  }

data RunOnceToken k v = Locked | HasValue (BasicHashTable k v)

newRunOnce :: (k -> IO v) -> IO (RunOnceSet k v)
newRunOnce initializer = do
  table <- new
  token <- newTVarIO $ HasValue table
  return RunOnceSet
    {  _token = undefined
    , _initializer = initializer
    }

{-# INLINE getRunOnce #-}
getRunOnce :: ( Eq k, Hashable k ) => RunOnceSet k v -> k -> IO v
getRunOnce once key = do --TODO: mask async exceptions, protect with try-finally
  table <- atomically $ do
    token <- readTVar $ _token once
    case token of
      HasValue v -> do
        writeTVar (_token once) Locked
        return v
      Locked -> retry
  maybeValue <- H.lookup table key
  value <- case maybeValue of
    Just v -> return v
    Nothing -> do
      v <- _initializer once key
      insert table key v
      return v
  atomically $ writeTVar (_token once) $ HasValue table
  return value
