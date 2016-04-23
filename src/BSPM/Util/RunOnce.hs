module BSPM.Util.RunOnce
  ( RunOnce ()
  , newRunOnce
  , getRunOnce
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM

data RunOnce a = RunOnce
  { _token :: !(TVar (RunOnceToken a) )
  , _initializer :: !(IO a)
  }

data RunOnceToken a = Empty | Initializing | HasValue a

newRunOnce :: IO a -> IO (RunOnce a)
newRunOnce initializer = do
  token <- newTVarIO Empty
  return RunOnce
    { _token = token
    , _initializer = initializer
    }

getRunOnce :: RunOnce a -> IO a
getRunOnce once = do
  maybeVal <- atomically $ do
    token <- readTVar $ _token once
    case token of
      HasValue a -> return $ Just a
      Empty -> do
        writeTVar (_token once) Initializing
        return Nothing
      Initializing -> retry
  case maybeVal of
    Just val -> return val
    Nothing -> do
      val <- _initializer once
      atomically $ writeTVar (_token once) $ HasValue val
      return val
