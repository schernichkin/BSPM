module BSPM.Util.RunOnce
  ( RunOnce ()
  , newRunOnce
  , getRunOnce
  ) where

import Control.Concurrent.STM.TVar
import Control.Exception
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

-- | This function will restore RunOnce state in case of asyncronous exception.
-- But still the initializer can be interrupted. It's up to user to mark
-- initialiser as interruptable.
getRunOnce :: RunOnce a -> IO a
getRunOnce once = bracketOnError acquire abort initialize
  where
    acquire = atomically $ do
      token <- readTVar $ _token once
      case token of
        HasValue a -> return $ Just a
        Empty -> do
          writeTVar (_token once) Initializing
          return Nothing
        Initializing -> retry

    abort maybeVal = atomically $
      case maybeVal of
        Just val -> writeTVar (_token once) $ HasValue val
        Nothing -> writeTVar (_token once) Empty

    initialize maybeVal =
      case maybeVal of
        Just val -> return val
        Nothing -> do
          val <- _initializer once
          atomically $ writeTVar (_token once) $ HasValue val
          return val
