module BSPM.Util.RunOnce
  ( RunOnce ()
  , newRunOnce
  , getRunOnce
  , initialized
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.STM

newtype RunOnce a = RunOnce { unRunOnce :: TVar (RunOnceToken a) }

data RunOnceToken a = Unitialized !(IO a)
                    | Initializing
                    | Initialized a

{-# INLINE newRunOnce #-}
newRunOnce :: IO a -> IO (RunOnce a)
newRunOnce initializer = RunOnce <$> newTVarIO (Unitialized initializer)

-- | This function will restore RunOnce state in case of asyncronous exception.
-- But still the initializer can be interrupted. It's up to user to mark
-- initialiser as interruptable.
{-# INLINE getRunOnce #-}
getRunOnce :: RunOnce a -> IO a
getRunOnce (RunOnce once) = bracketOnError acquire release $ \eitherVal ->
  case eitherVal of
    Right val -> return val
    Left initializer -> do
      val <- initializer
      atomically $ writeTVar once $ Initialized val
      return val
  where
    acquire = atomically $ do
      state <- readTVar once
      case state of
        Initialized a -> return $ Right a
        Unitialized initializer -> do
          writeTVar once Initializing
          return $ Left initializer
        Initializing -> retry

    release eitherVal = atomically $
      case eitherVal of
        Left initializer -> writeTVar once $ Unitialized initializer
        Right val -> writeTVar once $ Initialized val

initialized :: RunOnce a -> IO Bool
initialized once = do
  token <- atomically $ readTVar $ unRunOnce once
  return $ case token of
    Initialized _ -> True
    _ -> False
