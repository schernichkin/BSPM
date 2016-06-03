module BSP.Util.InitOnce
    ( InitOnce
    , getInitOnce
    , newInitOnce
    ) where

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.STM

newtype InitOnce a = InitOnce (TVar (InitOnceToken a))

data InitOnceToken a = Unitialized
                     | Initializing
                     | Initialized a

newInitOnce :: IO (InitOnce a)
newInitOnce = InitOnce <$> newTVarIO Unitialized

getInitOnce :: InitOnce a -> IO a -> IO a
getInitOnce (InitOnce tokenVar) initializer =
  bracketOnError acquire release $ \maybeVal ->
    case maybeVal of
      Just val -> return val
      Nothing  -> do
        val <- initializer
        atomically $ writeTVar tokenVar $ Initialized val
        return val
  where
    acquire = atomically $ do
      token <- readTVar tokenVar
      case token of
        Initialized a -> return $ Just a
        Unitialized -> do
          writeTVar tokenVar Initializing
          return Nothing
        Initializing -> retry

    release maybeVal = atomically $
      case maybeVal of
        Nothing  -> writeTVar tokenVar Unitialized
        Just val -> writeTVar tokenVar $ Initialized val
