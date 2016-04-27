module BSPM.Engine.Local
    ( BSPM ()
    , Worker ()
    , receive
    , run
    , send
    , spawn
    , sendTo
    ) where

import BSPM.StateStream ( StateStream(..) )
import BSPM.Util
import Control.Comonad.Cofree
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import Data.Hashable
import Data.IORef
import Data.Void

import BSPM.Util.RunOnceSet (RunOnceSet)
import qualified BSPM.Util.RunOnceSet as RS

newtype BSPM a k r = BSPM { unBSPM :: Worker a k -> IO r }

instance Functor (BSPM a k) where
  fmap = liftM

instance Applicative (BSPM a k) where
  pure = return
  (<*>) = ap

instance Monad (BSPM a k) where
  return = BSPM . const . return
  a >>= f = BSPM $ \w -> unBSPM a w >>= flip unBSPM w . f

instance MonadIO (BSPM a k) where
  liftIO = BSPM . const

data Message a = DataMessage a | EndReceive deriving ( Show, Eq )

data Worker a k = Worker
  { _chan :: !(TChan (Message a))
  , _currentStep :: !(Step a k)
  }

-- TODO: нужно отслеживать окончание работы.
-- Окончание работы может быть обнаружено в процессе вызова финалайзера.
-- Один из вариантов - проверить наличие инициализированного _nextStep.
data Step a k = Step
  { _workerCount :: !(IORef Int)
  -- После объединения базовой функциональности с WorkerSet нам больше не
  -- понадобится цепочка финалайзеров.
  , _finalizer :: !(IORef (IO ()))
  , _nextStep :: !(RunOnce (Step a k))
  , _workerSet :: (RunOnceSet (BSPM a k) k (Worker a k))
  }

{-# INLINE newStep #-}
newStep :: (k -> BSPM a k ()) -> IO (Step a k)
newStep workers = do
  workerCount <- newIORef 0 -- will be zero for root worker, I'm ok with it for now
  finalizer <- newIORef $ return ()
  nextStep <- newRunOnce (newStep workers)
  workerSet <- RS.newRunOnce (spawn . workers)
  return Step
    { _workerCount = workerCount
    , _finalizer = finalizer
    , _nextStep = nextStep
    , _workerSet = workerSet
    }

{-# INLINE newWorker #-}
newWorker :: Step a k -> IO (Worker a k)
newWorker step = do
  chan <- newTChanIO
  return Worker
    { _chan = chan
    , _currentStep = step --TODO: зарегистрировать воркер
    }

run :: (k -> BSPM a k ()) -> BSPM a k r -> IO r
run workers bspm = do
  step <- newStep workers
  worker <- newWorker step
  result <- unBSPM bspm worker
  join $ readIORef (_finalizer step)
  return result

endRecieve :: TChan (Message a) -> IO ()
endRecieve chan = skipToEnd
  where
    skipToEnd = do
      msg <- atomically (readTChan chan)
      case msg of
        EndReceive -> return () -- void $ putStrLn "got EndRecieve message."
        _ -> skipToEnd

--TODO: сделать приватной.
spawn :: BSPM a k () -> BSPM a k (Worker a k)
spawn child = BSPM $ \this -> do
  nextStep <- getRunOnce (_nextStep $ _currentStep this)
  worker <- newWorker nextStep
  atomicModifyIORef' (_finalizer $ _currentStep this) $ \finalizer ->
    (finalizer >> {- putStrLn "Sending EndReceive to child worker" >>  -} atomically (writeTChan (_chan worker) EndReceive), ())
  -- TODO: implement correct worker count protocol (e.g. take here, put in forked thread)
  vc <- atomicModifyIORef' (_workerCount nextStep) $ \vc -> (vc + 1, vc + 1)
  -- putStrLn $ "next step worker count " ++ (show vc)
  forkIO $ do --TODO: protect with try-catch, mask async exceptions
    unBSPM child worker
    endRecieve $ _chan worker
    workerCount <- atomicModifyIORef' (_workerCount nextStep) $ \vc -> (vc - 1, vc - 1)
    -- liftIO $ putStrLn $  "worker count == " ++ (show workerCount)
    when (workerCount == 0) $ do
      join $ readIORef $ _finalizer nextStep
      return ()
  return worker

send :: Worker a s -> a -> BSPM b s ()
send worker =  BSPM . const . atomically . writeTChan (_chan worker) . DataMessage

sendTo :: ( Eq k, Hashable k ) => k -> a -> BSPM a k ()
sendTo key message = do
  this <- BSPM return
  worker <- RS.getRunOnce (_workerSet $ _currentStep this) key
  send worker message

receive :: BSPM a s (Maybe a)
receive = BSPM $ \worker -> atomically $ do
  msg <- readTChan $ _chan worker
  case msg of
    DataMessage a -> return $ Just a
    EndReceive -> do
      unGetTChan (_chan worker) msg
      return Nothing
