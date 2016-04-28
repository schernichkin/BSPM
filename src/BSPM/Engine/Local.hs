{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module BSPM.Engine.Local
    ( BSPM ()
    , WorkerState ()
    , receive
    , run
    , send
    ) where

import           BSPM.Util.CriticalSection
import           BSPM.Util.RunOnce
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Hashable
import qualified Data.HashTable.IO as H
import           Data.IORef

type HashTable k v = H.BasicHashTable k v

newtype BSPM a k r = BSPM { unBSPM :: WorkerState a k -> IO r }

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

data MessageWrapper a = DataMessage a
                      | EndReceive
                      | Halt
                      deriving ( Show, Eq )

data WorkerState a k = WorkerState
  { _chan :: !(TChan (MessageWrapper a))
  , _currentStep :: !(StepState a k)
  }

data StepState a k = StepState
  { _activeWorkers :: !(IORef Int)
  , _nextChans     :: !(CriticalSection (HashTable k (TChan (MessageWrapper a))))
  , _nextStep      :: !(RunOnce (StepState a k))
  , _workerFactory :: !(k -> BSPM a k ())
  , _rootChan      :: !(TChan (MessageWrapper a))
  }

{-# INLINE newStepState #-}
newStepState :: (k -> BSPM a k ()) -> TChan (MessageWrapper a) -> IO (StepState a k)
newStepState workerFactory rootChan = do
  activeWorkers <- newIORef 0 -- will be zero for root worker, I'm ok with it for now
  nextChans <- H.new >>= newCriticalSection
  nextStep <- newRunOnce $ newStepState workerFactory rootChan
  return StepState
    { _activeWorkers = activeWorkers
    , _nextChans     = nextChans
    , _nextStep      = nextStep
    , _workerFactory = workerFactory
    , _rootChan      = rootChan
    }

{-# INLINE broadcastEndRecieve #-}
broadcastEndRecieve :: (StepState a k) -> IO ()
broadcastEndRecieve step =
  withCriticalSection
    ( _nextChans step )
    ( H.mapM_ $ atomically . flip writeTChan EndReceive . snd )

{-# INLINE endRecieve #-}
endRecieve :: TChan (MessageWrapper a) -> IO ()
endRecieve chan = skipToEnd
  where
    skipToEnd = do
      msg <- atomically $ readTChan chan
      case msg of
        EndReceive -> return ()
        _ -> skipToEnd

{-# INLINE getWorkerChan #-}
getWorkerChan :: ( Eq k, Hashable k ) => StepState a k -> k -> IO (TChan (MessageWrapper a))
getWorkerChan step k = modifyCriticalSection (_nextChans step) $ \nextChans -> do
  maybeChan <- H.lookup nextChans k
  case maybeChan of
    Just chan -> return (nextChans, chan)
    Nothing -> do
      nextStep <- getRunOnce (_nextStep step)
      chan <- newTChanIO
      mask_ $ do
        atomicModifyIORef' (_activeWorkers nextStep) $ \a -> (a + 1, ())
        forkIOWithUnmask $ \unmask -> finally
          ( unmask $ do
            unBSPM
              (_workerFactory nextStep k)
              WorkerState { _chan = chan, _currentStep = nextStep }
            endRecieve chan )
          ( do
            c <-atomicModifyIORef' (_activeWorkers nextStep) $ \a -> (a - 1, a - 1)
            when (c == 0) $ do
              hasNextStep <- initialized $ _nextStep nextStep
              if hasNextStep
                then ( broadcastEndRecieve nextStep )
                else atomically $ writeTChan (_rootChan nextStep) Halt )
        H.insert nextChans k chan
      return (nextChans, chan)

run :: (k -> BSPM a k ()) -> BSPM a k r -> IO r
run workerFactory bspm = do
  chan <- newTChanIO
  step <- newStepState workerFactory chan
  result <- finally
    ( unBSPM bspm WorkerState { _chan = chan, _currentStep = step } )
    ( broadcastEndRecieve step )
  halt <- atomically $ readTChan chan
  case halt of
    Halt  -> return result
    -- this should never happen
    EndReceive -> error "BSPM.Engine.Local.run: EndReceive"
    DataMessage _ -> error "BSPM.Engine.Local.run: DataMessage"

send :: ( Eq k, Hashable k ) => k -> a -> BSPM a k ()
send k a = BSPM $ \worker -> do
  chan <- getWorkerChan (_currentStep worker) k
  atomically $ writeTChan chan $ DataMessage a

receive :: BSPM a s (Maybe a)
receive = BSPM $ \worker -> atomically $ do
  msg <- readTChan $ _chan worker
  case msg of
    DataMessage a -> return $ Just a
    EndReceive -> do
      unGetTChan (_chan worker) msg
      return Nothing
    -- this should never happen
    Halt -> error "BSPM.Engine.Local.receive: Halt"
