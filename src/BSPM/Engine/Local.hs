module BSPM.Engine.Local
    ( BSPM ()
    , Worker ()
    , getSate
    , receive
    , run
    , send
    , spawn
    ) where

import BSPM.StateStream ( StateStream(..) )
import BSPM.Util
import Control.Comonad.Cofree
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import Data.IORef
import Data.Void

      -- Воркер привязан к супершагу, новый супершаг = новый воркек
      -- Операции воркера.
      -- 1. В любой момент можно создавать новые воркеры и посылать им сообщения
      --    (сообщение будут доступны в следующем супер-шаге).
      -- 1.1 Дополнтительно всем воркерам доступен глобальный реестр созданных на
      --     данном супершаге воркеров.
      -- 2. Воркер может прочитать все свои сообщения. Операция чтения блокирует воркер,
      --    если очередь сообщений пустая, так же после того, как все воркеры предыдущего
      --    супершага окончили отправку сообщений, в очередь помещается сообщение
      --    end-recieve означающее, что больше сообщений не будет.
      -- 3. Воркер может послать сообщение vote-halt и завершить работу. После того,
      --    как все воркеры послали vote-halt, текущий шаг считается завершенным и в
      --    очередь всем воркерам следующего шага кладётся сообщение end-recieve

newtype BSPM a s r = BSPM { unBSPM :: Worker a s -> IO r }

data Message a = DataMessage a | EndReceive deriving ( Show, Eq )

data Worker a s = Worker
  { _chan :: !(TChan (Message a))
  , _currentStep :: !(Step s)
  }

data Step s = Step
  { _workerCount :: !(IORef Int)
  , _finalizer :: !(IORef (IO ()))
  , _nextStep :: !(RunOnce (Step s))
  , _sharedSate :: s
  }

instance Functor (BSPM a s) where
  fmap = liftM

instance Applicative (BSPM a s) where
  pure = return
  (<*>) = ap

instance Monad (BSPM a s) where
  return = BSPM . const . return
  a >>= f = BSPM $ \w -> unBSPM a w >>= flip unBSPM w . f

instance MonadIO (BSPM a s) where
  liftIO = BSPM . const

{-# INLINE newStep #-}
newStep :: StateStream s -> IO (Step s)
newStep (state :< ss) = do
  workerCount <- newIORef 0 -- will be zero for root worker, I'm ok with it for now
  finalizer <- newIORef $ return ()
  stateStream <- ss
  nextStep <- newRunOnce $ newStep stateStream
  return Step
    { _workerCount = workerCount
    , _finalizer = finalizer
    , _nextStep = nextStep
    , _sharedSate = state
    }

{-# INLINE newWorker #-}
newWorker :: Step s -> IO (Worker a s)
newWorker step = do
  chan <- newTChanIO
  return Worker
    { _chan = chan
    , _currentStep = step
    }

run :: StateStream s -> BSPM a s r -> IO r
run ss bspm = do
  step <- newStep ss
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

spawn :: BSPM a s () -> BSPM b s (Worker a s)
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

receive :: BSPM a s (Maybe a)
receive = BSPM $ \worker -> atomically $ do
  msg <- readTChan $ _chan worker
  case msg of
    DataMessage a -> return $ Just a
    EndReceive -> do
      unGetTChan (_chan worker) msg
      return Nothing

{-# INLINE getSate #-}
getSate :: BSPM b s s
getSate = BSPM $ \worker -> return $ _sharedSate $ _currentStep worker
