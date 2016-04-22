{-# LANGUAGE RecordWildCards #-}

module BSPM.Engine.Local
    ( BSPM (..)
--    , run
--    , send
--    , spawn
    ) where

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

-- краевые случаи:
-- 1. Созданный воркер не выполняет никакой работы и сразу завершается. Это приведет
--    к созданию счетчика и сразу сбросу его на 0, что может привести к посылке
--    EndReceive, что не правильно. Т.е. воркер должен в любом случае дождаться
--    EndReceive, даже если он не собирается выполнять никакой работы.
--    Это можно обрабатывать в функции receive, просто делая unGetTChan для
--    сообщений типа EndReceive, в случае форка следует добавлять функцию,
--    которая будет дожидаться EndReceive, возможно пропуская сообщения.

newtype BSPM a r = BSPM { unBSPM :: Worker a -> IO r }

data Message a = DataMessage a | EndReceive deriving ( Show, Eq )

data Worker a = Worker
  { _chan :: TChan (Message a)
  , _currentStep :: Step
  }

data Step = Step
  { _workerCount :: IORef Int
  , _finalizer :: IORef (IO ())
  , _nextStep :: IORef (Maybe Step)
  }

instance Functor (BSPM a) where
  fmap = liftM

instance Applicative (BSPM a) where
  pure = return
  (<*>) = ap

instance Monad (BSPM a) where
  return = BSPM . const . return
  a >>= f = BSPM $ \w -> unBSPM a w >>= flip unBSPM w . f

instance MonadIO (BSPM a) where
  liftIO = BSPM . const

newStep :: IO Step
newStep = do
  workerCount <- newIORef 1
  finalizer <- newIORef $ return ()
  nextStep <- newIORef Nothing
  return Step
    { _workerCount = workerCount
    , _finalizer = finalizer
    , _nextStep = nextStep
    }

newWorker :: Step -> IO (Worker a)
newWorker step = do
  chan <- newTChanIO
  return Worker
    { _chan = chan
    , _currentStep = step
    }

run :: BSPM Void r -> IO r
run bspm = do
  step <- newStep
  worker <- newWorker step
  result <- unBSPM bspm worker
  readIORef (_finalizer step)
  return result

endRecieve :: TChan (Message a) -> IO ()
endRecieve chan = skipToEnd
  where
    skipToEnd = do
      msg <- atomically (readTChan chan)
      case msg of
        EndReceive -> putStrLn "got EndRecieve message." >> return ()
        _ -> skipToEnd

spawn :: BSPM b () -> BSPM a (Worker b)
spawn child = BSPM $ \this -> do
  step' <- newStep
  -- TODO: we need to create state only once, hence the result of this
  -- function will be discarded most the times. Need to profile and optimize.
  nextStep <- atomicModifyIORef' (_nextStep $ _currentStep this) $ \maybeStep ->
    case maybeStep of
      Just s -> (Just s, s)
      Nothing -> (Just step', step')
  worker <- newWorker nextStep
  atomicModifyIORef' (_finalizer $ _currentStep this) $ \finalizer ->
    (finalizer >> putStrLn "Sending EndReceive to child worker" >> atomically (writeTChan (_chan worker) EndReceive), ())
  forkIO $ do --TODO: protect with try-catch, mask async exceptions
    unBSPM child worker
    endRecieve $ _chan worker
    workerCount <- atomicModifyIORef' (_workerCount nextStep) $ \vc -> (vc - 1, vc - 1)
    when (workerCount == 0) $ do
      readIORef $ _finalizer nextStep
      return ()
  return worker

{-

send :: Worker a -> a -> BSPM ()
send Worker {..} = BSPM . const . atomically . writeTChan _chan . DataMessage

-- чтобы recieve работал, BSPM должен знать про свой канал.
recieve :: Int
recieve = undefined

-- halt :: BSPM () -- actually we do not want to halt explicitly
-- halt = undefined
-}
