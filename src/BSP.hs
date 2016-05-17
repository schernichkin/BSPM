{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module BSP
    (
    ) where

import           Control.Monad          (Monad)
import qualified Control.Monad          as M
import           Control.Monad.Indexed
import           Control.Monad.IO.Class
import           Data.Void
import           Prelude                hiding (Monad (..))

instance (IxFunctor f) => Functor (f i i) where
  fmap = imap

instance (IxApplicative f) => Applicative (f i i) where
  pure = ireturn
  (<*>) = iap

instance (IxMonad m) => Monad (m i i) where
  (>>=) = (>>=)
  return = ireturn

return :: (IxPointed m) => a -> m i i a
return = ireturn

(>>=) :: (IxMonad m) => m a b x -> (x -> m b c y) -> m a c y
(>>=) = flip ibind

(>>) :: (IxMonad m) => m a b x -> m b c y -> m a c y
(>>) = flip (ibind . const)

class Broadcast m where
  type BroadcastMessage m
  broadcast :: (BroadcastMessage m) -> m ()

data Process a b r = Process

instance IxFunctor Process where
  imap f = const Process

instance IxPointed Process where
  ireturn = const Process

instance IxApplicative Process where
  iap a = const Process

instance IxMonad Process where
  ibind m = const Process

instance MonadIO (Process i i) where
  liftIO = const Process

instance Broadcast (Process a b) where
  type BroadcastMessage (Process a b) = b
  broadcast = error "Broadcast (Process a b) => broadcast"

data Worker a b r = Worker

instance IxFunctor Worker where
  imap f = const Worker

instance IxPointed Worker where
  ireturn = const Worker

instance IxApplicative Worker where
  iap a = const Worker

instance IxMonad Worker where
  ibind m = const Worker

instance MonadIO (Worker i i) where
  liftIO = const Worker

data WorkerID

read :: Worker a b a
read = undefined

run :: Int -> ([WorkerID] -> Process Void Void r) -> IO r
run = undefined

step :: Worker a b r -> Process a b r
step = undefined

testWorker1 :: Worker i i ()
testWorker1 = do
  liftIO $ putStrLn "test worker"
  return ()

testWorker2 :: Worker i String ()
testWorker2 = undefined

testWorker3 :: Worker String i ()
testWorker3 = undefined

tessst :: Worker Void Void ()
tessst = testWorker2 >> testWorker3

runTestProcess = run 10 $ \workers -> do
  step $ do
    testWorker2 >> testWorker3
  return ()
