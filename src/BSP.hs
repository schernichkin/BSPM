{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module BSP
    ( Peer
    , run
    , read -- TODO: change names to not conflict with prelude
    , step
    , peerId
    , thisId
    , write
    ) where

import           BSPM.Util.CountDown
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Key
import           Data.Traversable
import           Prelude                hiding (read)

data Peer t a b = Peer
  { _peerId       :: !Int
  , _currentState :: !a
  , _nextState    :: !(IORef b)
  , _peers        :: !(t (Peer t a b))
  }

newtype Process t a b r = Process { unProcess :: Peer t a b -> IO r }

instance Functor (Process t a b) where
  fmap f m = Process $ fmap f . unProcess m

instance Applicative (Process t a b) where
  pure = Process . const . return
  f <*> g = Process $ \p -> unProcess f p <*> unProcess g p

instance Monad (Process t a b) where
  f >>= g = Process $ \p -> unProcess f p >>= flip unProcess p . g

instance MonadIO (Process t a b) where
  liftIO = Process . const

createPeers :: ( Traversable t ) => t a -> IO (Int, t (a, Peer t () ()))
createPeers t = do
  peerCounter <- newIORef 0
  peers <- mfix $ \peers -> forM t $ \a -> do
    i <- readIORef peerCounter
    writeIORef peerCounter (i + 1)
    stateRef <- newIORef mempty
    return (a, Peer
      { _peerId = i
      , _currentState = ()
      , _nextState = stateRef
      , _peers = fmap snd peers
      } )
  peerCount <- readIORef peerCounter
  return (peerCount, peers)

run :: ( Monoid r, Traversable t )
    => t a
    -> (a -> Process t () () r)
    -> IO r
run t p = do
  (n, peers) <- createPeers t
  workerCounter <- newCountDown n
  forM_ peers $ \(a, peer) -> void $ forkFinally
    ( let process = unProcess $ p a
      in process peer )
    ( const $ decCountDown workerCounter )
  waitCountDown workerCounter
  return mempty

read :: Process t a b a
read = Process $ return . _currentState

write :: ( Indexable t, Monoid b )
      => Key t
      -> b
      -> Process t a b ()
write k b = Process $ \peer -> atomicModifyIORef'
  (_nextState $ index (_peers peer) k)
  (\a -> (a `mappend` b, ()))

step :: Process t a b x -> (x -> Process t b c r) -> Process t a c r
step p f = do
  -- TODO: необходимо дождаться завершения всех процессов предыдущего шага,
  -- создать новую структуру пиров и запустить с ней следующий шаг.
  -- каждый из процессов знает свой результат, но ему еще нужна структура всех пиров,
  -- которая может быть построена только после того, как все процессы завершат работу.
  -- для построения такой структуры процессы могут писать свои результаты в массив,
  -- после того, как все результаты будут собраны, необходимо построить структуру,
  -- построить массив пиров и оповестить процессы о том, что они могут забрать свои
  -- пиры из массива.
  error "BSP.step"

thisId :: Process t a b Int
thisId = Process $ return . _peerId

peerId :: (Indexable t) => Key t -> Process t a b Int
peerId k = Process $ return . _peerId . flip index k . _peers
