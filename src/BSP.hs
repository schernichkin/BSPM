{-# LANGUAGE FlexibleInstances #-}

module BSP
    ( test
    ) where

import           BSPM.Util.CountDown
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Vector             (Vector)
import qualified Data.Vector             as V
import           Data.Void

data Peer a = Peer
  {
  --  _this  :: !PeerId
  -- , _peers :: !(Vector PeerId)
   _state :: !a
  }

newtype PeerId = PeerId Int deriving ( Show )

newtype Process a b r = Process { unProcess :: Peer a -> IO r }

instance Functor (Process a b) where
  fmap f m = Process $ fmap f . unProcess m

instance Applicative (Process a b) where
  pure = Process . const . return
  f <*> g = Process $ \p -> unProcess f p <*> unProcess g p

instance Monad (Process a b) where
  f >>= g = Process $ \p -> unProcess f p >>= flip unProcess p . g

instance MonadIO (Process a b) where
  liftIO = Process . const

run :: (Monoid r) => Int -> Process () Void r -> IO r
run n p = do
  activeWorkers <- newCountDown n
  resultRef <- newIORef mempty
  let peers = V.generate n $ \i -> Peer
        {
        --  _this = PeerId i
    --     , _peers = undefined
         _state = ()
        }
  V.forM_ peers $ \peer -> forkIO $ do
    r <- unProcess p peer -- >>= atomicModifyIORef' result (\a -> (undefined, undefined))
    atomicModifyIORef' resultRef (\a -> (a `mappend` r, ()))
    decCountDown activeWorkers
  waitCountDown activeWorkers
  readIORef resultRef

-- this :: Process a b PeerId
-- this = Process $ return . _this

-- peers :: Process a b (Vector PeerId)
-- peers = Process $ return . _peers

read :: Process a b a
read = Process $ return . _state

write :: (Monoid b) => PeerId -> b -> Process a b ()
write = error "BSP.write"

step :: (Monoid x) => Process a b x -> (x -> Process b c r) -> Process a c r
step = error "BSP.step"

test = do
  lock <- newMVar ()
  run 10 $ do
    liftIO $ withMVar lock $ const $ putStrLn "test"
    return ()

{-
data Step a b r = Step

instance Functor (Step a b) where
  fmap f = const Step

instance Applicative (Step a b) where
  pure = const Step
  (<*>) = const . const Step

instance Monad (Step a b) where
  (>>=) = const . const Step

data Process a b r = Process

data PeerId = PeerId deriving ( Show )

run :: Int -> Process Void Void r -> IO r
run = undefined

this :: Step a b PeerId
this = return PeerId
-}
