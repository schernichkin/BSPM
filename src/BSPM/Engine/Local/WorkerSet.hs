module BSPM.Engine.Local.WorkerSet where

import BSPM.Engine.Local
import BSPM.StateStream as SS
import BSPM.Util.RunOnceSet
import Control.Monad.IO.Class
import Data.Functor.Apply
import Data.Hashable
import Data.Void

type BSPMW a k s = BSPM a (WorkerSetState a k s)

data WorkerSetState a k s = WorkerSetState
  { _workerSet :: !(RunOnceSet (BSPMW a k s) k (Worker a (WorkerSetState a k s)))
  , _sharedState :: s
  }

newWorkerSetState :: (k -> BSPMW a k s ()) -> IO (s -> WorkerSetState a k s)
newWorkerSetState workers = do
  workerSet <- newRunOnce (spawn . workers)
  return $ \state -> WorkerSetState
    { _workerSet = workerSet
    , _sharedState = state
    }

runW :: StateStream s -> (k -> BSPMW a k s ())  -> BSPMW a k s r-> IO r
runW userStateStream  workers bspmw = do
  workerSetStateStream <- SS.new $ newWorkerSetState workers
  run (workerSetStateStream <.> userStateStream) bspmw

sendTo :: ( Eq k, Hashable k ) => k -> a -> BSPMW a k s ()
sendTo key message = do
  state <- getSate
  worker <- getRunOnce (_workerSet state) key
  send worker message
