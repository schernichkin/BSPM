module BSPM.Engine.Local.WorkerSet where

import BSPM.Engine.Local
import BSPM.StateStream
import BSPM.Util.RunOnceSet
import Control.Monad.IO.Class
import Data.Hashable
import Data.Void

type BSPMW a k s r = BSPM a (WorkerSetState a s k) r

data WorkerSetState a s k = WorkerSetStep
  { _workerSet :: !(RunOnceSet k (Worker a (WorkerSetState a s k)))
  , _sharedState :: s
  }

runW :: BSPMW Void k s r -> StateStream s -> IO r
runW = run . undefined

sendTo :: ( Eq k, Hashable k ) => k -> a -> BSPMW a k s ()
sendTo key message = do
  state <- getSate
  worker <- liftIO $ getRunOnce (_workerSet state) key
  send worker message
