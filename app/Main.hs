module Main where

import BSPM.Engine.Local
import BSPM.Engine.Local.WorkerSet
import Control.Monad.IO.Class
import System.IO
import Data.Void
import Control.Monad
import qualified BSPM.StateStream as SS

data Message = Message String

worker :: BSPM Message () ()
worker = do
  liftIO $ putStrLn "bspm worker spawned"
  return ()

bspmRoot :: BSPM Void () ()
bspmRoot = do
  liftIO $ putStrLn "bspmRoot!"
  w1 <- spawn worker
  w2 <- spawn worker
  --send w1 $ Message "test1"
  --send w1 $ Message "test2"
  --send w2 $ Message "test3"
  return ()

main :: IO ()
main = do
  putStrLn "press any key to exit.."
  run SS.unit bspmRoot
  void getLine
