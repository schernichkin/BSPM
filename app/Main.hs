module Main where

import BSPM.Engine.Local
import BSPM.Engine.Local.WorkerSet
import Control.Monad.IO.Class
import System.IO
import Data.Void
import Control.Monad
import qualified BSPM.StateStream as SS
import Data.Map.Strict as Map
import  BSPM.SSSP as SSSP

graph = Map.fromList
  [ (1, [(2, 7), (3, 9), (6, 14)])
  , (2, [(1, 7), (3, 10), (4, 15)])
  , (3, [(1, 9), (2, 10), (4, 11), (6, 2)])
  , (4, [(2, 15), (3, 11), (5, 6)])
  , (5, [(4, 6), (6, 9)])
  , (6, [(1, 14), (3, 2), (5, 9)])
  ]

main :: IO ()
main = do
  putStrLn "press any key to exit.."
  SSSP.runOnGraph graph 1000 1 5
  --run SS.unit bspmRoot
  void getLine
