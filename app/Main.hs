module Main where

import BSPM.Engine.Local
import Control.Monad.IO.Class
import Data.Void
import Control.Monad
import Data.Map.Strict as Map
import BSPM.SSSP as SSSP
import Data.Graph.Unboxed
import Data.Graph.Unboxed.Builder
import Data.Graph.Unboxed.Internal
import qualified Data.HashTable.IO as H
import Paths_BSPM
import Data.Graph.Unboxed.Reader.WikiVote
import System.IO

newGraph :: IO (UGraph Int (Int, Double))
newGraph = build $ do
  addEdges 1 [(2, 7), (3, 9), (6, 14)]
  addEdges 2 [(1, 7), (3, 10), (4, 15)]
  addEdges 3 [(1, 9), (2, 10), (4, 11), (6, 2)]
  addEdges 4 [(2, 15), (3, 11), (5, 6)]
  addEdges 5 [(4, 6), (6, 9)]
  addEdges 6 [(1, 14), (3, 2), (5, 9)]

main :: IO ()
main = do
  putStrLn "Loading graph.."
  graphPath <- getDataFileName "wiki-Vote.txt"
  graph <- withFile graphPath ReadMode readGraph
  SSSP.runOnGraph graph 1000 8232 4332
