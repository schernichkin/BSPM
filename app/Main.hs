module Main where

import           BSPM.Engine.Local
import           BSPM.SSSP                          as SSSP
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Graph.Unboxed
import           Data.Graph.Unboxed.Builder
import           Data.Graph.Unboxed.Internal
import           Data.Graph.Unboxed.Reader.WikiVote
import qualified Data.HashTable.IO                  as H
import           Data.Map.Strict                    as Map
import           Data.Shards.Ordered
import           Data.Void
import           Paths_BSPM
import           System.IO

newGraph :: IO (OrderedShardMap Int (UGraph Int (Int, Double)))
newGraph = buildSharded 4 $ do
  addEdges 1 [(2, 7), (3, 9), (6, 14)]
  addEdges 2 [(1, 7), (3, 10), (4, 15)]
  addEdges 3 [(1, 9), (2, 10), (4, 11), (6, 2)]
  addEdges 4 [(2, 15), (3, 11), (5, 6)]
  addEdges 5 [(4, 6), (6, 9)]
  addEdges 6 [(1, 14), (3, 2), (5, 9)]

main :: IO ()
main = do
  g <- newGraph
  print g
  return ()
  -- putStrLn "Loading graph.."
  -- graphPath <- getDataFileName "wiki-Vote.txt"
  -- graph <- withFile graphPath ReadMode readGraph
  -- SSSP.runOnGraph graph 1000 8232 4332
