module Main where

import           BSP
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Graph.Unboxed
import           Data.Graph.Unboxed.Builder
import           Data.Graph.Unboxed.Internal
import           Data.Graph.Unboxed.Reader.WikiVote
import qualified Data.HashTable.IO                  as H
import           Data.Key
import           Data.Map.Strict                    as Map
import           Data.Shards.Ordered
import           Data.Void
import           Paths_BSPM
import           System.IO
{-
newGraph :: IO (OrderedShardMap Int (UGraph Int (Int, Double)))
newGraph = buildSharded 4 $ do
  addEdges 1 [(2, 7), (3, 9), (6, 14)]
  addEdges 2 [(1, 7), (3, 10), (4, 15)]
  addEdges 3 [(1, 9), (2, 10), (4, 11), (6, 2)]
  addEdges 4 [(2, 15), (3, 11), (5, 6)]
  addEdges 5 [(4, 6), (6, 9)]
  addEdges 6 [(1, 14), (3, 2), (5, 9)]

--test :: (Traversable t, Indexable t) => t PeerId -> Key t -> Key t -> IO ()
test shardes src dst = do
  lock <- newMVar ()
  run shardes $ \a -> do
    i <- thisId
    srcSharde <- peerId src
    dstSharde <- peerId dst
    liftIO $ withMVar lock $ const $ do
      putStrLn $ show i ++ " ------------------------"
      when (srcSharde == i) $ putStrLn "Has shource"
      when (dstSharde == i) $ putStrLn "Has target"
    return ()

data TestData = TestData
  { _a :: !Int
  , _b :: ![TestData]
  } deriving ( Show )

testFix :: IO [TestData]
testFix = mfix (\a -> forM [1::Int,2,3] (\b -> do
  print b
  return $ TestData  b  a  ))

main :: IO ()
main = do
  shardes <- newGraph
  test shardes 1 5
  return ()
  {-
  putStrLn "Loading graph.."
  graphPath <- getDataFileName "wiki-Vote.txt"
  graph <- withFile graphPath ReadMode (readSharded 10)
  print graph
  -- SSSP.runOnGraph graph 1000 8232 4332
  -}
-}

data Worker i o = forall Worker 

main :: IO ()
main = do
  return ()
