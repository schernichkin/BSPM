{-# LANGUAGE ScopedTypeVariables #-}

module Bench where

import           BSPM.Engine.Local
import           BSPM.Util.CountDown
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Criterion.Main
import           Data.Graph.Unboxed.Reader.WikiVote
import           Data.HashTable.Class ( HashTable )
import           Data.HashTable.IO ( IOHashTable )
import qualified Data.HashTable.IO as H
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import qualified Data.HashTable.ST.Basic as Basic
import           Data.Proxy
import           Paths_BSPM
import           System.IO

forkBench :: Int -> IO ()
forkBench c = do
  cd <- newCountDown c
  replicateM_ c $ forkIO $ decCountDown cd
  waitCountDown cd

sendBench :: Int -> IO ()
sendBench c = do
  run ( const $ return () ) $ forM_  [1..c] $ flip send ()

populateHashtable :: forall h . HashTable h => Proxy h -> Int -> IO ()
populateHashtable _ c = do
  table <- H.new :: IO (IOHashTable h Int Int)
  forM_  [1..c] $ \i -> H.insert table i i

readWikiVote :: IO ()
readWikiVote = do
  graphPath <- getDataFileName "wiki-Vote.txt"
  void $ withFile graphPath ReadMode readGraph
  return ()

main :: IO ()
main = defaultMain
  [ bgroup "Baseline"
    [ bgroup "forkIO"
      [ bench "fork and wait 10 000" $ nfIO $ forkBench 10000
      -- , bench "fork and wait 1 000 000" $ nfIO $ forkAndWait 1000000
      ]
    , bgroup "hashtables"
      [ bench "Basic 10 000 inserts" $ nfIO $ populateHashtable (Proxy :: Proxy Basic.HashTable) 10000
      , bench "Cuckoo 10 000 inserts" $ nfIO $ populateHashtable (Proxy :: Proxy Cuckoo.HashTable) 10000
      ]
    ]
  , bgroup "BSPM.Engine.Local"
    [  bench "send 10 000" $ nfIO $ sendBench 10000
    ]
  , bgroup "Data.Graph.Unboxed.Reader.WikiVote"
    [ bench "readGraph" $ nfIO readWikiVote
    ]
  ]
