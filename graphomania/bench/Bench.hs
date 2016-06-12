{-# LANGUAGE ScopedTypeVariables #-}

module Bench where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Criterion.Main
import           Data.Graph.Unboxed.Reader.WikiVote
import           Data.HashTable.Class               (HashTable)
import           Data.HashTable.IO                  (IOHashTable)
import qualified Data.HashTable.IO                  as H
import qualified Data.HashTable.ST.Basic            as Basic
import qualified Data.HashTable.ST.Cuckoo           as Cuckoo
import           Data.MonoTraversable
import           Data.Proxy
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphomania.Shumov
import qualified Graphomania.Shumov.Offheap         as Off
import           Paths_graphomania
import           System.IO

populateHashtable :: forall h . HashTable h => Proxy h -> Int -> IO ()
populateHashtable _ c = do
  table <- H.new :: IO (IOHashTable h Int Int)
  forM_  [1..c] $ \i -> H.insert table i i

readWikiVote :: IO ()
readWikiVote = do
  graphPath <- getDataFileName "wiki-Vote.txt"
  void $ withFile graphPath ReadMode readGraph
  return ()

readShumovBETest :: IO Int
readShumovBETest = do
  path <- getDataFileName "shumov/graph.bin"
  g <- readShumovBE path
  return $ olength g

readShumovLETest :: IO Int
readShumovLETest = do
  path <- getDataFileName "shumov/graph_le.bin"
  g <- readShumovLE path
  return $ olength g

shomovBench :: Benchmark
shomovBench = env setupEnv $ \ ~(gBe, gLe, gOff) ->
  bgroup "Shumov"
    [ bench "BE" $ nf olength gBe
    , bench "LE" $ nf olength gLe
    , bench "Off" $ nf olength gOff
    ]
  where
    setupEnv = do
      pathBe <- getDataFileName "shumov/graph.bin"
      gBe <- readShumovBE pathBe
      pathLe <- getDataFileName "shumov/graph_le.bin"
      gLe <- readShumovLE pathLe
      gOff <- Off.readShumov pathLe
      return (gBe, gLe, gOff)

main :: IO ()
main = defaultMain
  [ bgroup "Baseline"
    [ bgroup "hashtables"
      [ bench "Basic 10 000 inserts" $ nfIO $ populateHashtable (Proxy :: Proxy Basic.HashTable) 10000
      , bench "Cuckoo 10 000 inserts" $ nfIO $ populateHashtable (Proxy :: Proxy Cuckoo.HashTable) 10000
      ]
    ]
  , bgroup "Data.Graph.Unboxed.Reader.WikiVote"
    [ bench "readGraph" $ nfIO readWikiVote
    ]
  , shomovBench
  ]
