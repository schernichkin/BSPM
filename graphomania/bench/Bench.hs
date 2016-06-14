{-# LANGUAGE ScopedTypeVariables #-}

module Bench where

import           Control.Lens
import           Control.Monad
import           Criterion.Main
import           Data.Graph.Unboxed.Reader.WikiVote
import           Data.HashTable.Class               (HashTable)
import           Data.HashTable.IO                  (IOHashTable)
import qualified Data.HashTable.IO                  as H
import qualified Data.HashTable.ST.Basic            as Basic
import qualified Data.HashTable.ST.Cuckoo           as Cuckoo
import           Data.MonoTraversable
import           Data.Proxy
import           Graphomania.Shumov
import qualified Graphomania.Shumov.Offheap         as Off
import qualified Graphomania.Shumov.OffheapBS       as OffBS
import qualified Graphomania.Shumov.OffheapI        as OffI
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
shomovBench = env setupEnv $ \ ~(gBe, gLe, gOff, gOffI, gOffBS) ->
  bgroup "Shumov"
    [ bench "BE" $ nf olength gBe
    , bench "LE" $ nf olength gLe
    , bench "Off" $ nf olength gOff
    , bench "lengthOf" $ nf (lengthOf Off.offheapVertices) gOff
    , bench "lengthOfI" $ nf (lengthOf OffI.offheapVertices) gOffI
    , bench "lengthOfBS" $ nf (lengthOf OffBS.offheapVertices) gOffBS
    ]
  where
    setupEnv = do
      pathBe <- getDataFileName "shumov/graph.bin"
      gBe <- readShumovBE pathBe
      pathLe <- getDataFileName "shumov/graph_le.bin"
      gLe <- readShumovLE pathLe
      gOff <- Off.readShumov pathLe
      gOffI <- OffI.readShumov pathLe
      gOffBS <- OffBS.readShumov pathLe
      return (gBe, gLe, gOff, gOffI, gOffBS)

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