{-# LANGUAGE ScopedTypeVariables #-}

module Bench where

import           BSPM.Engine.Local
import           BSPM.Util.CountDown
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
import           Data.Proxy
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Paths_BSPM
import           System.IO

forkBench :: Int -> IO ()
forkBench c = do
  cd <- newCountDown c
  replicateM_ c $ forkIO $ decCountDown cd
  waitCountDown cd

pokeTVar :: Int -> IO ()
pokeTVar c = do
  counter <- newTVarIO c
  void $ forkIO (worker counter)
  void $ forkIO (worker counter)
  atomically $ do
    a <- readTVar counter
    unless (a == 0) retry
  where
    worker counter = do
      a' <- atomically $ do
        a <- readTVar counter
        if a > 0 then do
          writeTVar counter (a - 1)
          return (a - 1)
        else
          return a
      unless (a' == 0) (worker counter)

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

readAlignedBench ::  Int -> IO ()
readAlignedBench c = allocaBytes (c * 8) $ \ptr ->
  forM_ [0, 8 .. c-1] $ \off -> peek (ptr `plusPtr` off :: Ptr Word64)

readUnalignedBench ::  Int -> IO ()
readUnalignedBench c = allocaBytes (c * 8 + 1) $ \ptr ->
  forM_ [1, 9 .. c] $ \off -> peek (ptr `plusPtr` off :: Ptr Word64)

main :: IO ()
main = defaultMain
  [ bgroup "Baseline"
    [ bgroup "memory"
      [ bench "read aligned 8M" $ nfIO $ readAlignedBench 1000000
      , bench "read unaligned 8M" $ nfIO $ readUnalignedBench 1000000
      , bench "read aligned 80M" $ nfIO $ readAlignedBench 10000000
      , bench "read unaligned 80M" $ nfIO $ readUnalignedBench 10000000
      ]
    , bgroup "forkIO"
      [ bench "fork and wait 10 000" $ nfIO $ forkBench 10000
      -- , bench "fork and wait 1 000 000" $ nfIO $ forkAndWait 1000000
      ]
    , bgroup "STM"
      [  bench "Poke TVar 10 000" $ nfIO $ pokeTVar 10000
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
