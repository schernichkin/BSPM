{-# LANGUAGE ScopedTypeVariables #-}

module Bench where

import           BSPM.Engine.Local
import           BSPM.Engine.Local.WorkerSet
import qualified BSPM.StateStream as SS
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

forkAndWait :: Int -> IO ()
forkAndWait c = do
  cd <- newCountDown c
  replicateM_ c $ forkIO $ decCountDown cd
  waitCountDown cd

spawnAndWait :: Int -> IO ()
spawnAndWait c = do
  cd <- newCountDown c
  run SS.unit $ replicateM_ c $ spawn $ liftIO $ decCountDown cd
  waitCountDown cd

spawnSendRecieveWait  :: Int -> IO ()
spawnSendRecieveWait c = do
  cd <- newCountDown c
  run SS.unit $ replicateM_ c $ do
    worker <- spawn (receive >> liftIO (decCountDown cd))
    send worker ()
  waitCountDown cd

sendToRecieveWait :: Int -> IO ()
sendToRecieveWait c = do
  cd <- newCountDown c
  runW SS.unit (const $ receive >> liftIO (decCountDown cd)) $
    forM_  [1..c] $ flip sendTo ()
  waitCountDown cd

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
      [ bench "fork and wait 10 000" $ nfIO $ forkAndWait 10000
      -- , bench "fork and wait 1 000 000" $ nfIO $ forkAndWait 1000000
      ]
    , bgroup "hashtable"
      [ bench "Basic 10 000 inserts" $ nfIO $ populateHashtable (Proxy :: Proxy Basic.HashTable) 10000
      , bench "Cuckoo 10 000 inserts" $ nfIO $ populateHashtable (Proxy :: Proxy Cuckoo.HashTable) 10000
      ]
    ]
  , bgroup "BSPM.Engine.Local"
    [ bgroup "spawn"
      [ bench "spawn and wait 10 000" $ nfIO $ spawnAndWait 10000
      ]
    , bgroup "send"
      [ bench "spawn and send 10 000" $ nfIO $ spawnSendRecieveWait 10000
      , bench "send to 10 000" $ nfIO $ sendToRecieveWait 10000
      ]
    ]
  , bgroup "Data.Graph.Unboxed.Reader.WikiVote"
    [ bench "readGraph" $ nfIO $ readWikiVote
    ]
  ]
