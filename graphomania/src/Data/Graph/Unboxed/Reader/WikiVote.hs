module Data.Graph.Unboxed.Reader.WikiVote
  ( readGraph
  , readSharded
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Char8      as BS
import           Data.Graph.Unboxed
import           Data.Graph.Unboxed.Builder
import           Data.Maybe
import           Data.Shards.Ordered
import           System.IO                  (Handle, hIsEOF)

readGraph :: Handle -> IO (UGraph Int (Int, Double))
readGraph h = build go
  where
    go = do
      eof <- liftIO $ hIsEOF h
      unless eof $ do
        line <- liftIO $ hGetLine h
        when (BS.head line /= '#') $ do
          let s : t : _ = BS.words line
          addEdge (fst $ fromJust $ readInt s) (fst $ fromJust $ readInt t, 1)
        go

readSharded :: Int -> Handle -> IO (OrderedShardMap Int (UGraph Int (Int, Double)))
readSharded n h = buildSharded n go
  where
    go = do
      eof <- liftIO $ hIsEOF h
      unless eof $ do
        line <- liftIO $ hGetLine h
        when (BS.head line /= '#') $ do
          let s : t : _ = BS.words line
          addEdge (fst $ fromJust $ readInt s) (fst $ fromJust $ readInt t, 1::Double)
        go
