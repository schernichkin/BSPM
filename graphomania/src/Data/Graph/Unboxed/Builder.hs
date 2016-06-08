module Data.Graph.Unboxed.Builder
  ( UGraphBuilder ()
  , addEdge
  , addEdges
  , build
  , buildSharded
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Data.Graph.Unboxed.Internal
import           Data.GrowingArray.Generic
import           Data.Hashable
import qualified Data.HashTable.IO            as H
import           Data.Shards.Ordered.Internal
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro
import           Data.Vector.Unboxed          ()
import           Data.Vector.Unboxed.Mutable  (Unbox)
import qualified Data.Vector.Unboxed.Mutable  as UMV
import           Data.Vector.Utils

type MUEdges e = GrowingArray (PrimState IO) UMV.MVector e
type MUGraph v e = HashTable v (MUEdges e)

newtype UGraphBuilder v e a = UGraphBuilder { unUGraphBuilder :: MUGraph v e -> IO a }

instance Functor (UGraphBuilder v e) where
  fmap = liftM

instance Applicative (UGraphBuilder v e) where
  pure = return
  (<*>) = ap

instance MonadIO (UGraphBuilder a s) where
  liftIO = UGraphBuilder . const

instance Monad (UGraphBuilder v e) where
  return = UGraphBuilder . const . return
  a >>= f = UGraphBuilder $ \g -> unUGraphBuilder a g >>= flip unUGraphBuilder g . f

addEdge :: ( Eq v, Hashable v, Unbox e ) => v -> e -> UGraphBuilder v e ()
addEdge v e = UGraphBuilder $ \g -> do
  maybeEdges <- H.lookup g v
  case maybeEdges of
    Just edges -> insert edges e
    Nothing -> newSingleton e >>= H.insert g v

addEdges :: ( Eq v, Hashable v, Unbox e, Foldable t )
         => v -> t e -> UGraphBuilder v e ()
addEdges v = mapM_ (addEdge v)

build :: ( Eq v, Hashable v, Unbox e )
      => UGraphBuilder v e ()
      -> IO (UGraph v e)
build builder = do
  src <- H.new
  unUGraphBuilder builder src
  dst <- H.new
  H.mapM_ ( \(v, eee) -> do
    edgs <- unsafeFreeze eee
    H.insert dst v edgs ) src
  return $ UGraph dst

toVector :: HashTable k e -> IO (V.Vector (k, e))
toVector h = do
  res <- new 1
  H.mapM_ (insert res) h
  unsafeFreeze res

fromVector :: (Eq v, Hashable v, Unbox e)
           => V.Vector (v, MUEdges e)
           -> IO (UGraph v e)
fromVector src = do
  dst <- H.newSized (V.length src)
  V.mapM_ ( \(v, e) -> do
    edgs <- unsafeFreeze e
    H.insert dst v edgs ) src
  return $ UGraph dst

buildSharded :: ( Ord v, Hashable v, Unbox e )
             => Int
             -> UGraphBuilder v e ()
             -> IO (OrderedShardMap v (UGraph v e))
buildSharded n builder = do
  src <- H.new
  unUGraphBuilder builder src
  mutable <- V.unsafeThaw =<< toVector src
  sortBy (\(k1, _) (k2, _) -> compare k1 k2) mutable
  shards <- salami n <$> V.unsafeFreeze mutable
  vvv <- forM shards $ \shardV -> do
    let (maxV, _) = V.last shardV
    shard <- fromVector shardV
    return $ OrderedShardEntry maxV shard
  let kkk = fromOrderedList vvv
  return kkk
