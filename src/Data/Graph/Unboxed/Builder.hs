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
import           Data.IORef
import           Data.Shards.Ordered.Internal
import qualified Data.Vector                  as V
import           Data.Vector.Algorithms.Intro
import qualified Data.Vector.Mutable          as MV
import           Data.Vector.Unboxed.Mutable  (Unbox)
import qualified Data.Vector.Unboxed.Mutable  as UMV

type MUGraph v e = HashTable v (IORef (GrowingArray (UMV.MVector (PrimState IO)) e))

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
    Just edges -> readIORef edges >>= flip insert e >>= writeIORef edges
    Nothing -> newSingleton e >>= newIORef >>= H.insert g v

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
  H.mapM_ ( \(v, e) -> do
    edges <- readIORef e >>= unsafeFreeze
    H.insert dst v edges ) src
  return $ UGraph dst

toVector :: HashTable k e -> IO (V.Vector e)
toVector h = do
  res <- new 1
  H.mapM_ ( \(_, v) -> insert res v) h
  unsafeFreeze res

buildSharded :: ( Ord v, Hashable v, Unbox e )
             => UGraphBuilder v e ()
             -> Int
             -> IO (OrderedShardMap v (UGraph v e))
buildSharded builder maxShards = do
  src <- H.new
  unUGraphBuilder builder src
  vertices <- toVector src >>= V.unsafeThaw
  let numShards = min maxShards $ MV.length vertices
  -- shardes
  -- kkk <- V.generate (min )
      -- теперь этот вектор нужно мапнуть и поделить на
      -- шарды
  return undefined
