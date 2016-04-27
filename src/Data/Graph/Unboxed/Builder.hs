{-# LANGUAGE RecordWildCards #-}

module Data.Graph.Unboxed.Builder
  ( UGraphBuilder ()
  , addEdge
  , addEdges
  , build
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Graph.Unboxed.Internal
import           Data.IORef
import           Data.Hashable
import qualified Data.HashTable.IO as H
import qualified Data.Vector.Unboxed  as V ( unsafeFreeze )
import           Data.Vector.Unboxed.Mutable ( MVector, Unbox )
import qualified Data.Vector.Unboxed.Mutable as V

data EdgeArray a = EdgeArray
  { _edges :: !(MVector (PrimState IO) a)
  , _count :: !Int
  }

new :: ( Unbox a ) => a -> IO (EdgeArray a)
new e = do
  edges <- V.new 1
  V.unsafeWrite edges 0 e
  return EdgeArray
    { _edges = edges
    , _count = 1
    }

insert :: ( Unbox a ) => EdgeArray a -> a -> IO (EdgeArray a)
insert EdgeArray{..} e = do
  edges <- if (V.length _edges /= _count)
    then return _edges
    else V.unsafeGrow _edges (_count * 2)
  V.unsafeWrite edges _count e
  return EdgeArray
    { _edges = edges
    , _count = _count + 1
    }

type MUGraph v e = HashTable v (IORef (EdgeArray e))

newtype UGraphBuilder v e a = UGraphBuilder { unUGraphBuilder :: MUGraph v e -> IO a }

instance Functor (UGraphBuilder v e) where
  fmap = liftM

instance Applicative (UGraphBuilder v e) where
  pure = return
  (<*>) = ap

instance Monad (UGraphBuilder v e) where
  return = UGraphBuilder . const . return
  a >>= f = UGraphBuilder $ \g -> unUGraphBuilder a g >>= flip unUGraphBuilder g . f

addEdge :: ( Eq v, Hashable v, Unbox e ) => v -> e -> UGraphBuilder v e ()
addEdge v e = UGraphBuilder $ \g -> do
  maybeEdges <- H.lookup g v
  case maybeEdges of
    Just edges -> readIORef edges >>= flip insert e >>= writeIORef edges
    Nothing -> new e >>= newIORef >>= H.insert g v

addEdges :: ( Eq v, Hashable v, Unbox e, Foldable t )
         => v -> t e -> UGraphBuilder v e ()
addEdges v = mapM_ (addEdge v)

build :: ( Eq v, Hashable v, Unbox e ) => UGraphBuilder v e () -> IO (UGraph v e)
build builder = do
  src <- H.new
  unUGraphBuilder builder src
  dst <- H.new
  H.mapM_ ( \(v, e) -> do
    EdgeArray{..} <- readIORef e
    edges <- V.unsafeFreeze $ V.unsafeTake _count _edges
    H.insert dst v edges ) src
  return $ UGraph dst
