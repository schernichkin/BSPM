module Data.GrowingArray.Unboxed
  ( GrowingArray
  , insert
  , new
  , newSingleton
  , unsafeFreeze
  ) where

import           Control.Monad.Primitive
import qualified Data.GrowingArray.Generic   as G
import           Data.Vector.Unboxed         (Vector)
import           Data.Vector.Unboxed.Mutable (MVector, Unbox)

type GrowingArray a = G.GrowingArray (MVector (PrimState IO)) a

new :: Unbox a => Int -> IO (GrowingArray a)
new = G.new

newSingleton :: Unbox a => a -> IO (GrowingArray a)
newSingleton = G.newSingleton

insert :: Unbox a => GrowingArray a -> a -> IO (GrowingArray a)
insert = G.insert

unsafeFreeze :: Unbox a => GrowingArray a -> IO (Vector a)
unsafeFreeze = G.unsafeFreeze
