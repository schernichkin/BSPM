{-# LANGUAGE RecordWildCards #-}

module Data.GrowingArray.Generic
  ( GrowingArray
  , new
  , newSingleton
  , insert
  , unsafeFreeze
  ) where

import           Control.Monad.Primitive
import Data.Primitive.MutVar
import           Data.Vector.Generic (Vector, Mutable)
import qualified Data.Vector.Generic         as V
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

data GrowingArray s v a = GrowingArray
  { _items :: !(MutVar s (v s a))
  , _count :: !(MutVar s Int)
  }

{-# INLINE new #-}
new :: (PrimMonad m, MVector v a)
    => Int -> m (GrowingArray (PrimState m) v a)
new c = do
  itemsVar <- MV.new c >>= newMutVar
  countVar <- newMutVar 0
  return GrowingArray
    { _items = itemsVar
    , _count = countVar
    }

{-# INLINE newSingleton #-}
newSingleton :: (PrimMonad m, MVector v a)
             => a -> m (GrowingArray (PrimState m) v a)
newSingleton e = do
  items <- MV.new 1
  itemsVar <- newMutVar items
  MV.unsafeWrite items 0 e
  countVar <- newMutVar 1
  return GrowingArray
    { _items = itemsVar
    , _count = countVar
    }

{-# INLINE insert #-}
insert :: (PrimMonad m, MVector v a)
       => GrowingArray (PrimState m) v a
       -> a
       -> m ()
insert GrowingArray{..} e = do
  count <- readMutVar _count
  items <- do
    currentItems <- readMutVar _items
    let growBy factor = do
          newItems <- MV.unsafeGrow currentItems factor
          writeMutVar _items newItems
          return newItems
    case MV.length currentItems of
      0 -> growBy 1
      l | l /= count -> return currentItems
        | otherwise  -> growBy (count * 2)
  MV.unsafeWrite items count e
  writeMutVar _count (count + 1)

unsafeFreeze :: (PrimMonad m, Vector v a)
             => GrowingArray (PrimState m) (Mutable v) a -> m (v a)
unsafeFreeze GrowingArray{..} = do
  count <- readMutVar _count
  currentItems <- readMutVar _items
  V.unsafeFreeze $ MV.unsafeTake count currentItems
