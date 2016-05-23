{-# LANGUAGE RecordWildCards #-}

module Data.GrowingArray.Generic
  ( GrowingArray
  , new
  , newSingleton
  , insert
  , unsafeFreeze
  ) where

import           Control.Monad.Primitive
import           Data.Vector.Generic (Vector, Mutable)
import qualified Data.Vector.Generic         as G
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as V

data GrowingArray v a = GrowingArray
  { _items :: !(v a)
  , _count :: !Int
  }

{-# INLINE new #-}
new :: (PrimMonad m, MVector v a) => Int -> m (GrowingArray (v (PrimState m)) a)
new c = do
  items <- V.new c
  return GrowingArray
    { _items = items
    , _count = 0
    }

{-# INLINE newSingleton #-}
newSingleton :: (PrimMonad m, MVector v a) => a -> m (GrowingArray (v (PrimState m)) a)
newSingleton e = do
  GrowingArray {..} <- new 1
  V.unsafeWrite _items 0 e
  return GrowingArray
    { _items = _items
    , _count = 1
    }

{-# INLINE insert #-}
insert :: (PrimMonad m, MVector v a)
       => GrowingArray (v (PrimState m)) a
       -> a
       -> m (GrowingArray (v (PrimState m)) a)
insert GrowingArray{..} e = do
  edges <- case V.length _items of
    0 -> V.unsafeGrow _items 1
    l | l /= _count -> return _items
      | otherwise   -> V.unsafeGrow _items (_count * 2)
  V.unsafeWrite edges _count e
  return GrowingArray
    { _items = edges
    , _count = _count + 1
    }

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, Vector v a)
             => GrowingArray (Mutable v (PrimState m)) a -> m (v a)
unsafeFreeze GrowingArray{..} = G.unsafeFreeze $ V.unsafeTake _count _items
