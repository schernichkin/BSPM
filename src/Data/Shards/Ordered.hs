{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Data.Shards.Ordered () where

import           Data.Key
import   Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic                   as V
import Data.Bits
import           Data.Vector.Algorithms.Search (Comparison )

data ShardEntry  k s  = ShardEntry
  { _from  :: !k
  , _shard :: !s
  }

data OrdShards v k s = OrdShards
  { _shards :: v
  }

type instance Key (OrdShards v k) = k



instance Lookup (OrdShards v k) where

-- Search immutable vectors

{-# INLINE binarySearch #-}
binarySearch :: (Vector v e, Ord e)
             => v e
             -> e
             -> Int
binarySearch = binarySearchBy compare

{-# INLINE binarySearchBy #-}
binarySearchBy :: (Vector v e)
               => Comparison e
               -> v e
               -> e
               -> Int
binarySearchBy cmp vec e = binarySearchByBounds cmp vec e 0 (V.length vec)

{-# INLINE binarySearchByBounds #-}
binarySearchByBounds :: (Vector v e)
                     => Comparison e
                     -> v e
                     -> e
                     -> Int
                     -> Int
                     -> Int
binarySearchByBounds cmp vec e = loop
  where
    loop !l !u
      | u <= l    = l
      | otherwise = case V.unsafeIndex vec k `cmp` e of
                      LT -> loop (k + 1) u
                      EQ -> k
                      GT -> loop l k
      where k = (u + l) `shiftR` 1
