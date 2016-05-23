{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Utils
  ( binarySearchByKey
  , salami
  ) where

import           Data.Bits
import           Data.Key
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G
import Data.Function

{-# INLINE binarySearchByKey #-}
binarySearchByKey :: ( Vector v (e k a)
                     , Key (e k) ~ k
                     , Ord k )
                     => (e k a -> k)
                     -> v (e k a)
                     -> k
                     -> Int
binarySearchByKey getKey v k = go 0 (G.length v)
  where
    go !l !r
      | r <= l = l
      | otherwise = case getKey (G.unsafeIndex v c) `compare` k of
                      LT -> go (c + 1) r
                      EQ -> c
                      GT -> go l c
      where c = (r + l) `shiftR` 1

salami :: (Vector v e) => Int -> v e -> [v e]
salami n v | len <= n = elementwise 0
           | otherwise = slicewise 0 1
  where
    elementwise i | i == len = []
                  | otherwise = G.unsafeSlice i 1 v : elementwise (i + 1)

    slicewise l i | i == n = [G.unsafeSlice l (len - l) v]
                  | otherwise = let r = floor $ sliceSize * fromIntegral i
                                in G.unsafeSlice l (r - l) v : slicewise r (i + 1)

    len = G.length v

    sliceSize = on (/) fromIntegral len n :: Double
