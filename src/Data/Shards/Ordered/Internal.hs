{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Shards.Ordered.Internal
  ( OrderedShardEntry (..)
  , OrderedShardMap (..)
  ) where

import           Data.Bits
import           Data.Key
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

data OrderedShardEntry k s = OrderedShardEntry
  { _maxKey :: !k
  , _shard  :: !s
  }

type instance Key (OrderedShardEntry k) = k

data OrderedShardMap k s = OrderedShardMap
  { _shards :: !(V.Vector (OrderedShardEntry k s))
  }

type instance Key (OrderedShardMap k) = k

instance Ord k => Lookup (OrderedShardMap k) where
  lookup k f = Just $ getShard f k

instance Ord k => Indexable (OrderedShardMap k) where
  index = getShard

{-# INLINE getShard #-}
getShard :: Ord k => OrderedShardMap k s -> k -> s
getShard f k = let v = _shards f
                   i = binarySearchByKey _maxKey v k
                   l = G.length v - 1
               in _shard $ G.unsafeIndex v (min i l)

{-# INLINE binarySearchByKey #-}
binarySearchByKey :: ( G.Vector v (e k a)
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
