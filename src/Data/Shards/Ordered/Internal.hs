{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Data.Shards.Ordered.Internal
  ( OrderedShardEntry (..)
  , OrderedShardMap (..)
  , fromOrderedList
  , getShard
  ) where

import           Data.Key
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as G
import           Data.Vector.Utils

data OrderedShardEntry k s = OrderedShardEntry
  { _maxKey :: !k
  , _shard  :: !s
  } deriving ( Show )

instance Functor (OrderedShardEntry k) where
  fmap f OrderedShardEntry{..} = OrderedShardEntry
    { _maxKey = _maxKey
    , _shard = f _shard
    }

type instance Key (OrderedShardEntry k) = k

data OrderedShardMap k s = OrderedShardMap
  { _shards :: !(V.Vector (OrderedShardEntry k s))
  } deriving ( Show )

type instance Key (OrderedShardMap k) = k

instance Ord k => Lookup (OrderedShardMap k) where
  lookup k f = Just $ getShard f k

instance Ord k => Indexable (OrderedShardMap k) where
  index = getShard

instance Foldable (OrderedShardMap k) where
  foldr f b = foldr (f . _shard) b . _shards
  length = V.length . _shards

instance Functor (OrderedShardMap k) where
  fmap f = OrderedShardMap . fmap (fmap f) . _shards

instance Traversable (OrderedShardMap k) where
  traverse f = let g e = OrderedShardEntry (_maxKey e) <$> f (_shard e)
               in fmap OrderedShardMap . traverse g . _shards

{-# INLINE getShard #-}
getShard :: Ord k => OrderedShardMap k s -> k -> s
getShard f k = let v = _shards f
                   i = binarySearchByKey _maxKey v k
                   l = G.length v - 1
               in _shard $ G.unsafeIndex v (min i l)

{-# INLINE fromOrderedList #-}
fromOrderedList :: [OrderedShardEntry k s] -> OrderedShardMap k s
fromOrderedList = OrderedShardMap . G.fromList
