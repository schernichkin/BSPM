{-# LANGUAGE TypeFamilies #-}

module Data.Graph.Unboxed.Internal
  ( UGraph (..)
  , HashTable
  ) where

import           Data.Graph.Class
import           Data.Hashable
import qualified Data.HashTable.IO as H
import           Data.Maybe
import           Data.Vector.Unboxed as V
import           System.IO.Unsafe

type HashTable k v = H.BasicHashTable k v

newtype UGraph v e = UGraph { unUGraph :: HashTable v (Vector e) } deriving ( Show )

instance ( Eq v, Hashable v ) => Graph (UGraph v e) where
  type Vertex (UGraph v e) = v
  type Edges (UGraph v e) = Vector
  type Edge (UGraph v e) = e

  edges g = fromJust . unsafePerformIO . H.lookup (unUGraph g)
