{-# LANGUAGE TypeFamilies #-}

module Data.Shards.Ordered () where


import           Data.Key
import           Data.Shards.Ordered.Internal
import           Data.Vector.Generic          (Vector)
import qualified Data.Vector.Generic          as V

-- type instance Key (OrdShards v k) = k



-- instance Lookup (OrdShards v k) where

-- Search immutable vectors
