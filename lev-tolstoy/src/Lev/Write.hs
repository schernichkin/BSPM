{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lev.Write where

import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Primitive.Addr
import           GHC.TypeLits
import           Lev.Layout              hiding (Layout (..))

data Layout = DynamicLayout | StaticLayout Nat Nat

data Writer m (l :: Layout) a where
   StaticWriter  :: (a -> m ()) -> Writer m ('StaticLayout o s) a

staticBind :: forall m o sa sb a b . ( Monad m )
          => (a -> Writer m ('StaticLayout (o + sa) sb) b)
          -> Writer m ('StaticLayout o sa) a
          -> Writer m ('StaticLayout o (sa + sb)) b
staticBind k (StaticWriter f) = undefined
{-# INLINE staticBind #-}

staticReturn :: forall m o a . ( Applicative m )
             => a -> Writer m ('StaticLayout o 0) a
staticReturn = undefined
{-# INLINE staticReturn #-}

prim :: forall m (o :: Nat) a . ( PrimMonad m, Prim a, KnownNat o )
    => Writer m ('StaticLayout o (SizeOf a)) a
prim = StaticWriter $ undefined
 where
  -- off = fromIntegral $ natVal (Proxy :: Proxy o)
{-# INLINE prim #-}
