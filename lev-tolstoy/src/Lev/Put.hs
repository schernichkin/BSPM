{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Lev.Put
  ( PutFixed
  , unPutFixed -- TODO: remove
  , prim
  , int16Host
  , int32Host
  , int64Host
  ) where

import           Control.Monad.Primitive
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           GHC.TypeLits
import           Lev.Layout

newtype PutFixed (i :: Nat) (o :: Nat) m a = PutFixed { unPutFixed :: Addr -> a -> m () }

prim :: forall i m a . ( KnownNat i, PrimMonad m, Prim a )
     => PutFixed i (i + SizeOf a) m a
prim = PutFixed $ \addr x -> writeOffAddr (plusAddr addr $ fromIntegral $ natVal (Proxy :: Proxy i)) 0 x
{-# INLINE prim #-}

int16Host :: forall i m . ( KnownNat i, PrimMonad m )
          => PutFixed i (i + SizeOf Int16) m Int16
int16Host = prim
{-# INLINE int16Host #-}

int32Host :: forall i m . ( KnownNat i, PrimMonad m )
          => PutFixed i (i + SizeOf Int32) m Int32
int32Host = prim
{-# INLINE int32Host #-}

int64Host :: forall i m . ( KnownNat i, PrimMonad m )
          => PutFixed i (i + SizeOf Int64) m Int64
int64Host = prim
{-# INLINE int64Host #-}
