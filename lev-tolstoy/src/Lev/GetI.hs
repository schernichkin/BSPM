{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lev.GetI
  ( FixedGetter(..)
  , prim
  , int16Host
  , int32Host
  , int64Host
  ) where

import           Control.Monad
import           Control.Monad.Indexed
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           GHC.TypeLits

newtype FixedGetter (i :: Nat) (o :: Nat) a = FixedGetter { unFixedGetter :: Addr -> a }

instance IxFunctor FixedGetter where
  imap f = FixedGetter . fmap f . unFixedGetter

instance IxPointed FixedGetter where
  ireturn = FixedGetter . const

instance IxApplicative FixedGetter where
  f `iap` g = FixedGetter $ unFixedGetter f `ap` unFixedGetter g

instance IxMonad FixedGetter where
  k `ibind` m = FixedGetter $ flip (unFixedGetter . k) `ap` unFixedGetter m

type family SizeOf a :: Nat
type instance SizeOf Int16 = 2
type instance SizeOf Int32 = 4
type instance SizeOf Int64 = 8

prim :: forall i a . ( KnownNat i, Prim a ) => FixedGetter i (i + SizeOf a) a
prim = FixedGetter $ \addr -> indexOffAddr (plusAddr addr $ fromIntegral $ natVal (Proxy :: Proxy i)) 0
{-# INLINE prim #-}

int16Host :: forall i . (KnownNat i) => FixedGetter i (i + SizeOf Int16) Int16
int16Host = prim
{-# INLINE int16Host #-}

int32Host :: forall i . (KnownNat i) => FixedGetter i (i + SizeOf Int32) Int32
int32Host = prim
{-# INLINE int32Host #-}

int64Host :: forall i . (KnownNat i) => FixedGetter i (i + SizeOf Int64) Int64
int64Host = prim
{-# INLINE int64Host #-}
