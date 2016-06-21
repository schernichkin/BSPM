{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lev.GetI
  ( FixedGetter(..)
  , prim
  , int16Host
  , int32Host
  , int64Host
  , runFixed
  , runFixedBuffer
  ) where

import           Control.Monad
import           Control.Monad.Indexed
import           Control.Monad.Primitive
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           Foreign.ForeignPtr
import           GHC.Ptr
import           GHC.TypeLits
import           Lev.Buffer

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

data Getter a = GetterDone    !Int !a
                -- ^ bytes read total and result
              | GetterPartial !Int !(Buffer -> Getter a)
                -- ^ minimal buffer size and next step

instance Functor Getter where
  fmap f (GetterDone n a) = GetterDone n $ f a

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Get." ++ fun ++ ':':' ':msg

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

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

runFixed :: FixedGetter 0 n a -> Addr -> a
runFixed g = unFixedGetter g
{-# INLINE runFixed #-}

checkBufferLength :: Int -> Int -> a -> a
checkBufferLength required remains r =
  if required <= remains
    then r
    else moduleError "checkBufferLength"
                   ( "Bytes remains = " ++ (show remains) ++
                     ", bytes required = " ++ (show required) )
{-# INLINE checkBufferLength #-}

runFixedBuffer :: forall n a . ( KnownNat n )
               => FixedGetter 0 n a -> Buffer -> (a, Buffer)
runFixedBuffer g Buffer {..} =
    checkBufferLength getterLength _length
  $ unsafeInlineIO
  $ withForeignPtr _base $ \(Ptr addr) -> return
    ( unFixedGetter g (Addr addr)
    , Buffer _base (_offset + getterLength) (_length - getterLength)
    )
  where
    getterLength = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE runFixedBuffer #-}
