{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE BangPatterns       #-}

module Lev.Get
  ( GetFixed(..)
  , prim
  , int16Host
  , int32Host
  , int64Host
  , runGetFixed
  , Get
  , fixed
  , runGet
  ) where

import           Control.Monad
import           Control.Monad.Indexed
import           Control.Monad.Primitive
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           Foreign.ForeignPtr
import           GHC.Ptr
import           GHC.TypeLits

-- * Utility functions

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Get." ++ fun ++ ':':' ':msg

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

bufferLengthError :: Int -> Int -> a
bufferLengthError required remains =
  moduleError "checkBufferLength" $
              "Bytes remains = " ++ (show remains) ++
              ", bytes required = " ++ (show required)
{-# NOINLINE bufferLengthError #-}

checkBufferLength :: Int -> Int -> a -> a
checkBufferLength required remains r =
  if required <= remains
    then r
    else bufferLengthError required remains
{-# INLINE checkBufferLength #-}

-- * Fixed size getter

newtype GetFixed (i :: Nat) (o :: Nat) a = GetFixed { unGetFixed :: Addr -> a }

instance IxFunctor GetFixed where
  imap f = GetFixed . fmap f . unGetFixed

instance IxPointed GetFixed where
  ireturn = GetFixed . const

instance IxApplicative GetFixed where
  f `iap` g = GetFixed $ unGetFixed f `ap` unGetFixed g

instance IxMonad GetFixed where
  k `ibind` m = GetFixed $ flip (unGetFixed . k) `ap` unGetFixed m

type family SizeOf a :: Nat
type instance SizeOf Int16 = 2
type instance SizeOf Int32 = 4
type instance SizeOf Int64 = 8

prim :: forall i a . ( KnownNat i, Prim a ) => GetFixed i (i + SizeOf a) a
prim = GetFixed $ \addr -> indexOffAddr (plusAddr addr $ fromIntegral $ natVal (Proxy :: Proxy i)) 0
{-# INLINE prim #-}

int16Host :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int16) Int16
int16Host = prim
{-# INLINE int16Host #-}

int32Host :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int32) Int32
int32Host = prim
{-# INLINE int32Host #-}

int64Host :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int64) Int64
int64Host = prim
{-# INLINE int64Host #-}

runGetFixed :: forall n a . ( KnownNat n )
               => GetFixed 0 n a -> ByteString -> (a, ByteString)
runGetFixed g b =
    checkBufferLength getterLength bufferLength
  $ unsafeInlineIO
  $ withForeignPtr base $ \(Ptr addr) -> return
    ( unGetFixed g $ plusAddr (Addr addr) offset
    , fromForeignPtr base (offset + getterLength) (bufferLength - getterLength)
    )
  where
    (base, offset, bufferLength) = toForeignPtr b
    getterLength = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE runGetFixed #-}

-- * Generic getter

type GetState = ( Addr, Int )
type GetCont a r = GetState -> a -> r

newtype Get a = Get { unGet :: forall r . GetState -> GetCont a r -> r }

fixed :: forall n a . ( KnownNat n )
      => GetFixed 0 n a -> Get a
fixed g = Get $ \(addr, remains) k ->
  checkBufferLength len remains $
   k (plusAddr addr len, remains - len) (unGetFixed g addr)
  where
    len = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE fixed #-}

runGet :: Get a -> ByteString -> (a, ByteString)
runGet g b = unsafeInlineIO $ withForeignPtr base $ \(Ptr addr) -> do
  let !res = unGet g (plusAddr (Addr addr) offset, bufferLen) $ \(addr', remains) a ->
              (a, fromForeignPtr base (minusAddr addr' (Addr addr)) remains)
  return res
  where
    (base, offset, bufferLen) = toForeignPtr b
{-# INLINE runGet #-}
