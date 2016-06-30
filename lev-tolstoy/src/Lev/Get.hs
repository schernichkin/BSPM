{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lev.Get
  ( GetFixed
  , prim
  , int16Host
  , int32Host
  , int64Host
  , int16LE
  , int32LE
  , int64LE
  , int16BE
  , int32BE
  , int64BE
  , runFixed
  , Get
  , fixed
  , byteString
  , run
  ) where

import           Control.Monad
import           Control.Monad.Indexed
import           Control.Monad.Primitive
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           GHC.Ptr
import           GHC.TypeLits
import           System.Endian

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

int16LE :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int16) Int16
int16LE = (fromIntegral . fromLE16 . fromIntegral :: Int16 -> Int16) `imap` prim
{-# INLINE int16LE #-}

int32LE :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int32) Int32
int32LE = (fromIntegral . fromLE32 . fromIntegral :: Int32 -> Int32) `imap` prim
{-# INLINE int32LE #-}

int64LE :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int64) Int64
int64LE = (fromIntegral . fromLE64 . fromIntegral :: Int64 -> Int64) `imap` prim
{-# INLINE int64LE #-}

int16BE :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int16) Int16
int16BE = (fromIntegral . fromBE16 . fromIntegral :: Int16 -> Int16) `imap` prim
{-# INLINE int16BE #-}

int32BE :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int32) Int32
int32BE = (fromIntegral . fromBE32 . fromIntegral :: Int32 -> Int32) `imap` prim
{-# INLINE int32BE #-}

int64BE :: forall i . (KnownNat i) => GetFixed i (i + SizeOf Int64) Int64
int64BE = (fromIntegral . fromBE64 . fromIntegral :: Int64 -> Int64) `imap` prim
{-# INLINE int64BE #-}

runFixed :: forall n a . ( KnownNat n )
               => GetFixed 0 n a -> ByteString -> (a, ByteString)
runFixed g b =
    checkBufferLength getterLength bufferLength
  $ unsafeInlineIO
  $ withForeignPtr base $ \(Ptr addr) -> return
    ( unGetFixed g $ plusAddr (Addr addr) offset
    , fromForeignPtr base (offset + getterLength) (bufferLength - getterLength)
    )
  where
    (base, offset, bufferLength) = toForeignPtr b
    getterLength = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE runFixed #-}

-- * Generic getter

type State = ( ForeignPtr Word8, Addr, Int )

data Result a = Done  !a
              | Fetch !Addr !Int !(State -> Result a)

type Cont a r = State -> a -> Result r

newtype Get a = Get { unGet :: forall r . State -> Cont a r -> Result r }

instance Functor Get where
  fmap f g = Get $ \s k -> unGet g s $ \s' -> k s' . f

instance Applicative Get where
  pure x  = Get $ \s k -> k s x
  f <*> g = Get $ \s k -> unGet f s $ \s' f' -> unGet (f' <$> g) s' k

instance Monad Get where
  f >>= g = Get $ \s k -> unGet f s $ \s' a -> unGet (g a) s' k

checked :: Int -> (Addr -> a) -> Get a
checked len get = Get $ \(base, addr, remains) k ->
  if len <= remains
    then k (base, plusAddr addr len, remains - len) (get addr)
    else Fetch addr len $ \(base', addr', remains') -> k (base', plusAddr addr' len, remains' - len) (get addr)
{-# INLINE checked #-}

fixed :: forall n a . ( KnownNat n )
      => GetFixed 0 n a -> Get a
fixed = checked (fromIntegral $ natVal (Proxy :: Proxy n)) . unGetFixed
{-# INLINE fixed #-}

byteString :: Int -> Get ByteString
byteString len = Get $ \(base, addr, remains) k ->
  checkBufferLength len remains $
    let !(Ptr baseAddr) = unsafeForeignPtrToPtr base
    in k (base, plusAddr addr len, remains - len) (fromForeignPtr base (minusAddr addr (Addr baseAddr)) len)

run :: Get a -> ByteString -> (a, ByteString)
run g b = unsafeInlineIO $ withForeignPtr base $ \(Ptr addr) -> do
  let !res = unGet g (base, plusAddr (Addr addr) offset, bufferLen)
              $ \(base', addr', remains) a -> Done (a, fromForeignPtr base' (minusAddr addr' (Addr addr)) remains)
  case res of
    Done a -> return a
    Fetch _ _ _ -> error "fetch."
  where
    (base, offset, bufferLen) = toForeignPtr b
{-# INLINE run #-}
