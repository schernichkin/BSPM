{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
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
  , runFixedGetter
  , fixed
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
import           GHC.Ptr
import           GHC.TypeLits

-- * Utility functions

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Get." ++ fun ++ ':':' ':msg

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

checkBufferLength :: Int -> Int -> a -> a
checkBufferLength required remains r =
  if required <= remains
    then r
    else moduleError "checkBufferLength"
                   ( "Bytes remains = " ++ (show remains) ++
                     ", bytes required = " ++ (show required) )
{-# INLINE checkBufferLength #-}

-- * Fixed size getter

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

runFixedGetter :: forall n a . ( KnownNat n )
               => FixedGetter 0 n a -> ByteString -> (a, ByteString)
runFixedGetter g b =
    checkBufferLength getterLength bufferLength
  $ unsafeInlineIO
  $ withForeignPtr base $ \(Ptr addr) -> return
    ( unFixedGetter g $ plusAddr (Addr addr) offset
    , fromForeignPtr base (offset + getterLength) (bufferLength - getterLength)
    )
  where
    (base, offset, bufferLength) = toForeignPtr b
    getterLength = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE runFixedGetter #-}

-- * Generic getter

data Getter a = GetterDone    !a
              | GetterPartial !Int !((ForeignPtr Word8, Addr) -> Getter a)
                -- ^ required buffer size and next step

instance Functor Getter where
  fmap f (GetterDone a) = GetterDone $ f a
  fmap f (GetterPartial n g) = GetterPartial n $ fmap f . g

instance Applicative Getter where
  pure = GetterDone
  (GetterDone f) <*> b = fmap f b
  (GetterPartial n f) <*> b = GetterPartial n $ \addr -> f addr <*> b

instance Monad Getter where
  (GetterDone a) >>= k = k a
  (GetterPartial n f) >>= k = GetterPartial n $ \addr -> f addr >>= k

fixed :: forall n a . ( KnownNat n )
      => FixedGetter 0 n a -> Getter a
fixed g = GetterPartial getterLength $ \(_, addr) -> GetterDone $ unFixedGetter g addr
  where
    getterLength = fromIntegral $ natVal (Proxy :: Proxy n)
{-# INLINE fixed #-}

runGetter :: Getter a -> ByteString -> (a, ByteString)
runGetter g b = unsafeInlineIO $ withForeignPtr base $ \(Ptr startAddr) -> do
  let go (GetterDone a) addr = let bytesRead = minusAddr addr $ Addr startAddr
                               in ( a, fromForeignPtr base (offset + bytesRead) (bufferLength - bytesRead) )
      go (GetterPartial n f) addr = undefined 
  return $ go g $ Addr startAddr
  where
    (base, offset, bufferLength) = toForeignPtr b
{-# INLINE runGetter #-}
