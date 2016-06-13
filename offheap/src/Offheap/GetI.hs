{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

-- | Getter for immutable store.
module Offheap.GetI
  ( GetI (..)
  , getPrim
  , getInt16Host
  , getInt32Host
  , getInt64Host
  , skip
  , runByteString
  ) where

import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Foreign.ForeignPtr
import           GHC.Base
import           GHC.Ptr

{- 4.51 -}
newtype GetI a  = GetI { unGetI :: Addr# -> (# a, Addr# #) }

instance Functor GetI where
  fmap f g = GetI $ \addr ->
    let (# a, addr' #) = unGetI g addr
     in (# f a, addr' #)

instance Applicative GetI where
  pure a = GetI $ \addr -> (# a, addr #)

  f1 <*> f2 = GetI $ \addr ->
    let (# f, addr' #)  = unGetI f1 addr
        (# a, addr'' #) = unGetI f2 addr'
     in (# f a, addr'' #)

instance Monad GetI where
  m >>= k = GetI $ \addr ->
    let (# a, addr' #)  = unGetI m addr
     in unGetI (k a) addr'

{-# INLINE getPrim #-}
getPrim :: forall a . (Prim a) => GetI a
getPrim = GetI $ \addr ->
  (# indexOffAddr# addr 0#, plusAddr# addr (sizeOf# (undefined :: a)) #)

{-# INLINE getInt16Host #-}
getInt16Host :: GetI Int16
getInt16Host = getPrim

{-# INLINE getInt32Host #-}
getInt32Host :: GetI Int32
getInt32Host = getPrim

{-# INLINE getInt64Host #-}
getInt64Host :: GetI Int64
getInt64Host = getPrim

{-# INLINE skip #-}
skip :: Int -> GetI ()
skip (I# i) = GetI $ \addr -> (# (), plusAddr# addr i #)

-- TODO: Удалить. Для чтнения байтсрингов использовать специализированный ридер (пока можно оставить для бенчмарков)
{-# INLINE runByteString #-}
runByteString :: GetI a -> ByteString -> (a, ByteString)
runByteString g bs  = unsafeInlineIO $ withForeignPtr fptr $ \(Ptr base) -> do
    let (# a, addr #) = unGetI g (plusAddr# base off)
        newOff = I# (minusAddr# addr base)
    return (a, fromForeignPtr fptr newOff (len - newOff + (I# off)))
  where
    !(fptr, (I# off), len) = toForeignPtr bs

    {- 4.44 мне не нравится этот вариант, поскольку приходится суммировать длинны
            и двигать указатель независимо. Хотя разницы в производительности
            тесты не показывают.

    newtype GetI a  = GetI { unGetI :: Addr# -> (# a, Int# #) }

    instance Functor GetI where
      fmap f g = GetI $ \addr ->
        let (# a, n #) = unGetI g addr
         in (# f a, n #)

    instance Applicative GetI where
      pure a = GetI $ \addr -> (# a, 0# #)

      f1 <*> f2 = GetI $ \addr ->
        let (# f, n0 #)  = unGetI f1 addr
            (# a, n1 #) = unGetI f2 (plusAddr# addr n0)
         in (# f a, n0 +# n1 #)

    instance Monad GetI where
      m >>= k = GetI $ \addr ->
        let (# a, n0 #)  = unGetI m addr
            (# x, n1 #) = unGetI (k a) (plusAddr# addr n0)
         in (# x, n0 +# n1 #)

    {-# INLINE getPrim #-}
    getPrim :: forall a . (Prim a) => GetI a
    getPrim = GetI $ \addr ->
      (# indexOffAddr# addr 0#, sizeOf# (undefined :: a) #)

    {-# INLINE getInt16Host #-}
    getInt16Host :: GetI Int16
    getInt16Host = getPrim

    {-# INLINE getInt32Host #-}
    getInt32Host :: GetI Int32
    getInt32Host = getPrim

    {-# INLINE getInt64Host #-}
    getInt64Host :: GetI Int64
    getInt64Host = getPrim

    {-# INLINE skip #-}
    skip :: Int -> GetI ()
    skip (I# i) = GetI $ \addr -> (# (), i #)

    -- TODO: Удалить. Для чтнения байтсрингов использовать специализированный ридер (пока можно оставить для бенчмарков)
    {-# INLINE runByteString #-}
    runByteString :: GetI a -> ByteString -> (a, ByteString)
    runByteString g bs  = unsafeInlineIO $ withForeignPtr fptr $ \(Ptr addr) -> do
        let (# a, n #) = unGetI g addr
            bytesRead = I# n
        return (a, fromForeignPtr fptr (off + bytesRead) (len - bytesRead))
      where
        (fptr, off, len) = toForeignPtr bs
    -}
