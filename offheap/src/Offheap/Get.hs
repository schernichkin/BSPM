{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}

module Offheap.Get
  ( Get (..)
  , getInt16Host
  , getInt32Host
  , getInt64Host
  , runByteString
  , skip
  ) where

import           Control.Arrow
import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive.Addr
import           Data.Primitive.Types
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           GHC.Base                  (Int (..))
import           GHC.Ptr

-- TODO: Нужно либо удалить этот модуль, либо переработать его аналогично
-- GetI, только использовать readOffAddr вместо indexOffAddr
newtype Get s a = Get { unGet :: Addr -> s (a, Addr) }

instance (Functor s) => Functor (Get s) where
  fmap f = Get . fmap (fmap (first f)) . unGet

instance (Monad s) => Applicative (Get s) where
  pure a = Get $ \(!addr) -> pure (a, addr)
  fa <*> fb = Get $ \(!addr) -> do
    (f, addr') <- unGet fa addr
    fmap (first f) (unGet fb addr')

instance (Monad s) => Monad (Get s) where
  m >>= k = Get $ \(!addr) -> do
    (a, addr') <- unGet m addr
    unGet (k a) addr'

{-# INLINE getInt16Host #-}
getInt16Host :: (PrimMonad s) => Get s Int16
getInt16Host = Get $ \(!addr) -> do
  a <- readOffAddr addr 0
  return (a, addr `plusAddr` (I# (sizeOf# (undefined :: Int16))))

{-# INLINE getInt32Host #-}
getInt32Host :: (PrimMonad s) => Get s Int32
getInt32Host = Get $ \(!addr) -> do
  a <- readOffAddr addr 0
  return (a, addr `plusAddr` (I# (sizeOf# (undefined :: Int32))))

{-# INLINE getInt64Host #-}
getInt64Host :: (PrimMonad s) => Get s Int64
getInt64Host = Get $ \(!addr) -> do
  a <- readOffAddr addr 0
  return (a, addr `plusAddr` (I# (sizeOf# (undefined :: Int64))))

{-# INLINE skip #-}
skip :: (PrimMonad s) => Int -> Get s ()
skip n = Get $ \(!addr) -> do
  return ((), addr `plusAddr` n)

{-# INLINE withForeignPtr' #-}
withForeignPtr' fo prim = do
  r <- prim (unsafeForeignPtrToPtr fo)
  touch fo
  return r

{-# INLINE runByteString #-}
runByteString :: (PrimMonad s) => Get s a -> ByteString -> s (a, ByteString)
runByteString get bs  = withForeignPtr' fptr $ \ptr -> do
  let (!Ptr addr) = ptr `plusPtr` off
  (r, addr') <- unGet get (Addr addr)
  let off2 = addr' `minusAddr` (Addr addr)
  return $ (r, fromForeignPtr fptr (off + off2) (len - off2))
  where
    (fptr, off, len) = toForeignPtr bs
