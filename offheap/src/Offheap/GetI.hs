{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

-- | Getter for immutable store.
module Offheap.GetI
  ( GetI
  , getPrim
  , getInt16Host
  , getInt32Host
  , getInt64Host
  , skip
  , runByteString
  ) where

import           Control.Arrow
import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Foreign.ForeignPtr
import           GHC.Base
import           GHC.Ptr

-- TODO: базовый указатель нужен только для вычисления оффсета.
-- судя по тестам его наличие не влияет на производительность,
-- но мне всё равно не очень нравится факт его наличия.
-- Кроме того, базовый указатель не включает оффсет байтстринга, поэтому
-- не может быть использован для байтсринга непосредственно.
-- Нужно разработать нормальное решение для чтения байтсрингов.
-- Как вариант - можно даже отдельную монаду, которая хранит
-- информацию о байтсринге, оборачивает комбинаторы GetI в ensure
-- (проверка размера буфера) и имеет комбинаторы для генерации байтсрингов.
newtype GetI a  = GetI { unGetI :: (# Addr#, Addr# #) -> (# a, Addr# #) }

instance Functor GetI where
  fmap f g = GetI $ \(# base, addr #) ->
    let (# a, addr' #) = unGetI g (# base, addr #)
     in (# f a, addr' #)

instance Applicative GetI where
  pure a = GetI $ \(# base, addr #) -> (# a, addr #)

  f1 <*> f2 = GetI $ \(# base, addr #) ->
    let (# f, addr' #)  = unGetI f1 (# base, addr #)
        (# a, addr'' #) = unGetI f2 (# base, addr' #)
     in (# f a, addr'' #)

instance Monad GetI where
  m >>= k = GetI $ \(# base, addr #) ->
    let (# a, addr' #)  = unGetI m (# base, addr #)
     in unGetI (k a) (# base, addr' #)

{-# INLINE getPrim #-}
getPrim :: forall a . (Prim a) => GetI a
getPrim = GetI $ \(# base, addr #) ->
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
skip (I# i) = GetI $ \(# base, addr #) -> (# (), plusAddr# addr i #)

{-# INLINE runByteString #-}
runByteString :: GetI a -> ByteString -> (a, ByteString)
runByteString g bs  = unsafeInlineIO $ withForeignPtr fptr $ \(Ptr addr) -> do
    let (# a, addr' #) = unGetI g (# addr, addr #)
        bytesRead = I# (minusAddr# addr' addr)
    return (a, fromForeignPtr fptr (off + bytesRead) (len - bytesRead))
  where
    (fptr, off, len) = toForeignPtr bs
