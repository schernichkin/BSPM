{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE BangPatterns #-}

module Offheap.GetBS (
    GetBS (..)
  , runByteString
  , getPrim
  , getInt16Host
  , getInt32Host
  , getInt64Host
  , skip
  ) where

import           Data.ByteString.Internal
import           Data.Word
import           Foreign.ForeignPtr
import           GHC.Prim
import           GHC.Ptr
import qualified Offheap.GetI as I
import           Offheap.GetI (GetI(..))
import           GHC.Types
import           Data.Primitive
import           Data.Int
import           Control.Monad.Primitive

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Offheap.GetBS." ++ fun ++ ':':' ':msg

{-# NOINLINE moduleError #-}
moduleError :: forall (r :: RuntimeRep). forall (a :: TYPE r).
               String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)

{-# NOINLINE bufferLenghtError #-}
bufferLenghtError :: forall (r :: RuntimeRep). forall (a :: TYPE r).
                     Int# -> Int# -> a
bufferLenghtError required remains =
  moduleError "checkBufferLength"
            ( "Bytes remains = " ++ (show (I# remains)) ++
              ", bytes required = " ++ (show (I# required)) )

newtype GetBS a = GetBS
  { unGetBS :: ForeignPtr Word8  -- ^ base pointer
            -> Addr#             -- ^ current pointer
            -> Int#              -- ^ bytes remain in buffer
            -> (# a, Addr#, Int# #)    -- ^ (result, current pointer, bytes remain in buffer)
  }

instance Functor GetBS where
  fmap f g = GetBS $ \base addr remains ->
    let (# a, addr', remains' #) = unGetBS g base addr remains
     in (# f a, addr', remains' #)

instance Applicative GetBS where
  pure a = GetBS $ \_ addr remains -> (# a, addr, remains #)

  f1 <*> f2 = GetBS $ \base addr remains ->
    let (# f, addr', remains' #) = unGetBS f1 base addr remains
        (# a, addr'', remains'' #) = unGetBS f2 base addr' remains'
     in (# f a, addr'', remains'' #)

instance Monad GetBS where
  m >>= k = GetBS $ \base addr remains ->
    let (# a, addr', remains' #) = unGetBS m base addr remains
     in unGetBS (k a) base addr' remains'

-- TODO: эту функцию можно переделать так, чтобы она либо возвращала
-- новые reamins, либо кидала ошибку - это упростит её тип и уменьшит
-- кол-во вычислений.
{-# INLINE checkBufferLength #-}
checkBufferLength :: Int# -> Int# -> (# a, Addr#, Int# #) -> (# a, Addr#, Int# #)
checkBufferLength required remains r =
  case required <=# remains of
    1# -> r
    _  -> bufferLenghtError required remains

-- | Lift GetI info GetBS
-- Note that provided lenght must match number of bytes read by GetI hence
-- lifted GetI must alwaus read fixed number of bytes.
-- No checks are made, safety is the programmer's responsibility.
{-# INLINE usafeLiftGetI #-}
usafeLiftGetI :: Int# -> GetI a -> GetBS a
usafeLiftGetI required (GetI getI) = GetBS $ \base addr remains ->
  checkBufferLength required remains
    ( let (# a, addr' #) = getI addr
      in  (# a, addr', remains -# required #) )

{-# INLINE getPrim #-}
getPrim :: forall a . (Prim a) => GetBS a
getPrim = usafeLiftGetI (sizeOf# (undefined :: a)) I.getPrim

{-# INLINE getInt16Host #-}
getInt16Host :: GetBS Int16
getInt16Host = getPrim

{-# INLINE getInt32Host #-}
getInt32Host :: GetBS Int32
getInt32Host = getPrim

{-# INLINE getInt64Host #-}
getInt64Host :: GetBS Int64
getInt64Host = getPrim

{-# INLINE skip #-}
skip :: Int -> GetBS ()
skip (I# i) = GetBS $ \base addr remains ->
  checkBufferLength i remains (# (), plusAddr# addr i, remains -# i #)

{-# INLINE runByteString #-}
runByteString :: GetBS a -> ByteString -> (a, ByteString)
runByteString g bs  = unsafeInlineIO $ withForeignPtr fptr $ \(Ptr base) -> do
    let (# a, addr, remains #) = unGetBS g fptr (plusAddr# base off) len
    return (a, fromForeignPtr fptr (I# (minusAddr# addr base)) (I# remains))
  where
    !(fptr, I# off, I# len) = toForeignPtr bs
