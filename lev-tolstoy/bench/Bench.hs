{-# LANGUAGE Arrows              #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Bench where

import           Control.Arrow
import           Control.Monad.Indexed
import           Control.Monad.Primitive
import           Criterion.Main
import           Data.ByteString          as BS
import           Data.ByteString.Internal
import           Data.Int
import           Foreign.ForeignPtr
import           GHC.Int
import           GHC.IO                   (IO (..))
import           GHC.Prim
import           GHC.Ptr
import qualified Lev.Get                  as LI


{-# INLINE get100BHWO #-}
get100BHWO :: ByteString -> ( Int64, ByteString )
get100BHWO buffer = case toForeignPtr buffer of
  (fbase, I# off, I# len) ->
    unsafeInlineIO $ withForeignPtr fbase $ \(Ptr base) -> do
      let !addr = plusAddr# base off
          !a1  = indexInt64OffAddr# addr 0#
          !a2  = indexInt64OffAddr# addr 1#
          !a3  = indexInt64OffAddr# addr 2#
          !a4  = indexInt64OffAddr# addr 3#
          !a5  = indexInt64OffAddr# addr 4#
          !a6  = indexInt64OffAddr# addr 5#
          !a7  = indexInt64OffAddr# addr 6#
          !a8  = indexInt64OffAddr# addr 7#
          !a9  = indexInt64OffAddr# addr 8#
          !a10 = indexInt64OffAddr# addr 9#
          !a11 = indexInt64OffAddr# addr 10#
          !a12 = indexInt64OffAddr# addr 11#
          !a13 = indexInt32OffAddr# addr 24#
      return ( I64# (a1 +# a2 +# a3 +# a4
               +# a5 +# a6 +# a7 +# a8
               +# a9 +# a10 +# a11 +# a12
               +# a13), fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )

read1GHWO :: ByteString -> Int64
read1GHWO = go (10000000 :: Int) 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = get100BHWO b in go (n - 1) (a + a') b'

{-# INLINE get100BHW #-}
get100BHW :: ByteString -> ( Int64, ByteString )
get100BHW buffer = case toForeignPtr buffer of
  (fbase, I# off, I# len) ->
    unsafeInlineIO $ withForeignPtr fbase $ \(Ptr base) -> do
      let !addr = plusAddr# base off
          !a1  = indexInt64OffAddr# addr 0#
          !a2  = indexInt64OffAddr# (plusAddr# addr  8#) 0#
          !a3  = indexInt64OffAddr# (plusAddr# addr 16#) 0#
          !a4  = indexInt64OffAddr# (plusAddr# addr 24#) 0#
          !a5  = indexInt64OffAddr# (plusAddr# addr 32#) 0#
          !a6  = indexInt64OffAddr# (plusAddr# addr 40#) 0#
          !a7  = indexInt64OffAddr# (plusAddr# addr 48#) 0#
          !a8  = indexInt64OffAddr# (plusAddr# addr 56#) 0#
          !a9  = indexInt64OffAddr# (plusAddr# addr 64#) 0#
          !a10 = indexInt64OffAddr# (plusAddr# addr 72#) 0#
          !a11 = indexInt64OffAddr# (plusAddr# addr 80#) 0#
          !a12 = indexInt64OffAddr# (plusAddr# addr 88#) 0#
          !a13 = indexInt32OffAddr# (plusAddr# addr 96#) 0#
      return ( I64# (a1 +# a2 +# a3 +# a4
               +# a5 +# a6 +# a7 +# a8
               +# a9 +# a10 +# a11 +# a12
               +# a13), fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )

read1GHW :: ByteString -> Int64
read1GHW = go (10000000 :: Int) 0
 where
   go 0 a _ = a
   go n a b = let (a', b') = get100BHW b in go (n - 1) (a + a') b'

{-# INLINE get100BLI #-}
get100BLI :: LI.GetFixed 0 100 ( Int64 )
get100BLI =
  LI.int64Host >>>= \a1 ->
  LI.int64Host >>>= \a2 ->
  LI.int64Host >>>= \a3 ->
  LI.int64Host >>>= \a4 ->
  LI.int64Host >>>= \a5 ->
  LI.int64Host >>>= \a6 ->
  LI.int64Host >>>= \a7 ->
  LI.int64Host >>>= \a8 ->
  LI.int64Host >>>= \a9 ->
  LI.int64Host >>>= \a10 ->
  LI.int64Host >>>= \a11 ->
  LI.int64Host >>>= \a12 ->
  LI.int32Host >>>= \a13 ->
  ireturn $   a1 + a2 + a3 + a4
            + a5 + a6 + a7 + a8
            + a9 + a10 + a11 + a12
            + fromIntegral a13

read1GLI :: ByteString -> Int64
read1GLI = go (10000000 :: Int) 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = LI.runGetFixed get100BLI b in go (n - 1) (a + a') b'

get100BLILifted :: LI.Get Int64
get100BLILifted = LI.fixed get100BLI
{-# INLINE get100BLILifted #-}

read1GLILifted :: ByteString -> Int64
read1GLILifted = go (10000000 :: Int) 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = LI.runGet get100BLILifted b in go (n - 1) (a + a') b'

read1GBench :: Benchmark
read1GBench = env setupEnv $ \ ~(buffer) ->
  bgroup "read 1G"
  [ bench "Handwritten" $ nf read1GHW buffer
  , bench "Handwritten with offsets" $ nf read1GHWO buffer
  , bench "Lev fixed getter" $ nf read1GLI buffer
  , bench "Lev converted fixed getter" $ nf read1GLILifted buffer
  ]
  where
    setupEnv = return $ BS.replicate 1000000000 0

main :: IO ()
main = defaultMain [ read1GBench ]
