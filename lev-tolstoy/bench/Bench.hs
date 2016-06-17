{-# LANGUAGE Arrows              #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Bench where

import           Control.Arrow
import           Control.Monad.Primitive
import           Criterion.Main
import           Data.Int
import           Foreign.ForeignPtr
import           GHC.Int
import           GHC.IO                  (IO (..))
import           GHC.Prim
import           GHC.Ptr
import qualified Lev.Buffer              as L
import qualified Lev.Get                 as L

{-# INLINE get100BHWO #-}
get100BHWO :: L.Buffer -> ( Int64, L.Buffer )
get100BHWO buffer = case buffer of
  L.Buffer (fbase) (I# off) (I# len) ->
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
               +# a13), L.fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )

read1GHWO :: L.Buffer -> Int64
read1GHWO = go (10000000 :: Int) 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = get100BHWO b in go (n - 1) (a + a') b'

{-# INLINE get100BHW #-}
get100BHW :: L.Buffer -> ( Int64, L.Buffer )
get100BHW buffer = case buffer of
  L.Buffer (fbase) (I# off) (I# len) ->
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
               +# a13), L.fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )

read1GHW :: L.Buffer -> Int64
read1GHW = go (10000000 :: Int) 0
 where
   go 0 a _ = a
   go n a b = let (a', b') = get100BHW b in go (n - 1) (a + a') b'

{-# INLINE get100BL #-}
get100BL :: L.Get a ( Int64 )
get100BL = proc _ -> do
  a1  <- (L.get L.int64Host) -< ()
  a2  <- (L.get L.int64Host) -< ()
  a3  <- (L.get L.int64Host) -< ()
  a4  <- (L.get L.int64Host) -< ()
  a5  <- (L.get L.int64Host) -< ()
  a6  <- (L.get L.int64Host) -< ()
  a7  <- (L.get L.int64Host) -< ()
  a8  <- (L.get L.int64Host) -< ()
  a9  <- (L.get L.int64Host) -< ()
  a10 <- (L.get L.int64Host) -< ()
  a11 <- (L.get L.int64Host) -< ()
  a12 <- (L.get L.int64Host) -< ()
  a13 <- (L.get L.int32Host) -< ()
  returnA -<  a1 + a2 + a3 + a4
            + a5 + a6 + a7 + a8
            + a9 + a10 + a11 + a12
            + fromIntegral a13

read1GL :: L.Buffer -> Int64
read1GL = go (10000000 :: Int) 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = L.run get100BL b in go (n - 1) (a + a') b'


{-# INLINE get100BLO #-}
get100BLO :: L.Get a ( Int64 )
get100BLO = proc _ -> do
  a1  <- (L.get $ L.int64HostO 0) -< ()
  a2  <- (L.get $ L.int64HostO 8) -< ()
  a3  <- (L.get $ L.int64HostO 16) -< ()
  a4  <- (L.get $ L.int64HostO 24) -< ()
  a5  <- (L.get $ L.int64HostO 32) -< ()
  a6  <- (L.get $ L.int64HostO 40) -< ()
  a7  <- (L.get $ L.int64HostO 48) -< ()
  a8  <- (L.get $ L.int64HostO 56) -< ()
  a9  <- (L.get $ L.int64HostO 64) -< ()
  a10 <- (L.get $ L.int64HostO 72) -< ()
  a11 <- (L.get $ L.int64HostO 80) -< ()
  a12 <- (L.get $ L.int64HostO 88) -< ()
  a13 <- (L.get $ L.int32HostO 96) -< ()
  returnA -<  a1 + a2 + a3 + a4
            + a5 + a6 + a7 + a8
            + a9 + a10 + a11 + a12
            + fromIntegral a13

read1GLO :: L.Buffer -> Int64
read1GLO = go (10000000 :: Int) 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = L.run get100BLO b in go (n - 1) (a + a') b'


read1GBench :: Benchmark
read1GBench = env setupEnv $ \ ~(buffer) ->
  bgroup "read 1G"
  [ bench "Handwritten" $ nf read1GHW buffer
  , bench "Handwritten with offsets" $ nf read1GHWO buffer
  , bench "Lev" $ nf read1GL buffer
  , bench "Levfo" $ nf read1GLO buffer
  ]
  where
    setupEnv = L.newBuffer 1000000000

main :: IO ()
main = defaultMain [ read1GBench ]
