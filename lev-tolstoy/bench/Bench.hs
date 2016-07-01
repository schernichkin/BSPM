{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench where

import           Control.Monad.Indexed
import           Control.Monad.Primitive
import           Criterion.Main
import qualified Data.Binary.Get          as B
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Serialize.Get       as C
import           Foreign.ForeignPtr
import           GHC.Int
import           GHC.IO                   (IO (..))
import           GHC.Prim
import           GHC.Ptr
import qualified Lev.Get                  as L

runBinaryGetStrict :: B.Get a -> ByteString -> (a, ByteString)
runBinaryGetStrict g = feed (B.runGetIncremental g) . Just
  where
    feed (B.Done s _ a) _ = (a, s)
    feed (B.Partial f) s = feed (f s) Nothing
    feed (B.Fail _ pos msg) _ = error $ "Bench.runBinaryGetStrict failed at position "
                                     ++ show pos ++ " with message : " ++ msg
{-# INLINE runBinaryGetStrict #-}

runCerealGetStrict :: C.Get a -> ByteString -> (a, ByteString)
runCerealGetStrict g = \s -> case (C.runGetPartial g s) of
  C.Done a s'  -> (a, s')
  C.Partial _  -> error $ "Bench.runCerealGetStrict failed: unexpected Partial."
  C.Fail msg _ -> error $ "Bench.runCerealGetStrict failed with message : " ++ msg
{-# INLINE runCerealGetStrict #-}

readerBench :: Benchmark
readerBench = bgroup "reader" [ strict ]
  where
    strict = bgroup "strict"
      [ read1Ginto12Int64plusInt32
      , bigVsLittleEndian
      ]
      where
        read1Ginto12Int64plusInt32 = env setupEnv $ \ ~(buffer) ->
          bgroup "read 1G into 12 int64 + int32"
          [ bench "handwritten (indexOffAddr#)" $ nf handwritten buffer
          , bench "handwritten (indexOffAddr# with offsets) " $ nf handwrittenOff buffer
          , bench "lev" $ nf lev buffer
          , bench "lev fixed" $ nf levFixed buffer
          , bench "binary" $ nf binary buffer
          , bench "cereal" $ nf cereal buffer
          ]
          where
            bufferSize :: Int
            bufferSize = 1000000000
            {-# INLINE bufferSize #-}

            iterations :: Int
            iterations = bufferSize `div` 100
            {-# INLINE iterations #-}

            setupEnv :: IO ByteString
            setupEnv = return $ BS.replicate bufferSize 0

            run :: (ByteString -> (Int64, ByteString)) -> ByteString -> Int64
            run f = go 0 iterations
              where
                go a 0 _ = a
                go a n s = let (a', s') = f s in go (a + a') (n - 1) s'
            {-# INLINE run #-}

            handwritten :: ByteString -> Int64
            handwritten = run $ \buffer -> case BS.toForeignPtr buffer of
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
                           +# a13), BS.fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )
            {-# NOINLINE handwritten #-}

            handwrittenOff :: ByteString -> Int64
            handwrittenOff = run $ \buffer -> case BS.toForeignPtr buffer of
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
                           +# a13), BS.fromForeignPtr fbase (I# (off +# 100#)) (I# (len -# 100#)) )
            {-# NOINLINE handwrittenOff #-}

            fixedGetter :: L.GetFixed 0 100 Int64
            fixedGetter =
              L.int64Host >>>= \a1 ->
              L.int64Host >>>= \a2 ->
              L.int64Host >>>= \a3 ->
              L.int64Host >>>= \a4 ->
              L.int64Host >>>= \a5 ->
              L.int64Host >>>= \a6 ->
              L.int64Host >>>= \a7 ->
              L.int64Host >>>= \a8 ->
              L.int64Host >>>= \a9 ->
              L.int64Host >>>= \a10 ->
              L.int64Host >>>= \a11 ->
              L.int64Host >>>= \a12 ->
              L.int32Host >>>= \a13 ->
              ireturn $   a1 + a2 + a3 + a4
                        + a5 + a6 + a7 + a8
                        + a9 + a10 + a11 + a12
                        + fromIntegral a13
            {-# INLINE fixedGetter #-}

            lev = run $ L.run $ L.fixed fixedGetter
            {-# NOINLINE lev #-}

            levFixed = run $ L.runFixed fixedGetter
            {-# NOINLINE levFixed #-}

            binary = run $ runBinaryGetStrict $ do
              a1 <- B.getInt64host
              a2 <- B.getInt64host
              a3 <- B.getInt64host
              a4 <- B.getInt64host
              a5 <- B.getInt64host
              a6 <- B.getInt64host
              a7 <- B.getInt64host
              a8 <- B.getInt64host
              a9 <- B.getInt64host
              a10 <- B.getInt64host
              a11 <- B.getInt64host
              a12 <- B.getInt64host
              a13 <- B.getInt32host
              return $ a1 + a2 + a3 + a4
                     + a5 + a6 + a7 + a8
                     + a9 + a10 + a11 + a12
                     + fromIntegral a13
            {-# NOINLINE binary #-}

            cereal = run $ runCerealGetStrict $ do
              a1 <- C.getWord64host
              a2 <- C.getWord64host
              a3 <- C.getWord64host
              a4 <- C.getWord64host
              a5 <- C.getWord64host
              a6 <- C.getWord64host
              a7 <- C.getWord64host
              a8 <- C.getWord64host
              a9 <- C.getWord64host
              a10 <- C.getWord64host
              a11 <- C.getWord64host
              a12 <- C.getWord64host
              a13 <- C.getWord32host
              return $ (fromIntegral (a1 + a2 + a3 + a4
                     + a5 + a6 + a7 + a8
                     + a9 + a10 + a11 + a12) :: Int64)
                     + fromIntegral a13
            {-# NOINLINE cereal #-}

        bigVsLittleEndian = env setupEnv $ \ ~(buffer) ->
          bgroup "read 1G into 12 int64 + int32"
          [ bench "lev big-endian" $ nf bigEndian buffer
          , bench "lev little-endian" $ nf littleEndian buffer
          ]
          where
            bufferSize :: Int
            bufferSize = 1000000000
            {-# INLINE bufferSize #-}

            iterations :: Int
            iterations = bufferSize `div` 100
            {-# INLINE iterations #-}

            setupEnv :: IO ByteString
            setupEnv = return $ BS.replicate bufferSize 0

            run :: (ByteString -> (Int64, ByteString)) -> ByteString -> Int64
            run f = go 0 iterations
              where
                go a 0 _ = a
                go a n s = let (a', s') = f s in go (a + a') (n - 1) s'
            {-# INLINE run #-}

            bigEndian = run $ L.run $ L.fixed $
              L.int64BE >>>= \a1 ->
              L.int64BE >>>= \a2 ->
              L.int64BE >>>= \a3 ->
              L.int64BE >>>= \a4 ->
              L.int64BE >>>= \a5 ->
              L.int64BE >>>= \a6 ->
              L.int64BE >>>= \a7 ->
              L.int64BE >>>= \a8 ->
              L.int64BE >>>= \a9 ->
              L.int64BE >>>= \a10 ->
              L.int64BE >>>= \a11 ->
              L.int64BE >>>= \a12 ->
              L.int32BE >>>= \a13 ->
              ireturn $   a1 + a2 + a3 + a4
                        + a5 + a6 + a7 + a8
                        + a9 + a10 + a11 + a12
                        + fromIntegral a13
            {-# NOINLINE bigEndian #-}

            littleEndian = run $ L.run $ L.fixed $
              L.int64LE >>>= \a1 ->
              L.int64LE >>>= \a2 ->
              L.int64LE >>>= \a3 ->
              L.int64LE >>>= \a4 ->
              L.int64LE >>>= \a5 ->
              L.int64LE >>>= \a6 ->
              L.int64LE >>>= \a7 ->
              L.int64LE >>>= \a8 ->
              L.int64LE >>>= \a9 ->
              L.int64LE >>>= \a10 ->
              L.int64LE >>>= \a11 ->
              L.int64LE >>>= \a12 ->
              L.int32LE >>>= \a13 ->
              ireturn $   a1 + a2 + a3 + a4
                        + a5 + a6 + a7 + a8
                        + a9 + a10 + a11 + a12
                        + fromIntegral a13
            {-# NOINLINE littleEndian #-}

writerBench :: Benchmark
writerBench = bgroup "Writer" [ strict ]
  where
    strict = bgroup "Strict" [ ]

main :: IO ()
main = defaultMain
  [ readerBench
  , writerBench
  ]
