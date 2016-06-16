{-# LANGUAGE Arrows #-}

module Bench where

import           Control.Arrow
import           Criterion.Main
import           Data.Int
import qualified Lev.Buffer     as L
import qualified Lev.Get        as L

-- PC3-12800: 1600MTS
{-# INLINE get100Bytes #-}
get100Bytes :: L.Get a ( Int64 )
get100Bytes = proc _ -> do
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

read100ML :: L.Buffer -> Int64
read100ML = go 1000000 0
  where
    go 0 a _ = a
    go n a b = let (a', b') = L.run get100Bytes b in go (n - 1) (a + a') b'

get100MBench :: Benchmark
get100MBench = env setupEnv $ \ ~(buffer) ->
  bgroup "read 100M"
  [ bench "Lev" $ nf read100ML buffer
  ]
  where
    setupEnv = newBuffer 100000000

main :: IO ()
main = defaultMain [ ]
