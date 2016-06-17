{-# LANGUAGE Arrows #-}

module Main where

import           Control.Arrow
import           Data.Int
import           Lev.Get

test_a :: Get a Int32
test_a = (get int32Host)
   >>> (get int32Host)
   >>> (get int32Host)
   >>> (get int32Host)

test_b :: Get a (Int32, Int32, Int32, Int32)
test_b = proc _ -> do
  a <- (get int32Host) -< ()
  b <- (get int32Host) -< ()
  c <- (get int32Host) -< ()
  d <- (get int32Host) -< ()
  returnA -< (a, b, c, d)

test_c :: Get a (Int32, Int32, Int32, Int32)
test_c = proc _ -> do
  a <- (get $ int32HostO 0) -< ()
  b <- (get $ int32HostO 4) -< ()
  c <- (get $ int32HostO 8) -< ()
  d <- (get $ int32HostO 12) -< ()
  returnA -< (a, b, c, d)

main :: IO ()
main = do
  return ()
