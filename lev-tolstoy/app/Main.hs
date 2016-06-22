{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Arrow
import           Control.Monad.Indexed
import           Data.ByteString       as BS
import           Data.Int
import           Data.Primitive.Addr
import           GHC.Prim
import           GHC.TypeLits
import           Lev.Get
import qualified Lev.GetI              as GI

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

test_d :: _ => GI.FixedGetter i j (Int32, Int32, Int32, Int32)
test_d =
  GI.int32Host >>>= \a ->
  GI.int32Host >>>= \b ->
  GI.int32Host >>>= \c ->
  GI.int32Host >>>= \d ->
  ireturn (a, b, c, d)

test_f = GI.runFixedGetter test_d (BS.replicate 16 0)

--test_d :: GI.FixedGetter i (i + 16) (Int32, Int32, Int32, Int32)
--test_d = do
--  a <- GI.int32Host
--  b <- GI.int32Host
--  c <- GI.int32Host
--  d <- GI.int32Host
--  return (a, b, c, d)

main :: IO ()
main = do
  return ()
