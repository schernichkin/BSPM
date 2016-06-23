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

test_d :: _ => GI.FixedGetter i j (Int32, Int32, Int32, Int32)
test_d =
  GI.int32Host >>>= \a ->
  GI.int32Host >>>= \b ->
  GI.int32Host >>>= \c ->
  GI.int32Host >>>= \d ->
  ireturn (a, b, c, d)

test_f = GI.runFixedGetter test_d (BS.replicate 16 0)

test_g = GI.runGetter (GI.fixed test_d) (BS.replicate 16 0)

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
