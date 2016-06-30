{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Monad.Indexed
import           Data.ByteString       as BS
import           Data.Int
import           Lev.Get

test_a :: _ => GetFixed i j (Int32, Int32, Int32, Int32)
test_a =
  int32Host >>>= \a ->
  int32Host >>>= \b ->
  int32Host >>>= \c ->
  int32Host >>>= \d ->
  ireturn (a, b, c, d)
{-# INLINE test_a #-}


test_b = runFixed test_a (BS.replicate 16 0)

test_c = run (fixed test_a) (BS.replicate 16 0)

main :: IO ()
main = do
  return ()
