{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax  #-}

module Bench.Lev (
    read12Int64PlusInt32
  , module X
  ) where

import qualified Data.ByteString as BS
import           Data.Int
import           Data.Word
import           Lev.Monad       as X
import           Prelude         hiding (Monad (..))

{-# INLINE read12Int64PlusInt32 #-}
read12Int64PlusInt32 :: Reader IO ('StaticLayout 0 100) Int64
read12Int64PlusInt32 = do
  a1 <- readInt64Host
  a2 <- readInt64Host
  a3 <- readInt64Host
  a4 <- readInt64Host
  a5 <- readInt64Host
  a6 <- readInt64Host
  a7 <- readInt64Host
  a8 <- readInt64Host
  a9 <- readInt64Host
  a10 <- readInt64Host
  a11 <- readInt64Host
  a12 <- readInt64Host
  a13 <- readInt32Host
  return $ a1 + a2 + a3 + a4
         + a5 + a6 + a7 + a8
         + a9 + a10 + a11 + a12
         + fromIntegral a13

read4Strings :: Reader IO 'DynamicLayout BS.ByteString
read4Strings = readByteString 11 >>= \_ -> readByteString 12 
