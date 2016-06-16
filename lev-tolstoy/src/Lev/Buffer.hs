{-# LANGUAGE RecordWildCards #-}

module Lev.Buffer
    ( Buffer (..)
    , fromForeignPtr
    , toForeignPtr
    , newBuffer
    ) where

import           Data.Word
import           Foreign.ForeignPtr
import           GHC.ForeignPtr

data Buffer = Buffer
  { _base   :: !(ForeignPtr Word8)
  , _offset :: !Int
  , _length :: !Int
  }

fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> Buffer
fromForeignPtr = Buffer
{-# INLINE fromForeignPtr #-}

toForeignPtr :: Buffer -> (ForeignPtr Word8, Int, Int)
toForeignPtr Buffer {..} = (_base, _offset, _length)
{-# INLINE toForeignPtr #-}

newBuffer :: Int -> IO Buffer
newBuffer size = do
  fptr <- mallocPlainForeignPtrBytes size
  return $ fromForeignPtr fptr 0 size
{-# INLINE newBuffer #-}
