{-# LANGUAGE RecordWildCards #-}

module Lev.Buffer
    ( Buffer
    , fromForeignPtr
    , toForeignPtr
    ) where

import           Data.Word
import           Foreign.ForeignPtr

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
