{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphomania.Utils.Binary
  ( TaggedBinary
  , EndiannessAware
  , runGetStrict
  , unsafePutStrict
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString              (ByteString)
import           Data.Int
import           Data.Tagged
import           Graphomania.Utils.ByteString
import           System.Endian

instance Binary (Tagged 'BigEndian Int16) where
  {-# INLINE get #-}
  get = Tagged <$> getInt16be
  put = putInt16be . untag

instance Binary (Tagged 'BigEndian Int32) where
  {-# INLINE get #-}
  get = Tagged <$> getInt32be
  put = putInt32be . untag

instance Binary (Tagged 'BigEndian Int64) where
  {-# INLINE get #-}
  get = Tagged <$> getInt64be
  put = putInt64be . untag

instance Binary (Tagged 'LittleEndian Int16) where
  {-# INLINE get #-}
  get = Tagged . fromIntegral . fromLE16 . fromIntegral <$> getInt16host
  put = putInt16le . untag

instance Binary (Tagged 'LittleEndian Int32) where
  {-# INLINE get #-}
  get = Tagged . fromIntegral . fromLE32 . fromIntegral <$> getInt32host
  put = putInt32le . untag

instance Binary (Tagged 'LittleEndian Int64) where
  {-# INLINE get #-}
  get = Tagged . fromIntegral . fromLE64 . fromIntegral <$> getInt64host
  put = putInt64le . untag

type TaggedBinary s a = Binary (Tagged s a)

type EndiannessAware e = ( TaggedBinary e Int16
                         , TaggedBinary e Int32
                         , TaggedBinary e Int64 )

{-# INLINE runGetStrict #-}
runGetStrict :: Get a -> ByteString -> (a, ByteString)
runGetStrict g = feed (runGetIncremental g) . Just
  where
    feed (Done r _ x) _ = (x, r)
    feed (Partial k) s = feed (k s) Nothing
    feed (Fail _ pos msg) _ = error $ "Data.Graph.Shumov.runGetStrict at position "
                                   ++ show pos ++ ": " ++ msg

-- | Put value to bytestring.
-- This function will update ByteString content, since bytestring content
-- consideted immutable this is unsafe operation.
{-# INLINE unsafePutStrict #-}
unsafePutStrict :: Put -> ByteString -> IO ByteString
unsafePutStrict p b = unsafePutBuilder (execPut p) b
