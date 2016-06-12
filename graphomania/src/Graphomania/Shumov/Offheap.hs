{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphomania.Shumov.Offheap ( readShumov ) where

import           Control.DeepSeq
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Int
import           Data.MonoTraversable
import           GHC.Generics            (Generic)
import           Offheap.Get

newtype ShumovOffheap = ShumovOffheap { unShumov :: ByteString } deriving (Generic)

instance NFData ShumovOffheap

data ShumovVertexOffheap = ShumovVertexOffheap
  { _idSize    :: !Int16
  , _edgeCount :: !Int32
  }  deriving ( Show, Eq, Ord )

type instance Element ShumovOffheap = ShumovVertexOffheap

instance MonoFoldable ShumovOffheap where
  ofoldr f z = go z . unShumov
    where
      go z s | BS.null s = z
             | otherwise = let (x, xs) = runGetStrict getShumovVertexOffheap s
                           in f x (go z xs)

  ofoldMap = error "Data.Graph.Shumov.ofoldMap"
  ofoldr1Ex = error "Data.Graph.Shumov.ofoldr1Ex"
  ofoldl1Ex' = error "Data.Graph.Shumov.ofoldl1Ex'"

  ofoldl' f v = go v . unShumov
    where
      go !a !s | BS.null s = a
               | otherwise = let (x, xs) = runGetStrict getShumovVertexOffheap s
                             in go (f a x) xs

  ofoldlM f v = go v . unShumov
    where
      go !a !s | BS.null s = pure a
               | otherwise = let (x, xs) = runGetStrict getShumovVertexOffheap s
                             in f a x >>= flip go xs

runGetStrict :: Get (ST s) a -> ByteString -> (a, ByteString)
runGetStrict get bs = unsafeInlineST (runByteString get bs)

getShumovVertexOffheap :: Get (ST s) ShumovVertexOffheap
getShumovVertexOffheap = do
  idSize     <- getInt16Host
  skip (fromIntegral idSize)
  edgeCount  <- getInt32Host
  skip (fromIntegral (edgeCount * 10))
  return $ ShumovVertexOffheap idSize edgeCount

readShumov :: FilePath -> IO ShumovOffheap
readShumov = fmap ShumovOffheap . BS.readFile
