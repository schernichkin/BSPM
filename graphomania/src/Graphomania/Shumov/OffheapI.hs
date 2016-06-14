{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphomania.Shumov.OffheapI
  ( readShumov
  , offheapVertices
  ) where

import           Control.DeepSeq
import           Control.Lens.Fold
import           Control.Lens.Internal.Getter
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Int
import           GHC.Generics                 (Generic)
import           Offheap.GetI

newtype ShumovOffheap = ShumovOffheap { unShumov :: ByteString } deriving (Generic)

instance NFData ShumovOffheap

data ShumovVertexOffheap = ShumovVertexOffheap
  { _idSize    :: !Int16
  , _edgeCount :: !Int32
  }  deriving ( Show, Eq, Ord )

getShumovVertexOffheap :: GetI ShumovVertexOffheap
getShumovVertexOffheap = do
  idSize     <- getInt16Host
  skip (fromIntegral idSize)
  edgeCount  <- getInt32Host
  skip (fromIntegral (edgeCount * 10))
  return $ ShumovVertexOffheap idSize edgeCount

readShumov :: FilePath -> IO ShumovOffheap
readShumov = fmap ShumovOffheap . BS.readFile

type FoldShumovOffheap = Fold ShumovOffheap ShumovVertexOffheap

{-# INLINE offheapVertices #-}
offheapVertices :: FoldShumovOffheap
offheapVertices f = go . unShumov
  where
    go s | BS.null s = noEffect
         | otherwise = let (x, xs) = runByteString getShumovVertexOffheap s
                        in f x *> go xs