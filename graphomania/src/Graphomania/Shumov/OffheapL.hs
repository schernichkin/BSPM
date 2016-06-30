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

module Graphomania.Shumov.OffheapL
  ( readShumov
  , offheapVertices
  ) where

import           Control.DeepSeq
import           Control.Lens.Fold
import           Control.Lens.Internal.Getter
import           Control.Monad.Indexed
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Int
import           GHC.Generics                 (Generic)
import           Lev.Get

newtype ShumovOffheap = ShumovOffheap { unShumov :: ByteString } deriving (Generic)

instance NFData ShumovOffheap

data ShumovVertexOffheap = ShumovVertexOffheap
  { _vertexId  :: !ByteString
  , _edgeCount :: !Int32
  , _edges     :: !ByteString
  }  deriving ( Show, Eq, Ord )

shumovVertexOffheap :: Get ShumovVertexOffheap
shumovVertexOffheap = do
  idSize     <- fixed int16BE -- want to measure original BE format
  vertexId   <- byteString (fromIntegral idSize)
  edgeCount  <- fixed int32BE
  edges      <- byteString (fromIntegral edgeCount * 10)
  return $ ShumovVertexOffheap vertexId edgeCount edges
{-# INLINE shumovVertexOffheap #-}

readShumov :: FilePath -> IO ShumovOffheap
readShumov = fmap ShumovOffheap . BS.readFile

type FoldShumovOffheap = Fold ShumovOffheap ShumovVertexOffheap

offheapVertices :: FoldShumovOffheap
offheapVertices f = go . unShumov
  where
    go s | BS.null s = noEffect
         | otherwise = let (x, xs) = run shumovVertexOffheap s
                        in f x *> go xs
{-# INLINE offheapVertices #-}
