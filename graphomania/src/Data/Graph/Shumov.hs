{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Graph.Shumov
    ( Shumov
    , ShumovVertex
    , readFile
    , uncons
    ) where

import           Data.Binary.Get
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Graph.Class
import           Data.MonoTraversable
import           Graphomania.Utils.Binary
import           Prelude                  hiding (readFile)

newtype Shumov = Shumov { unShumov :: ByteString }

data ShumovVertex = ShumovVertex
  { _externalId :: !ByteString
  } deriving ( Show, Eq, Ord )

data ShumovEdges = ShumovEdges

type instance Element Shumov = ShumovVertex

instance MonoFoldable Shumov where
  ofoldr f z = go z . unShumov
    where
      go z s | BS.null s = z
             | otherwise = let (x, xs) = runGetStrict getVertex s
                           in f x (go z xs)

  ofoldMap = error "Data.Graph.Shumov.ofoldMap"
  ofoldr1Ex = error "Data.Graph.Shumov.ofoldr1Ex"
  ofoldl1Ex' = error "Data.Graph.Shumov.ofoldl1Ex'"

  ofoldl' f v = go v . unShumov
    where
      go !a !s | BS.null s = a
               | otherwise = let (x, xs) = runGetStrict getVertex s
                             in go (f a x) xs

  ofoldlM f v = go v . unShumov
    where
      go !a !s | BS.null s = pure a
               | otherwise = let (x, xs) = runGetStrict getVertex s
                             in f a x >>= flip go xs

instance Graph Shumov where
  type Vertex Shumov = ShumovVertex
  type Edges Shumov = ShumovEdges

  edges = error "Data.Graph.Shumov.edges"


{-# INLINE getVertex #-}
getVertex :: Get ShumovVertex
getVertex = do
  idSize <- getWord16be
  externalId <- getByteString (fromIntegral idSize)
  edgeCount <- getWord32be
  skip (fromIntegral edgeCount * 10)
  return ShumovVertex
    { _externalId = externalId
    }

readFile :: FilePath -> IO Shumov
readFile = fmap Shumov . BS.readFile

uncons :: Shumov -> Maybe (ShumovVertex, Shumov)
uncons (Shumov s) =
  if BS.null s
    then Nothing
    else let (v, r) = runGetStrict getVertex s
         in Just (v, Shumov r)
