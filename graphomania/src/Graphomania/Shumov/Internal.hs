{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphomania.Shumov.Internal
    ( ShumovBinary (..)
    , ShumovEdge (..)
    , ShumovVertexBinary (..)
    , readShumov
    , writeShumov
    , unsafeConvertShumov
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Int
import           Data.MonoTraversable
import           Data.Proxy
import           Data.Tagged
import           Graphomania.Utils.Binary
import           System.Endian

data ShumovEdge = ShumovEdge
  { _edgeTag :: !Int16
  , _nodeId  :: !Int64
  } deriving ( Show, Eq, Ord )

instance (EndiannessAware e) => Binary (Tagged e ShumovEdge) where
  get = do
    edgeTag <- untag <$> (get :: Get (Tagged e Int16))
    nodeId  <- untag <$> (get :: Get (Tagged e Int64))
    return $ Tagged $ ShumovEdge
      { _edgeTag = edgeTag
      , _nodeId  = nodeId
      }

  put (Tagged ShumovEdge {..}) = do
    put (Tagged _edgeTag :: Tagged e Int16)
    put (Tagged _nodeId  :: Tagged e Int64)

data ShumovVertexHeader = ShumovVertexHeader
  { _externalId :: !ByteString
  , _edgeCount  :: !Int32
  } deriving ( Show, Eq, Ord )

instance (EndiannessAware e) => Binary (Tagged e ShumovVertexHeader) where
  get = do
    idSize     <- untag <$> (get :: Get (Tagged e Int16))
    externalId <- getByteString (fromIntegral idSize)
    edgeCount  <- untag <$> (get :: Get (Tagged e Int32))
    return $ Tagged $ ShumovVertexHeader
      { _externalId = externalId
      , _edgeCount  = edgeCount
      }

  put (Tagged ShumovVertexHeader {..}) = do
    put (Tagged (fromIntegral $ BS.length _externalId) :: Tagged e Int16)
    putByteString _externalId
    put (Tagged _edgeCount :: Tagged e Int32)

data ShumovVertexBinary e = ShumovVertexBinary
  { _vertexHeader :: !ShumovVertexHeader
  , _edges        :: !ByteString
  } deriving ( Show, Eq, Ord ) -- TODO: redefine Show, Eq, Ord classes.

instance (EndiannessAware e) => Binary (ShumovVertexBinary e) where
  get = do
    vertexHeader <- untag <$> (get :: Get (Tagged e ShumovVertexHeader))
    edges        <- getByteString (fromIntegral $ _edgeCount vertexHeader * 10)
    return ShumovVertexBinary
      { _vertexHeader = vertexHeader
      , _edges        = edges
      }

  put ShumovVertexBinary {..} = do
    put (Tagged _vertexHeader :: Tagged e ShumovVertexHeader)
    putByteString _edges

newtype ShumovBinary e = ShumovBinary { unShumov :: ByteString }

type instance Element (ShumovBinary e) = ShumovVertexBinary e

instance (EndiannessAware e) => MonoFoldable (ShumovBinary e) where
  ofoldr f z = go z . unShumov
    where
      go z s | BS.null s = z
             | otherwise = let (x, xs) = runGetStrict get s
                           in f x (go z xs)

  ofoldMap = error "Data.Graph.Shumov.ofoldMap"
  ofoldr1Ex = error "Data.Graph.Shumov.ofoldr1Ex"
  ofoldl1Ex' = error "Data.Graph.Shumov.ofoldl1Ex'"

  ofoldl' f v = go v . unShumov
    where
      go !a !s | BS.null s = a
               | otherwise = let (x, xs) = runGetStrict get s
                             in go (f a x) xs

  ofoldlM f v = go v . unShumov
    where
      go !a !s | BS.null s = pure a
               | otherwise = let (x, xs) = runGetStrict get s
                             in f a x >>= flip go xs

readShumov :: (EndiannessAware e) => FilePath -> IO (ShumovBinary e)
readShumov = fmap ShumovBinary . BS.readFile

writeShumov :: (EndiannessAware e) => FilePath -> (ShumovBinary e) -> IO ()
writeShumov path = BS.writeFile path . unShumov

--TODO: consider streaming instead of inplace conversion

-- | Inplace edge encoding conversion
-- This function will rewrite source buffer and return unused part.
unsafeConverEdge :: forall from to . (EndiannessAware from, EndiannessAware to)
                 => Proxy from -> Proxy to -> ByteString -> IO ByteString
unsafeConverEdge _ _ bs = do
  let edge = untag $ fst $ runGetStrict (get :: Get (Tagged from ShumovEdge)) bs
  unsafePutStrict (put (Tagged edge :: (Tagged to ShumovEdge))) bs

unsafeConverVertex :: forall from to . (EndiannessAware from, EndiannessAware to)
                   => Proxy from -> Proxy to -> ByteString -> IO ByteString
unsafeConverVertex from to bs = do
  let vertexHeader = untag $ fst $ runGetStrict (get :: Get (Tagged from ShumovVertexHeader)) bs
  edgesS <- unsafePutStrict (put (Tagged vertexHeader :: (Tagged to ShumovVertexHeader))) bs
  foldM (const . unsafeConverEdge from to) edgesS [1 .. _edgeCount vertexHeader]

unsafeConvertShumov :: forall from to . (EndiannessAware from, EndiannessAware to)
                    => ShumovBinary from -> IO (ShumovBinary to)
unsafeConvertShumov ShumovBinary {..} = go unShumov
  where
    go s | BS.null s = return $ ShumovBinary unShumov
         | otherwise = unsafeConverVertex (Proxy :: Proxy from) (Proxy :: Proxy to) s >>= go
