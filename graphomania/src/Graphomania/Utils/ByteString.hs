module Graphomania.Utils.ByteString ( unsafePutBuilder ) where

import           Control.Exception
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.ByteString.Internal
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr

{-# NOINLINE moduleError #-}
moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)

{-# NOINLINE moduleErrorIO #-}
moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Graphomania.Utils.ByteString." ++ fun ++ ':':' ':msg

-- | Write builder content to specified bytestring and return unused part of string.
-- Since bytestring content consideted immutable this is unsafe operation.
unsafePutBuilder ::  Builder -> ByteString -> IO ByteString
unsafePutBuilder b s = withForeignPtr fptr $ \ptr ->  go (runBuilder b) (ptr `plusPtr` off) len
   where
    (fptr, off, len) = toForeignPtr s
    go writter ptr size = do
      (written, next) <- writter ptr size
      let remains = size - written
          newPos = ptr `plusPtr` written :: Ptr Word8
      case next of
        Done ->
          return $ fromForeignPtr fptr (off + len - remains) remains
        More minSize nextWritter ->
          if minSize <= remains
            then go nextWritter newPos remains
            else moduleErrorIO "unsafePutBuilder" "Writer has requested more space than available in buffer."
        Chunk chunk nextWritter -> do
          let (cfptr, coff, clen) = toForeignPtr chunk
          if clen <= remains
            then do
              withForeignPtr cfptr $ \cptr -> moveArray newPos (cptr `plusPtr` coff) clen
              go nextWritter (newPos `plusPtr` clen) remains
            else moduleErrorIO "unsafePutBuilder" "Writer has retuned chunk which size exceeded buffer size."
