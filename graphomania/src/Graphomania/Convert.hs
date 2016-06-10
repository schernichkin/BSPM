module Graphomania.Convert
  ( Endianness (..)
  , GraphSource (..)
  , ShumovBinarySource (..)
  , convertGraph
  ) where

import           Graphomania.Shumov
import           Graphomania.Shumov.Internal
import           System.Endian

data GraphSource = ShumovGraphSource ShumovBinarySource deriving ( Show )

data ShumovBinarySource = ShumovBinarySource
  { _shumovBinaryFile       :: !FilePath
  , _shumovBinaryEndianness :: !Endianness
  } deriving ( Show )

convertGraph :: GraphSource -> GraphSource -> IO ()
convertGraph (ShumovGraphSource (ShumovBinarySource src BigEndian))
             (ShumovGraphSource (ShumovBinarySource dst LittleEndian)) = do
  shumovBE <- readShumovBE src
  shumovLE <- unsafeConvertShumov shumovBE
  writeShumovLE dst shumovLE

convertGraph src dst = error $ "Conversion from `" ++ (show src)
                    ++ "' to `" ++ (show dst) ++ "' not supported."
