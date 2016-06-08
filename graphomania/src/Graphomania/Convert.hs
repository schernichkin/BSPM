module Graphomania.Convert
  ( Endianness (..)
  , GraphSource (..)
  , ShumovBinarySource (..)
  , convertGraph
  ) where

import           System.Endian

data GraphSource = ShumovGraphSource ShumovBinarySource deriving ( Show )

data ShumovBinarySource = ShumovBinarySource
  { _shumovBinaryFile       :: !FilePath
  , _shumovBinaryEndianness :: !Endianness
  } deriving ( Show )

convertGraph :: GraphSource -> GraphSource -> IO ()
convertGraph (ShumovGraphSource src) (ShumovGraphSource dst) = do
  -- TODO: Нужно поточное перекодирование файлов.
  print src
  print dst

convertGraph src dst = error $ "Conversion from `" ++ (show src)
                    ++ "' to `" ++ (show dst) ++ "' not supported."
