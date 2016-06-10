{-# LANGUAGE DataKinds #-}

module Graphomania.Shumov
    ( ShumovBinary
    , readShumov
    , readShumovBE
    , readShumovLE
    , writeShumov
    , writeShumovBE
    , writeShumovLE
    ) where

import           Graphomania.Shumov.Internal
import           System.Endian

readShumovBE :: FilePath -> IO (ShumovBinary BigEndian)
readShumovBE = readShumov

readShumovLE :: FilePath -> IO (ShumovBinary LittleEndian)
readShumovLE = readShumov

writeShumovBE :: FilePath -> ShumovBinary BigEndian -> IO ()
writeShumovBE = writeShumov

writeShumovLE :: FilePath -> ShumovBinary LittleEndian -> IO ()
writeShumovLE = writeShumov
