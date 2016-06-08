module Graphomania.Main
  ( defaultMain
  ) where

import           Control.Monad
import           Graphomania.Convert
import           Options.Applicative
import           System.Endian

argsParser :: ParserInfo (IO ())
argsParser = info (helper <*> hsubparser convertCommand) fullDesc

data ConvertArgs = ConvertArgs
  { _convertFrom :: !GraphSource
  , _convertTo   :: !GraphSource
  } deriving ( Show )

convertCommand :: Mod CommandFields (IO ())
convertCommand =  command "convert" $ info ((\(ConvertArgs a b) -> convertGraph a b) <$> converArgsParser) fullDesc

converArgsParser :: Parser ConvertArgs
converArgsParser = ConvertArgs <$> graphSourceParser <*> graphSourceParser

graphSourceParser :: Parser GraphSource
graphSourceParser = ShumovGraphSource <$> subparser shumovBinarySourceCommand

shumovBinarySourceCommand :: Mod CommandFields ShumovBinarySource
shumovBinarySourceCommand = command "shumov" $ info shumovBinarySourceParser fullDesc

shumovBinarySourceParser :: Parser ShumovBinarySource
shumovBinarySourceParser = ShumovBinarySource
                        <$> argument str (metavar "FILE")
                        <*> argument endianness (metavar "ENDIANNESS")

endianness :: ReadM Endianness
endianness = eitherReader $ \arg -> case arg of
  "be" -> Right BigEndian
  "le" -> Right LittleEndian
  _    -> Left $ "cannot parse endianness value `" ++ arg ++ "'"

defaultMain :: IO ()
defaultMain = join $ execParser argsParser
