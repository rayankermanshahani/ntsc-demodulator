module Main (main) where

import qualified Data.Vector.Unboxed as VU
import Options.Applicative
import System.IO

import NTSC.Audio
import NTSC.Color
import NTSC.Frame
import NTSC.Signal
import NTSC.Sync
import NTSC.Types

-- command line argument parsing and main entry point
datga Args = Args 
  { inputFile :: FilePath,
    outputFile :: FilePath
  }

main :: IO ()
main = do
  args <- parseArgs
  demodulateNTSC (inputFile args) (outputFile args)

demodulateNTSC :: FilePath -> FilePath -> IO ()
demodulateNTSC input output = do
  -- the full signal processing pipeline goes here:
  -- reading -> sync detection -> color processing -> output
  error "TODO"

parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc "NTSC Signal Demodulator"
     <> header "ntsc-demodulator - convert NTSC signals to decoded video/audio" )

argsParser :: Parser Args
argsParser = argsParser
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Input NTSC signal file" )
  <$> strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Output file for demodulated content" )
