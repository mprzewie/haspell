module Main where

import System.Environment
import System.Exit

import Phonemizer (phonemize)
import Soundgluer (glueSpeech)
import Speaker(speak, playFile)

-- | Usage info
usage :: String
usage = "Usage: " ++ mainCmd ++ spellMode ++" LANGUAGE [" ++ fileOption ++ "] INPUT OUTPUT\n\
        \     | " ++ mainCmd ++ utterMode ++" [LANGUAGE] INPUT"

-- | Command line switch indicating input from file.

mainCmd :: String
mainCmd = "stack-exec haspell-exe -- "

fileOption :: String
fileOption = "-f"

utterMode :: String
utterMode = "-u"

spellMode :: String
spellMode = "-s"

-- | Name of the default language used when no language is specified.
defaultLang :: String
defaultLang = "pol"

-- | Name of the default output file.
defaultOutput :: String
defaultOutput = "haspelled"


main :: IO ()
main = do
    args@(mode : lang : input : [output]) <- parseArgs =<< getArgs
    phones <- phonemize lang input
    putStrLn mode
    case (mode == spellMode) of
      True  -> spell lang input output
      False -> utter lang input

-- | Simple argument parsing.
parseArgs :: [String] -> IO [String]
parseArgs args = case args of
                    (mode : [input])                                      -> return                              $ mode               : defaultLang : input : [defaultOutput]
                    (mode : lang : [input])                               -> return                              $ mode               : lang        : input : [defaultOutput]
                    (mode : lang : fileOption : input  : output : _)      -> readFile input >>= \input -> return $ spellMode          : lang        : input : [output]
                    (mode : lang : input      : output : _)               -> return                              $ spellMode          : lang        : input : [output]
                    _                                                     -> invalidArgumentsError
     
-- | Prints usage and exits.               
invalidArgumentsError :: IO a
invalidArgumentsError = do
    putStrLn usage
    exitFailure

-- | Utility enabling spelling from GHCi.
spell :: String     -- ^ Language name matching a folder
      -> String     -- ^ Text to spell
      -> String     -- ^ Name of the output file
      -> IO ()
spell lang input output = do
    phones <- phonemize lang input
    glueSpeech lang phones output

utter :: String
      -> String
      -> IO()
utter lang input = do
    phones <- phonemize lang input
    speak lang phones 

-- | Utility enabling easier spelling from GHCi based on some defaults.
bitbox :: String    -- ^ Text to spell
       -> IO ()
bitbox input = spell defaultLang input defaultOutput
