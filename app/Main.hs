module Main where

import System.Environment
import System.Exit

import Phonemizer (phonemize)
import Soundgluer (glueSpeech)
import Speaker(speak, playFile)

-- | Usage info
usage :: String
usage = "Usage: " ++ mainCmd ++ spellMode ++" LANGUAGE VOICE [" ++ fileOption ++ "] INPUT OUTPUT\n\
        \     | " ++ mainCmd ++ utterMode ++" [LANGUAGE] [VOICE] INPUT"

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

-- | Name of the default voice used when no voice is specified
defaultVoice :: String
defaultVoice = "luknw"

-- | Name of the default output file.
defaultOutput :: String
defaultOutput = "haspelled"


main :: IO ()
main = do
    args@(mode : lang : voice : input : [output]) <- parseArgs =<< getArgs
    phones <- phonemize lang input
    putStrLn mode
    case (mode == spellMode) of
      True  -> spell lang voice input output
      False -> utter lang voice input

-- | Simple argument parsing.
parseArgs :: [String] -> IO [String]
parseArgs args = case args of
                    (mode : [input])                                              -> return                              $ mode               : defaultLang : defaultVoice : input : [defaultOutput]
                    (mode : lang : voice : [input])                               -> return                              $ mode               : lang        : voice : input : [defaultOutput]
                    (mode : lang : voice : fileOption : input  : output : _)      -> readFile input >>= \input -> return $ spellMode          : lang        : voice : input : [output]
                    (mode : lang : voice : input      : output : _)               -> return                              $ spellMode          : lang        : voice : input : [output]
                    _                                                             -> invalidArgumentsError
     
-- | Prints usage and exits.               
invalidArgumentsError :: IO a
invalidArgumentsError = do
    putStrLn usage
    exitFailure

-- | Utility enabling spelling from GHCi.
spell :: String     -- ^ Language name matching a folder
      -> String     -- ^ Voice to spell with
      -> String     -- ^ Text to spell
      -> String     -- ^ Name of the output file
      -> IO ()
spell lang voice input output = do
    phones <- phonemize lang input
    glueSpeech voice phones output

utter :: String     -- ^ Language name matching a folder
      -> String     -- ^ Voice to spell with
      -> String     -- ^ Text to utter
      -> IO()
utter lang voice input = do
    phones <- phonemize lang input
    speak voice phones 

-- | Utility enabling easier spelling from GHCi based on some defaults.
bitbox :: String    -- ^ Text to spell
       -> IO ()
bitbox input = spell defaultLang defaultVoice input defaultOutput
