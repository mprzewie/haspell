module Main where

import System.Environment
import System.Exit

import Phonemizer (phonemize)
import Soundgluer (glueSpeech)

-- | Usage info
usage :: String
usage = "Usage: haspell LANGUAGE [" ++ fileOption ++ "] INPUT OUTPUT\n\
        \     | haspell [LANGUAGE] INPUT"

-- | Command line switch indicating input from file.
fileOption :: String
fileOption = "-f"

-- | Name of the default language used when no language is specified.
defaultLang :: String
defaultLang = "pol"

-- | Name of the default output file.
defaultOutput :: String
defaultOutput = "haspelled"


main :: IO ()
main = do
    args@(lang : input : [output]) <- parseArgs =<< getArgs
    phones <- phonemize lang input
    spell lang input output

-- | Simple argument parsing.
parseArgs :: [String] -> IO [String]
parseArgs args = case args of
                    [input]                                   -> return                              $ defaultLang : input : [defaultOutput]
                    (lang : [input])                          -> return                              $ lang        : input : [defaultOutput]
                    (lang : fileOption : input  : output : _) -> readFile input >>= \input -> return $ lang        : input : [output]
                    (lang : input      : output : _)          -> return                              $ lang        : input : [output]
                    _                                         -> invalidArgumentsError
     
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

-- | Utility enabling easier spelling from GHCi based on some defaults.
bitbox :: String    -- ^ Text to spell
       -> IO ()
bitbox input = spell defaultLang input defaultOutput
