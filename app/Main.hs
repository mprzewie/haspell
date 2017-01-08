module Main where

import System.Environment
import System.Exit

import Phonemizer (phonemize)
import Soundgluer (glueSpeech)


usage :: String
usage = "Usage: haspell LANGUAGE [" ++ fileOption ++ "] INPUT OUTPUT\n\
        \     | haspell [LANGUAGE] INPUT"

fileOption :: String
fileOption = "-f"

defaultLang :: String
defaultLang = "pol"

defaultOutput :: String
defaultOutput = "haspelled"


main :: IO ()
main = do
    args@(lang : input : [output]) <- parseArgs =<< getArgs
    phones <- phonemize lang input
    spell lang input output

parseArgs :: [String] -> IO [String]
parseArgs args = case args of
                    [input]                                   -> return                              $ defaultLang : input : [defaultOutput]
                    (lang : [input])                          -> return                              $ lang        : input : [defaultOutput]
                    (lang : fileOption : input  : output : _) -> readFile input >>= \input -> return $ lang        : input : [output]
                    (lang : input      : output : _)          -> return                              $ lang        : input : [output]
                    _                                         -> invalidArgumentsError
                    
invalidArgumentsError :: IO a
invalidArgumentsError = do
    putStrLn usage
    exitFailure

-- GHCi support
spell :: String -> String -> String -> IO ()
spell lang input output = do
    phones <- phonemize lang input
    glueSpeech lang phones output

bitbox :: String -> IO ()
bitbox input = spell defaultLang input defaultOutput
