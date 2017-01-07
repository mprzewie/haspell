module Main where

import System.Environment
import System.Exit

-- import qualified Data.Text.IO as T

import Phonemizer (phonemize)
-- import Soundgluer (glueSpeech)


usage :: String
usage = "Usage: haspell LANGUAGE INPUT OUTPUT"


main :: IO ()
main = do
    args@(language : input : output) <- parseArgs =<< getArgs
    phones <- phonemize language input
    print phones
    -- -> (phonemize language text) >>= \phones
    -- -> (glueSpeech filename phones)

parseArgs :: [String] -> IO [String]
parseArgs args
    | length args < 3 = invalidArgumentsError
    | otherwise       = return $ take 3 args

invalidArgumentsError :: IO a
invalidArgumentsError = do
    putStrLn usage
    exitFailure


-- GHCi support
spell :: String -> String -> String -> IO()
spell lang txt filename = (phonemize lang txt) >>= \phones -> print phones --(glueSpeech filename phones)

bitbox :: String -> IO()
bitbox txt = spell "pol" txt "bitbox"
