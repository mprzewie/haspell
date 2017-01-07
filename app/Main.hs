module Main where

import System.Environment
import System.Exit

-- import Phonemizer (phonemize)
-- import Soundgluer (glueSpeech)


usage :: String
usage = "Usage: haspell LANGUAGE TEXT FILE"



main :: IO ()
main = do
    (language : text : filename) <- parseArgs =<< getArgs
    -- -> (phonemize language text) >>= \phones
    -- -> (glueSpeech filename phones)

parseArgs :: [String] -> IO [String]
parseArgs args
    | length args < 3 = invalidArgumentsError
    | otherwise       = return $ take 3 args

-- invalidArgumentsError :: a
invalidArgumentsError = do
    putStrLn usage
    exitFailure
