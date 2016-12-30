module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Text (pack, unpack, splitOn)
import Data.Char (toLower)
import HspInterpreter (rules, LangRule(..))



phonemizeCMD :: String -> String -> IO [String]
phonemizeCMD lang inp = phonemize lang (return inp)


phonemize :: String -> IO String -> IO [String]
phonemize lang inp = do
        rlz <- rules lang
        i <- fmap (map toLower) inp
        if null i
            then return []
            else do
                rst <- phonemize lang (rest (match i rlz) i)
                res <- fmap (++rst) (return $ phones $ match i rlz)
                return (filter (/="") res)

rest :: LangRule -> String -> IO String
rest l s = return $ drop (length (token l)) s

match:: String -> [LangRule] -> LangRule
match _ [] = MkLangRule "" ["-"]
match s (x:xs) = 
    if matching x then x
    else match s xs
    where
        matching a = (length s >= length (token a))
                     && (foldr (&&) True (zipWith (==) s (token a)))



