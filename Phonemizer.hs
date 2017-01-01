module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Char (toLower)
import HspInterpreter (rules, LangRule(..))



-- phonemizeCMD :: String -> String -> IO [String]
-- phonemizeCMD lang inp = phonemize lang inp


phonemize :: String -> String -> IO [String]
phonemize lang inp = do
        rlz <- rules lang
        let i = map toLower inp
         in if null i
            then return []
            else do
                rst <- phonemize lang $ rest (match i rlz) i
                let res = (phones $ match i rlz) ++ rst
                 in return (filter (/="") res)

rest :: LangRule -> String -> String
rest l s = drop (length (token l)) s

match:: String -> [LangRule] -> LangRule
match _ [] = MkLangRule "" ["-"]
match s (x:xs) = 
    if matching x then x
    else match s xs
    where
        matching a = (length s >= length (token a))
                     && (foldr (&&) True (zipWith (==) s (token a)))



