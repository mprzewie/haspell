module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Char (toLower)
import HspInterpreter (Phone,langRules, LangRule(..), AliasRule(..), aliasRules)

--given the ID of language and a String returns a list of "words" - lists of phones
phonemize :: String -> String -> IO [[Phone]]
phonemize lang inp = do
        let wrds = filter (/="") $ words (map toLower inp)
        als <- aliasRules lang
        rlz <- langRules lang
        let phnmzd = map (phonemize' rlz als) wrds
        return $ phnmzd
        where 
            phonemize':: [LangRule] -> [AliasRule] -> String-> [Phone]
            phonemize' r a i=
                let 
                rest l s = drop (length (token l)) s
                in 
                if null i
                then return []
                else do
                    let rst = phonemize' r a $ rest (matchLangRule i r) i
                    let res = (phones $ matchLangRule i r) ++ rst
                     in considerAliases a $ (filter (/="") res)

--given a String and a list of LangRules returns the LangRule whose token is the same as the beginning of the String
matchLangRule:: String -> [LangRule] -> LangRule
matchLangRule s [] = MkLangRule [(head s)] ["-"]
matchLangRule s (x:xs) = 
    if matching x then x
    else matchLangRule s xs
    where
        matching a = (length s >= length (token a))
                     && (foldr (&&) True (zipWith (==) s (token a)))

--modifies the list of phones based on AliasRules
considerAliases :: [AliasRule] -> [Phone]-> [Phone]
considerAliases _ []=[]
considerAliases rules phones= maybe didntmatch didmatch $ matchAliasRule phones rules
        where
            didntmatch=[head phones]++(considerAliases rules (tail phones))
            didmatch =(\rule -> considerAliases rules $ (output rule)++(rest rule phones))
            --didmatch =(\rule -> (output rule)++(considerAliases rules $ rest rule phones))
            rest aliasrule s = drop (length (regex aliasrule)) s

--given a word (a list of phones) and a list of AliasRules returns the AliasRule whose regex is the same as the beginning of the word
matchAliasRule :: [Phone] -> [AliasRule] -> Maybe AliasRule
matchAliasRule _ [] = Nothing
matchAliasRule s (x:xs) = 
    if matching x then Just x
    else matchAliasRule s xs
    where
        matching a = (length s >= length (regex a))
                     && (foldr (&&) True (zipWith (==) s (regex a)))
