module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Char (toLower)
import HspInterpreter (rules, LangRule(..), AliasRule(..), aliasRules)


phonemize :: String -> String -> IO [[String]]
phonemize lang inp = do
        let wrds = filter (/="") $ words (map toLower inp)
        als <- aliasRules lang
        rlz <- rules lang
        let phnmzd = map (phonemize' rlz als) wrds
        return $ phnmzd
        --return $ foldr (\acc wrd -> acc>>= \a -> fmap (a++ wrd])) (return []) phnmzd
        where 
            phonemize':: [LangRule] -> [AliasRule] -> String-> [String]
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

matchLangRule:: String -> [LangRule] -> LangRule
matchLangRule s [] = MkLangRule [(head s)] ["-"]
matchLangRule s (x:xs) = 
    if matching x then x
    else matchLangRule s xs
    where
        matching a = (length s >= length (token a))
                     && (foldr (&&) True (zipWith (==) s (token a)))

considerAliases :: [AliasRule] -> [String]-> [String]
considerAliases _ []=[]
considerAliases rules phones= maybe didntmatch didmatch $ matchAliasRule phones rules
        where
            didntmatch=[head phones]++(considerAliases rules (tail phones))
            didmatch =(\rule -> considerAliases rules $ (output rule)++(rest rule phones))
            --didmatch =(\rule -> (output rule)++(considerAliases rules $ rest rule phones))
            rest aliasrule s = drop (length (regex aliasrule)) s


matchAliasRule :: [String] -> [AliasRule] -> Maybe AliasRule
matchAliasRule _ [] = Nothing
matchAliasRule s (x:xs) = 
    if matching x then Just x
    else matchAliasRule s xs
    where
        matching a = (length s >= length (regex a))
                     && (foldr (&&) True (zipWith (==) s (regex a)))
