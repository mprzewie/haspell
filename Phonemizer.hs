module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Char (toLower)
import HspInterpreter (rules, LangRule(..), AliasRule(..), aliasRules)



-- phonemizeCMD :: String -> String -> IO [String]
-- phonemizeCMD lang inp = phonemize lang inp


phonemize :: String -> String -> IO [String]
phonemize lang inp = do
        rlz <- rules lang
        let 
        	i = map toLower inp
        	rest l s = drop (length (token l)) s
        	in 
        	if null i
            then return []
            else do
                rst <- phonemize lang $ rest (matchLangRule i rlz) i
                als <- aliasRules lang
                let res = (phones $ matchLangRule i rlz) ++ rst
                 in return $ considerAliases als $ (filter (/="") res)

matchLangRule:: String -> [LangRule] -> LangRule
matchLangRule _ [] = MkLangRule "" ["-"]
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
			didmatch =(\rule -> (output rule)++(considerAliases rules $ rest rule phones))
			rest aliasrule s = drop (length (regex aliasrule)) s




matchAliasRule :: [String] -> [AliasRule] -> Maybe AliasRule
matchAliasRule _ [] = Nothing
matchAliasRule s (x:xs) = 
    if matching x then Just x
    else matchAliasRule s xs
    where
        matching a = (length s >= length (regex a))
                     && (foldr (&&) True (zipWith (==) s (regex a)))