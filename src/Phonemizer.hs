module Phonemizer (module Phonemizer) where
--for converting text into phones
import HspTypes(Phone, LangRule(..), AliasRule(..))
import Data.Char (toLower)
import HspInterpreter (langRules, aliasRules)

-- |Given the ID of language and a String returns a list of "words" - lists of phones
phonemize :: String -- ^ ID of language - for example "p
          -> String -- ^ String to translate into words
          -> IO [[Phone]]
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
                     in considerAliases [] a $ (filter (/="") res)

-- |Given a String and a list of LangRules returns the LangRule whose token is the same as the beginning of the String
matchLangRule:: String -- ^ String to match the token of langRule to
			 -> [LangRule] -- ^ List of available langRules to match to
			  -> LangRule -- ^ Matched langRule
matchLangRule s [] = MkLangRule [(head s)] ["-"]
matchLangRule s (x:xs) = 
    if matching x then x
    else matchLangRule s xs
    where
        matching a = (length s >= length (token a))
                     && (foldr (&&) True (zipWith (==) s (token a)))

-- |Modifies the list of phones based on AliasRules
considerAliases :: [AliasRule] -- ^ List of already considered aliasRules
                -> [AliasRule] -- ^ List of aliasRules to consider 
				-> [Phone] -- ^ Word to modify based on aliasRules
				-> [Phone]
considerAliases _ _ []=[]
considerAliases already rules phones = maybe didntmatch didmatch $ matchAliasRule phones rules
        where
            didntmatch=[head phones]++(considerAliases already rules (tail phones))
            didmatch =(\rule -> 
                if rule `elem` already 
                    then error ("Infinite loop in language file in an AliasRule which applies to:\n" ++ (foldr (++) "" $ regex rule))
                else considerAliases (already++[rule]) rules $ (output rule)++(rest rule phones))
            rest aliasrule s = drop (length (regex aliasrule)) s

-- |Given a word (a list of phones) and a list of AliasRules returns the AliasRule whose regex is the same as the beginning of the word
matchAliasRule :: [Phone]  -- ^ List of phones to match the token of aliasRule to
				-> [AliasRule] -- ^ List of available aliasRules to consider
				-> Maybe AliasRule -- ^ Matched langRule
matchAliasRule _ [] = Nothing
matchAliasRule s (x:xs) = 
    if matching x then Just x
    else matchAliasRule s xs
    where
        matching a = (length s >= length (regex a))
                     && (foldr (&&) True (zipWith (==) s (regex a)))
