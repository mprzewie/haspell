module Phonemizer (module Phonemizer) where
--for converting text into phones
import HspTypes(Phoneme, Rule(..), PhonemeRule, AliasRule)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import HspInterpreter (langRules, aliasRules)

-- |Given the ID of language and a String returns a list of "words" - lists of phones
phonemize :: String -- ^ ID of language - for example "p
          -> String -- ^ String to translate into words
          -> IO [[Phoneme]]
phonemize lang inp = do
        let wrds = filter (/="") $ words (map toLower inp)
        als <- aliasRules lang
        rlz <- langRules lang
        let phnmzd = map (phonemize' rlz als) wrds
        return phnmzd
        where
            phonemize':: [PhonemeRule] -> [AliasRule] -> String-> [Phoneme]
            phonemize' r a i =
                let
                rest l = drop (length (regex l))
                in
                if null i
                then return []
                else do
                    let matched = fromMaybe (MkRule "" ["-"]) $ matchRule i r
                    let rst = phonemize' r a $ rest matched i
                    let res = phones matched ++ rst
                      in considerAliases [] a $ filter (/="") res


matchRule :: Eq r => [r]
                  -> [Rule r]
                  -> Maybe (Rule r)
matchRule s [] = Nothing
matchRule s (x:xs) =
  if matching x then Just x
  else matchRule s xs
  where
    matching rule = (length s >= length (regex rule))
                    && and (zipWith (==) s (regex rule))


-- |Modifies the list of phones based on AliasRules
considerAliases :: [AliasRule] -- ^ List of already considered aliasRules
                -> [AliasRule] -- ^ List of aliasRules to consider
                -> [Phoneme] -- ^ Word to modify based on aliasRules
                -> [Phoneme]
considerAliases _ _ []=[]
considerAliases already rules word = maybe didntmatch didmatch $ matchRule word rules
        where
            didntmatch = head word : considerAliases already rules (tail word)
            didmatch rule =
                if rule `elem` already
                    then error ("Infinite loop in language file in an AliasRule which applies to:\n" ++ concat (regex rule))
                else considerAliases (already++[rule]) rules $ phones rule ++ rest rule word
            rest aliasrule = drop (length (regex aliasrule))
