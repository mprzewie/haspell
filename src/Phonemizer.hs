module Phonemizer (module Phonemizer) where
--for converting text into phones
import HspTypes(Phoneme, Rule(..), PhonemeRule, AliasRule)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import HspInterpreter (phonemeRules, aliasRules)

-- |Given the ID of language and a String returns a list of "words" - lists of phones
phonemize :: String -- ^ ID of language - for example "p
          -> String -- ^ String to translate into words
          -> IO [[Phoneme]]
phonemize lang inp = do
        let wrds = filter (/="") $ words (map toLower inp)
        alsrules <- aliasRules lang
        phnrules <- phonemeRules lang
        return $ map (applyAliasRules [] alsrules . applyPhonemeRules phnrules) wrds

applyPhonemeRules :: [PhonemeRule]
                  -> String
                  -> [Phoneme]
applyPhonemeRules _ "" = []
applyPhonemeRules rules word = filter (/= "") $ output matched ++ applyPhonemeRules rules rest where
  matched = fromMaybe (MkRule [head word] [""]) $ matchRule word rules
  rest = drop (length (regex matched)) word

-- |Modifies the list of phones based on AliasRules
applyAliasRules :: [AliasRule] -- ^ List of already considered aliasRules
                -> [AliasRule] -- ^ List of aliasRules to consider
                -> [Phoneme] -- ^ Word to modify based on aliasRules
                -> [Phoneme]
applyAliasRules _ _ []=[]
applyAliasRules already rules word = maybe didntmatch didmatch $ matchRule word rules
        where
            didntmatch = head word : applyAliasRules already rules (tail word)
            didmatch rule =
                if rule `elem` already
                    then error ("Infinite loop in language file in an AliasRule which applies to:\n" ++ concat (regex rule))
                else applyAliasRules (already++[rule]) rules $ output rule ++ rest rule word
            rest aliasrule = drop (length (regex aliasrule))


matchRule :: Eq r => [r]
                  -> [Rule r x]
                  -> Maybe (Rule r x)
matchRule s [] = Nothing
matchRule s (x:xs) =
  if matching x then Just x
  else matchRule s xs
  where
    matching rule = (length s >= length (regex rule))
                    && and (zipWith (==) s (regex rule))
