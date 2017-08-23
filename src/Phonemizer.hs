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
        return $ map ( filter (/= "") . applyNonCompulsoryRules [] alsrules . applyMappingRules phnrules [""]) wrds


applyMappingRules :: Eq f => [Rule f t]
                          -> [t]
                          -> [f]
                          -> [t]
applyMappingRules _ _ [] = []
applyMappingRules rules defaultResult toMap = output matched ++ applyMappingRules rules defaultResult rest where
  matched = fromMaybe (MkRule [head toMap] defaultResult) $ matchRule toMap rules
  rest = drop (length (regex matched)) toMap

-- |Modifies the list of phones based on AliasRules
applyNonCompulsoryRules :: Eq f => [Rule f f] -- ^ List of already considered aliasRules
                                -> [Rule f f] -- ^ List of aliasRules to consider
                                -> [f] -- ^ Word to modify based on aliasRules
                                -> [f]
applyNonCompulsoryRules _ _ []=[]
applyNonCompulsoryRules already rules toMap = maybe didntmatch didmatch $ matchRule toMap rules
        where
            didntmatch = head toMap : applyNonCompulsoryRules already rules (tail toMap)
            didmatch rule =
                if rule `elem` already
                    then error ("Infinite loop in language file in an noncompulsory rule!")
                else applyNonCompulsoryRules (already++[rule]) rules $ output rule ++ rest rule toMap
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
