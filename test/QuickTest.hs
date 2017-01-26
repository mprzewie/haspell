module QuickTest (module QuickTest) where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import HspInterpreter (Phone,langRules, LangRule(..), AliasRule(..), aliasRules, replaceOccurences)
import Phonemizer (phonemize,matchLangRule)


qtest :: IO ()
qtest = do
	putStrLn "Checking phonemizer"
	quickCheck prop_phonemizer
	putStrLn "Checking matchLangRule"
	quickCheck prop_matchLangRule
	putStrLn "Checking replaceOccurences"
	quickCheck prop_replaceOccurences

prop_phonemizer :: String -> Property
prop_phonemizer s = monadicIO $ do
			phnmzd <- run (phonemize "pol" s)
			let wrds= words s
			assert ((length phnmzd)==(length wrds))

prop_matchLangRule :: String -> String -> [String] -> Bool
prop_matchLangRule a b c = 
			let 
			string=a++b 
			rule=MkLangRule a c
			in ((matchLangRule string [rule])==rule)

prop_replaceOccurences :: String -> [String] -> [String] -> Bool
prop_replaceOccurences x y z=((x `elem` y) || not (x `elem` (replaceOccurences x y z)))