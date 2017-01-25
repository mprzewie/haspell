module QuickTest (module QuickTest) where

import Test.QuickCheck
import HspInterpreter (Phone,langRules, LangRule(..), AliasRule(..), aliasRules)
import Phonemizer (phonemize)


qtest :: IO ()
qtest = do
	quickCheck (1==1 ::Bool)






