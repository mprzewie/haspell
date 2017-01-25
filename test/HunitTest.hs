module HunitTest (module HunitTest) where

import Test.HUnit
import HspInterpreter (Phone,langRules, LangRule(..), AliasRule(..), aliasRules)
import Phonemizer (phonemize)


htest :: IO ()
htest = do
	phones1 <-(phonemize "pol" "przewięźlikowski")
	phones2 <-(phonemize "pol" "Litwo, ojczyzno moja, ty jesteś jak zdrowie")
	let res1= [["p","rz","e","w","i","en","zi","l","i","k","o","w","s","k","i"]]
	let res2=[["l","i","t","w","o","-"],["o","j","cz","y","z","n","o"],["m","o","j","a","-"],["t","y"],["j","e","s","t","e","si"],["j","a","k"],["z","d","r","o","w","j","e"]]
	runTestTT $ TestList [assrtEqual phones1 res1,
				assrtEqual phones2 res2 ]
	
	putStrLn "Test suite not yet implemented"



assrtEqual :: (Eq a, Show a)=> a -> a -> Test
assrtEqual x y=TestCase $ assertEqual ("Should return "++(show x)) x y



