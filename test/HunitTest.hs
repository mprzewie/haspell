module HunitTest (module HunitTest) where

import Test.HUnit
import HspInterpreter
import Phonemizer
import Soundgluer


htest :: IO ()
htest = do
    phones1 <- (phonemize "pol" "przewięźlikowski")
    phones2 <- (phonemize "pol" "Litwo, ojczyzno moja, ty jesteś jak zdrowie")
    phones3 <- (phonemize "pol" "Jeżu klątw, spuść Finom część gry hańb!")
    let res1 = [["p","rz","e","w","i","en","zi","l","i","k","o","w","s","k","i"]]
    let res2 = [["l","i","t","w","o","-"],["o","j","cz","y","z","n","o"],["m","o","j","a","-"],["t","y"],["j","e","s","t","e","si"],["j","a","k"],["z","d","r","o","w","j","e"]]
    let res3 = [["j","e","rz","u"],["k","l","on","t","w","-"],["s","p","u","si","ci"],["f","i","n","o","m"],["cz","en","si","ci"],["g","r","y"],["h","a","ni","b","-"]]
    runTestTT $ TestList [
                -- Phonemizer.phonemize
                assertEqual' "przewięźlikowski" res1 phones1,
                assertEqual' "Litwo, ojczyzno..." res2 phones2,
                assertEqual' "Jeżu klątw, spuść Finom część gry hańb!" res3 phones3,
                -- HspInterpreter.strToLangRule
                assertEqual' "a -> a" (MkLangRule "a" ["a"]) (strToLangRule "a -> a"),
                assertEqual' "żółćęśąźń -> rz,u,ll,ci,en,si,on,zi,ni" (MkLangRule "żółćęśąźń" ["rz","u","ll","ci","en","si","on","zi","ni"]) (strToLangRule "żółćęśąźń -> rz,u,ll,ci,en,si,on,zi,ni"),
                -- HspInterpreter.despace
                assertEqual' "" "" (despace ""),
                assertEqual' " " "" (despace " "),
                assertEqual' "    " "" (despace "    "),
                assertEqual' "qwerty" "qwerty" (despace "qwerty"),
                assertEqual' " qwerty" "qwerty" (despace " qwerty"),
                assertEqual' "qwerty " "qwerty" (despace "qwerty "),
                assertEqual' " with Out Spaces   " "withOutSpaces" (despace " with Out Spaces   "),
                -- Soundgluer.isWave
                assertBool' ".wav" (isWave ".wav"),
                assertBool' "wav." (not $ isWave "wav."),
                assertBool' "" (not $ isWave ""),
                assertBool' "wav" (not $ isWave "wav"),
                -- Soundgluer.phoneName
                assertEqual' ".wav" "" (phoneName ".wav"),
                assertEqual' "a.wav" "a" (phoneName "a.wav"),
                assertEqual' "" "" (phoneName ""),
                assertEqual' "asdf.wav" "asdf" (phoneName "asdf.wav"),
                assertEqual' "żółćęśąźń.wav" "żółćęśąźń" (phoneName "żółćęśąźń.wav"),
                -- Soundgluer.getLangPath
                assertEqual' "pol" (langsDirectory ++ pathSeparator ++ "pol") (getLangPath "pol")
                ]
    
    putStrLn "Test suite not yet implemented"



assertEqual' :: (Eq a, Show a) => String -> a -> a -> Test
assertEqual' msg x y = TestCase $ assertEqual msg x y

assertBool' :: String -> Bool -> Test
assertBool' msg b = TestCase $ assertBool msg b

