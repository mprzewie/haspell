module QuickTest (module QuickTest) where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import HspInterpreter
import Phonemizer
import Soundgluer


qtest :: IO ()
qtest = do
    putStrLn "Checking phonemizer"
    quickCheck prop_phonemizer
    putStrLn "Checking matchLangRule"
    quickCheck prop_matchLangRule
    putStrLn "Checking replaceOccurences"
    quickCheck prop_replaceOccurences
    putStrLn "strToLangRule: x -> x"
    quickCheck prop_strToLangRule_xTox
    putStrLn "despace: works like filter"
    quickCheck prop_despace_likeFilter
    putStrLn "isWave: true for .wav files"
    quickCheck prop_isWave_trueForWav
    putStrLn "phoneName: drops .wav extension"
    quickCheck prop_phoneName_dropsWav
    putStrLn "getLangPath: prepends langDirectory + separator"
    quickCheck prop_getLangPath_prependsLangDirSep

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

prop_strToLangRule_xTox :: String -> Bool
prop_strToLangRule_xTox x =
    let x' = filter (/= ' ') . filter (/= ',') $ x
     in (MkLangRule x' [x']) == (strToLangRule $ x' ++ " -> " ++ x')

prop_despace_likeFilter :: String -> Bool
prop_despace_likeFilter x =
    filter (/= ' ') x == despace x

prop_isWave_trueForWav :: String -> Bool
prop_isWave_trueForWav x =
    isWave $ x ++ waveExtension

prop_phoneName_dropsWav :: String -> Bool
prop_phoneName_dropsWav x =
    x == phoneName (x ++ ".wav")

prop_getLangPath_prependsLangDirSep :: String -> Bool
prop_getLangPath_prependsLangDirSep x =
    (langsDirectory ++ pathSeparator ++ x) == getLangPath x
