module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Text


data LangRule = MkLangRule {txt::String, phones::[String]} deriving Show


rules:: String -> IO [String]
rules lang= rulesStringList
	
	where 
		fileCont = readFile ("lang/"++lang++"/"++lang++".hsp")
		langName = fileCont >>= \f -> (return $ (lines' $ (splitStr "#" f)!!1)!!1)
		rulesStringList = fileCont >>= \f -> (return  $ Prelude.tail $ lines' $ (splitStr "#" f)!!2)

strToRule::String -> LangRule
strToRule s = MkLangRule "korwo" ["mać"]

lines' :: String -> [String]
lines' s = Prelude.filter (\x -> not (x=="")) $ Prelude.lines s

splitStr :: String -> String -> [String]
splitStr regex input = Prelude.map unpack $ splitOn (pack regex) $ pack input
