module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Text


data LangRule = MkLangRule {txt::String, phones::[String]} deriving Show


rules:: String -> IO [LangRule]
rules lang= fmap (Prelude.map strToRule) rulesStringList 
	
	where 
		fileCont = readFile ("lang/"++lang++"/"++lang++".hsp")
		langName = fileCont >>= \f -> (return $ (lines' $ (splitStr "#" f)!!1)!!1)
		rulesStringList = fileCont >>= \f -> (return  $ Prelude.tail $ lines' $ (splitStr "#" f)!!2)

strToRule::String -> LangRule
strToRule s = MkLangRule letter phones
	where
		letter = splitRule!!0
		phones = splitStr "," $ splitRule!!1
		splitRule=splitStr "->" $ despace s

lines' :: String -> [String]
lines' s = Prelude.filter (\x -> not (x=="")) $ Prelude.lines s

splitStr :: String -> String -> [String]
splitStr regex input = Prelude.map unpack $ splitOn (pack regex) $ pack input

despace :: String -> String 
despace s = Prelude.filter (\x -> not(x==' ')) s

printList :: Show a => [a] => IO()
printList []=putStr ""
printList (x:xs) = print x >>printList xs

