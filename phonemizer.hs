module Phonemizer (module Phonemizer) where
-- for converting text into phones
import Data.Text (pack, unpack, splitOn)


data LangRule = MkLangRule {txt::String, phones::[String]} deriving Show

phonemizeCMD:: String -> String -> IO [String]
phonemizeCMD lang inp = phonemize lang (return inp)

phonemize:: String -> IO String -> IO [String]
phonemize lang inp=rlz>>= \r -> inp>>= \i -> 
			if i=="" then return $ []
			else pnmzrest i r >>= \rst -> fmap (++rst) (return $ phones ( match i r))
	where 
		rlz = rules lang
		pnmzrest i r= phonemize lang (rest (match i r) i)


rest:: LangRule -> String -> IO String
rest l s= return $ reverse $ take (length s - length (txt l)) $ reverse s

match:: String -> [LangRule] -> LangRule
match _ [] = MkLangRule "" ["-"]
match s (x:xs) = 
	if matching x then x
	else match s xs
	where
		matching a = (length s >= length (txt a))&&(foldr (&&) True (zipWith (==) s (txt a)))

rules:: String -> IO [LangRule]
rules lang= fmap (++[MkLangRule " " ["-"]]) $fmap sortRules $ fmap  (map $ strToRule) rulesStringList 
	
	where 
		fileCont = readFile ("lang/"++lang++"/"++lang++".hsp")
		langName = fileCont >>= \f -> (return $ (lines' $ (splitStr "#" f)!!1)!!1)
		rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f)!!2)

strToRule::String -> LangRule
strToRule s = MkLangRule letter phones
	where
		letter = splitRule!!0
		phones = splitStr "," $ splitRule!!1
		splitRule=splitStr "->" $ despace s

lines' :: String -> [String]
lines' s = filter (\x -> not (x=="")) $ lines s

splitStr :: String -> String -> [String]
splitStr regex input = Prelude.map unpack $ splitOn (pack regex) $ pack input

despace :: String -> String 
despace s = filter (\x -> not(x==' ')) s

printList :: Show a => [a] => IO()
printList []=putStr ""
printList (x:xs) = print x >>printList xs

sortRules :: [LangRule] -> [LangRule]
sortRules [] = []
sortRules xl = rl ++ ml ++ ll
	where
		piv= xl!!0
		ll = sortRules [l | l <- xl, (length (txt l))<(length (txt piv))]
		ml = sortAlph [m | m <- xl, (length (txt m))==(length (txt piv))]
		rl = sortRules [r | r <- xl, (length (txt r))>(length (txt piv))]

sortAlph :: [LangRule] -> [LangRule]
sortAlph [] = []
sortAlph xl = ll ++ ml ++ rl
	where
		piv= xl!!0
		ll = sortAlph [l | l <- xl,  (txt l)<(txt piv)]
		ml = [m | m <- xl, (txt m)==(txt piv)]
		rl = sortAlph [r | r <- xl, (txt r)>(txt piv)]

