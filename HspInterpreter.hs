module HspInterpreter (module HspInterpreter) where
-- for converting text into phones
import Data.Text (pack, unpack, splitOn)
import Data.Char (toLower)


data LangRule = MkLangRule {token :: String, phones :: [String]} deriving Show

phonemizeCMD :: String -> String -> IO [String]
phonemizeCMD lang inp = phonemize lang (return inp)


phonemize :: String -> IO String -> IO [String]
phonemize lang inp = do
        rlz <- rules lang
        i <- fmap (map toLower) inp
        if null i
            then return []
            else do
                rst <- phonemize lang (rest (match i rlz) i)
                res <- fmap (++rst) (return $ phones $ match i rlz)
                return (filter (/="") res)

rest :: LangRule -> String -> IO String
rest l s = return $ drop (length (token l)) s

match:: String -> [LangRule] -> LangRule
match _ [] = MkLangRule "" ["-"]
match s (x:xs) = 
    if matching x then x
    else match s xs
    where
        matching a = (length s >= length (token a))
                     && (foldr (&&) True (zipWith (==) s (token a)))

rules :: String -> IO [LangRule]
rules lang = (langRules "std")>>= \r -> fmap (++r++[MkLangRule " " ["-"]]) (langRules lang)

langRules :: String -> IO [LangRule]
langRules lang = fmap sortRules $ fmap  (map $ strToRule) rulesStringList 
    where 
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
        langName = fileCont >>= \f -> (return $ (lines' $ (splitStr "#" f) !! 1) !! 1)
        rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 2)

strToRule :: String -> LangRule

strToRule s = MkLangRule letter phones
    where
        letter = splitRule !! 0
        phones = splitStr "," $ splitRule !! 1
        splitRule = splitStr "->" $ despace s

lines' :: String -> [String]
lines' s = filter (/= "") $ lines s

splitStr :: String -> String -> [String]
splitStr regex input = Prelude.map unpack $ splitOn (pack regex) $ pack input

despace :: String -> String 
despace s = filter (/= ' ') s

sortRules :: [LangRule] -> [LangRule]
sortRules [] = []
sortRules xl = rl ++ ml ++ ll
    where
        piv = xl !! 0
        ll = sortRules [l | l <- xl, length (token l) < length (token piv)]
        ml = sortAlph [m | m <- xl, length (token m) == length (token piv)]
        rl = sortRules [r | r <- xl, length (token r) > length (token piv)]

sortAlph :: [LangRule] -> [LangRule]
sortAlph [] = []
sortAlph xl = ll ++ ml ++ rl
    where
        piv = xl !! 0
        ll = sortAlph [l | l <- xl,  token l < token piv]
        ml = [m | m <- xl, token m == token piv]
        rl = sortAlph [r | r <- xl, token r > token piv]
