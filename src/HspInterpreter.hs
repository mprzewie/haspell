module HspInterpreter (module HspInterpreter) where
-- for converting text into phones
import Data.Text (pack, unpack, splitOn)
import Data.Char (toLower)


data LangRule = MkLangRule {token :: String, phones :: [Phone]} deriving Show
type Phone = String

-- |List of langRules found in .hsp file of language with given ID
langRules :: String -- ^ ID of language - for example "pol"         
   -> IO [LangRule]
langRules lang = fmap sortLangRules $ fmap  (map $ strToLangRule) rulesStringList 
    where 
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
        langName = fileCont >>= \f -> (return $ (lines' $ (splitStr "#" f) !! 1) !! 1)
        rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 2)

-- |Helper method used to turn contents of .hsp file into langrules
strToLangRule :: String -- ^ String containing a LangRule - for example "ni -> ni,i"
            -> LangRule
strToLangRule s = MkLangRule letter phones
    where
        letter = splitRule !! 0
        phones = splitStr "," $ splitRule !! 1
        splitRule = splitStr "->" $ despace s

-- |Just like embedded 'lines' method, but omitting empty lines
lines' :: String -- ^ String containing newLine characters
        -> [String]
lines' s = filter (/= "") $ lines s

-- |Splitting String into a list based on a regex
splitStr :: String -- ^ String to split
            -> String -- ^ Regular expression to check for and split on
            -> [String]
splitStr regex input = Prelude.map unpack $ splitOn (pack regex) $ pack input

-- |Filters all spaces out of a given String
despace :: String  -- ^ String to filter
            -> String 
despace s = filter (/= ' ') s
        
-- |Sorts a list langRules according to length of their tokens firstly and alphabetically secondarily
sortLangRules :: [LangRule] -- ^ List of langRules to sort
                -> [LangRule]
sortLangRules [] = []
sortLangRules xl = rl ++ ml ++ ll
    where
        piv = xl !! 0
        ll = sortLangRules [l | l <- xl, length (token l) < length (token piv)]
        ml = sortAlph [m | m <- xl, length (token m) == length (token piv)]
        rl = sortLangRules [r | r <- xl, length (token r) > length (token piv)]

-- |Sorts a list of langRules alphabetically according to their tokens
sortAlph :: [LangRule] -- ^ List of langRules to sort
            -> [LangRule]
sortAlph [] = []
sortAlph xl = ll ++ ml ++ rl
    where
        piv = xl !! 0
        ll = sortAlph [l | l <- xl,  token l < token piv]
        ml = [m | m <- xl, token m == token piv]
        rl = sortAlph [r | r <- xl, token r > token piv]


data Alias = MkAlias {alias :: String, matches :: [Phone]} deriving Show

-- |Makes an alias out of a String from .hsp file
strToAlias:: String -- ^ String containing an Alias - for example "<vow> -> a,ą,e,ę,i,o,ó,u,y"
            -> Alias
strToAlias s = MkAlias alias matches
    where
        alias = splitRule !! 0
        matches = splitStr "," $ splitRule !! 1
        splitRule = splitStr "->" $ despace s

-- |List of aliases from given language (.hsp file)
aliases :: String -- ^ ID of language - for exaple "pol"
        -> IO [Alias]
aliases lang = fmap  (map $ strToAlias) rulesStringList 
    where 
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
        rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 3)

data AliasRule = MkAliasRule {regex::[Phone], output::[Phone]} deriving Show

-- |List of aliasRules from given language (.hsp file)
aliasRulesAliased :: String -- ^ ID of language - for exaple "pol"
            -> IO [AliasRule]
aliasRulesAliased lang = fmap  (map $ strToAliasRule) rulesStringList 
    where 
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
        rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 4)

-- |Makes an aliasRule out of a String from .hsp file
strToAliasRule :: String -- ^ String containing an aliasRule - for example "<hardcon>,i,<vow> -> $0,j,$1"
            -> AliasRule
strToAliasRule s = MkAliasRule regex output
    where
        regex = splitStr "," $ splitRule !! 0
        output = splitStr "," $ splitRule !! 1
        splitRule = splitStr "->" $ despace s

-- |Generates a list of aliasRules from the given unaliased aliasRule:
-- example:
--
-- @
--
-- unAliasRule [{alias="vovel"}, matches=["a,e,i,o,u,y"]] {regex=[<vovel>,"i"], output=["$0","j"]} 
-- 
-- would produce:
--
-- [{regex=["a","i"], output=["a","j"]},
-- 
-- {regex=["e","i"], output=["e","j"]},
-- 
-- {regex=["i","i"], output=["i","j"]},
-- 
-- {regex=["o","i"], output=["o","j"]},
-- 
-- {regex=["u","i"], output=["u","j"]},
-- 
-- {regex=["y","i"], output=["y","j"]}]
--
-- @
unAliasRule :: [Alias] -- ^ List of aliases to consider
            -> AliasRule -- ^ aliasRule to unAlias
            -> [AliasRule]
unAliasRule alslist rule = aliasRuleMaker alslist [] (regex rule) (output rule) 0  
    where
        aliasRuleMaker als doneRegex toDoRegex output aliasNo 
            | toDoRegex==[] = [MkAliasRule doneRegex output]
            | head (toDoRegex!!0) == '<' = let 
                        alias=toDoRegex!!0 
                        mapStuff = map (\phn -> aliasRuleMaker als (doneRegex++[phn]) (tail toDoRegex) (replaceOccurences ("$"++(show aliasNo)) [phn] output) (aliasNo+1)) (matches (matchAlias als alias))
                        in
                        foldr (++) [] mapStuff
            | otherwise = aliasRuleMaker als (doneRegex++[head toDoRegex]) (tail toDoRegex) output aliasNo
        matchAlias als alsname = 
            [x | x<-als, (alias x)==alsname]!!0
-- |Returns a list of unaliased rules from given language (.hsp file)
aliasRules :: String  -- ^ ID of language - for exaple "pol"
           -> IO [AliasRule]
aliasRules lang = do
                rlz <- aliasRulesAliased lang
                als <- aliases lang
                return $ sortAliasRules $ foldr (++) [] (map (unAliasRule als) rlz)
-- |Sorts a list of aliasRules according to length of regex
sortAliasRules :: [AliasRule] -- ^ List of aliasRules to sort
             -> [AliasRule]
sortAliasRules [] = []
sortAliasRules xl = rl ++ ml ++ ll
    where
        piv = xl !! 0
        ll = sortAliasRules [l | l <- xl, length (regex l) < length (regex piv)]
        ml = [m | m <- xl, length (regex m) == length (regex piv)]
        rl = sortAliasRules [r | r <- xl, length (regex r) > length (regex piv)]

-- |Replaces all of occurences of an element of a list with a list of other elements
replaceOccurences :: Eq a => a  -- ^ Element of list to replace
                    -> [a]  -- ^ List to replace regex with
                    -> [a]  -- ^ List to make replacements in
                    -> [a]
replaceOccurences _ _ [] = []
replaceOccurences regex replacer (x:xs) 
                    | x==regex = replacer++(replaceOccurences regex replacer xs)
                    | otherwise = [x]++(replaceOccurences regex replacer xs)


