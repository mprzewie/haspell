module HspInterpreter (module HspInterpreter) where
-- for converting text into phones
import Data.Text (pack, unpack, splitOn)
import Data.Char (toLower)
import HspTypes(PhonemeRule, AliasRule, Rule(..), Alias(..))

-- |List of phonemeRules found in .hsp file of language with given ID
phonemeRules :: String -- ^ ID of language - for example "pol"
   -> IO [PhonemeRule]

phonemeRules lang = fmap sortRules $ fmap  (map strToPhonemeRule) rulesStringList
    where
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ "_phones.hsp")
        rulesStringList = fileCont >>= \f -> return $ lines' f

-- |Helper method used to turn contents of .hsp file into phonemeRules
strToPhonemeRule :: String -- ^ String containing a PhonemeRule - for example "ni -> ni,i"
            -> PhonemeRule
strToPhonemeRule s = MkRule letter phones
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
splitStr regex input = map unpack $ splitOn (pack regex) $ pack input

-- |Filters all spaces out of a given String
despace :: String  -- ^ String to filter
            -> String
despace = filter (/= ' ')

-- |Sorts a list rules based on the length of their regexes
sortRules :: Ord r  => [Rule r] -- ^ List of rules to sort
                    -> [Rule r]
sortRules [] = []
sortRules xl = rl ++ ml ++ ll
    where
        piv = head xl
        ll = sortRules [l | l <- xl, length (regex l) < length (regex piv)]
        ml = [m | m <- xl, length (regex m) == length (regex piv)]
        rl = sortRules [r | r <- xl, length (regex r) > length (regex piv)]

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
aliases lang = fmap  (map strToAlias) rulesStringList
    where
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ "_alias.hsp")
        rulesStringList = fileCont >>= \f -> return $ lines' f


-- |List of aliasRules from given language (.hsp file)
aliasRulesAliased :: String -- ^ ID of language - for exaple "pol"
            -> IO [AliasRule]
aliasRulesAliased lang = fmap  (map strToAliasRule) rulesStringList
    where
        fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ "_aliasrules.hsp")
        rulesStringList = fileCont >>= \f -> return  $ lines' f

-- |Makes an aliasRule out of a String from .hsp file
strToAliasRule :: String -- ^ String containing an aliasRule - for example "<hardcon>,i,<vow> -> $0,j,$1"
            -> AliasRule
strToAliasRule s = MkRule regex output
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
unAliasRule alslist rule = aliasRuleMaker alslist [] (regex rule) (phones rule) 0
    where
        aliasRuleMaker als doneRegex toDoRegex output aliasNo
            | toDoRegex == [] = [MkRule doneRegex output]
            | head (head toDoRegex) == '<' = let
                        alias = head toDoRegex
                        mapStuff = map (\phn -> aliasRuleMaker als (doneRegex++[phn]) (tail toDoRegex) (replaceOccurences ("$" ++ show aliasNo) [phn] output) (aliasNo+1)) (matches (matchAlias als alias))
                        in
                        concat mapStuff
            | otherwise = aliasRuleMaker als (doneRegex++[head toDoRegex]) (tail toDoRegex) output aliasNo
        matchAlias als alsname =
            head [x | x<-als, alias x ==alsname]
            
-- |Returns a list of unaliased rules from given language (.hsp file)
aliasRules :: String  -- ^ ID of language - for exaple "pol"
           -> IO [AliasRule]
aliasRules lang = do
                rlz <- aliasRulesAliased lang
                als <- aliases lang
                return $ sortRules $ concatMap (unAliasRule als) rlz

-- |Replaces all of occurences of an element of a list with a list of other elements
replaceOccurences :: Eq a => a  -- ^ Element of list to replace
                    -> [a]  -- ^ List to replace regex with
                    -> [a]  -- ^ List to make replacements in
                    -> [a]
replaceOccurences _ _ [] = []
replaceOccurences regex replacer (x:xs)
                    | x==regex = replacer ++ replaceOccurences regex replacer xs
                    | otherwise = x : replaceOccurences regex replacer xs
