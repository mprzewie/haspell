module HspInterpreter (module HspInterpreter) where
-- for converting text into phones
import Data.Text (pack, unpack, splitOn)
import Data.Char (toLower)
import HspTypes(PhonemeRule, AliasRule, Rule(..), Alias(..))

readHspFile :: String
            -> IO [(String, String)]
readHspFile path = do
  fileCont <- readFile path
  let rawLines = filter (/= "") $ lines fileCont
  let parsedLines = filter (/= "") $ map (\line -> despace $ head $ splitStr "#" line) rawLines
  return $ map (\line -> (splitStr "->" line !!0 , splitStr "->" line !! 1)) parsedLines


-- |List of phonemeRules found in .hsp file of language with given ID
phonemeRules :: String -- ^ ID of language - for example "pol"
   -> IO [PhonemeRule]
phonemeRules lang = fmap sortRules $ fmap  (map tupleToPhonemeRule) rulesTupleList
    where
        rulesTupleList =  readHspFile ("lang/" ++ lang ++ "/" ++ lang ++ "_phones.hsp")

tupleToPhonemeRule :: (String, String)
                            -> PhonemeRule
tupleToPhonemeRule (regex, phonesInStr) = MkRule regex (splitStr "," phonesInStr)

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
sortRules :: Ord r  => [Rule r x] -- ^ List of rules to sort
                    -> [Rule r x]
sortRules [] = []
sortRules xl = rl ++ ml ++ ll
    where
        piv = head xl
        ll = sortRules [l | l <- xl, length (regex l) < length (regex piv)]
        ml = [m | m <- xl, length (regex m) == length (regex piv)]
        rl = sortRules [r | r <- xl, length (regex r) > length (regex piv)]

-- |Makes an alias out of a String from .hsp file
tupleToAlias:: (String, String) -- ^ String containing an Alias - for example "<vow> -> a,ą,e,ę,i,o,ó,u,y"
            -> Alias
tupleToAlias (alias, matchesStr) = MkAlias alias (splitStr "," matchesStr)

-- |List of aliases from given language (.hsp file)
aliases :: String -- ^ ID of language - for exaple "pol"
        -> IO [Alias]
aliases lang = fmap  (map tupleToAlias) aliasesTupleList
    where
        aliasesTupleList = readHspFile ("lang/" ++ lang ++ "/" ++ lang ++ "_alias.hsp")


-- |List of aliasRules from given language (.hsp file)
aliasRulesAliased :: String -- ^ ID of language - for exaple "pol"
            -> IO [AliasRule]
aliasRulesAliased lang = fmap  (map tupleToAliasRule) rulesTupleList
    where
        rulesTupleList = readHspFile ("lang/" ++ lang ++ "/" ++ lang ++ "_aliasrules.hsp")
-- |Makes an aliasRule out of a String from .hsp file
tupleToAliasRule :: (String, String) -- ^ String containing an aliasRule - for example "<hardcon>,i,<vow> -> $0,j,$1"
            -> AliasRule
tupleToAliasRule (regexStr, outputStr) = MkRule (splitStr "," regexStr) (splitStr "," outputStr)

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
