-- module HspInterpreter (module HspInterpreter) where
-- -- for converting text into phones
-- import Data.Text (pack, unpack, splitOn)
-- import Data.Char (toLower)


-- data LangRule = MkLangRule {token :: String, phones :: [String]} deriving Show

-- rules :: String -> IO [LangRule]
-- rules lang = (langRules "std")>>= \r -> fmap (++r++[MkLangRule " " ["-"]]) (langRules lang)

-- langRules :: String -> IO [LangRule]
-- langRules lang = fmap sortLangRules $ fmap  (map $ strToLangRule) rulesStringList 
--     where 
--         fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
--         langName = fileCont >>= \f -> (return $ (lines' $ (splitStr "#" f) !! 1) !! 1)
--         rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 2)

-- strToLangRule :: String -> LangRule
-- strToLangRule s = MkLangRule letter phones
--     where
--         letter = splitRule !! 0
--         phones = splitStr "," $ splitRule !! 1
--         splitRule = splitStr "->" $ despace s

-- lines' :: String -> [String]
-- lines' s = filter (/= "") $ lines s

-- splitStr :: String -> String -> [String]
-- splitStr regex input = Prelude.map unpack $ splitOn (pack regex) $ pack input

-- despace :: String -> String 
-- despace s = filter (/= ' ') s

-- sortLangRules :: [LangRule] -> [LangRule]
-- sortLangRules [] = []
-- sortLangRules xl = rl ++ ml ++ ll
--     where
--         piv = xl !! 0
--         ll = sortLangRules [l | l <- xl, length (token l) < length (token piv)]
--         ml = sortAlph [m | m <- xl, length (token m) == length (token piv)]
--         rl = sortLangRules [r | r <- xl, length (token r) > length (token piv)]

-- sortAlph :: [LangRule] -> [LangRule]
-- sortAlph [] = []
-- sortAlph xl = ll ++ ml ++ rl
--     where
--         piv = xl !! 0
--         ll = sortAlph [l | l <- xl,  token l < token piv]
--         ml = [m | m <- xl, token m == token piv]
--         rl = sortAlph [r | r <- xl, token r > token piv]


-- data Alias = MkAlias {alias :: String, matches :: [String]} deriving Show


-- strToAlias:: String -> Alias
-- strToAlias s = MkAlias alias matches
--     where
--         alias = splitRule !! 0
--         matches = splitStr "," $ splitRule !! 1
--         splitRule = splitStr "->" $ despace s

-- aliases :: String -> IO [Alias]
-- aliases lang = fmap  (map $ strToAlias) rulesStringList 
--     where 
--         fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
--         rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 3)

-- data AliasRule = MkAliasRule {regex::[String], output::[String]} deriving Show

-- aliasRulesAliased :: String -> IO [AliasRule]
-- aliasRulesAliased lang = fmap  (map $ strToAliasRule) rulesStringList 
--     where 
--         fileCont = readFile ("lang/" ++ lang ++ "/" ++ lang ++ ".hsp")
--         rulesStringList = fileCont >>= \f -> (return  $ tail $ lines' $ (splitStr "#" f) !! 4)


-- strToAliasRule :: String -> AliasRule
-- strToAliasRule s = MkAliasRule regex output
--     where
--         regex = splitStr "," $ splitRule !! 0
--         output = splitStr "," $ splitRule !! 1
--         splitRule = splitStr "->" $ despace s

-- unAliasRule :: [Alias] -> AliasRule -> [AliasRule]
-- unAliasRule alslist rule = aliasRuleMaker alslist [] (regex rule) (output rule) 0  
--     where
--         aliasRuleMaker als doneRegex toDoRegex output aliasNo 
--             | toDoRegex==[] = [MkAliasRule doneRegex output]
--             | head (toDoRegex!!0) == '<' = let 
--                         alias=toDoRegex!!0 
--                         mapStuff = map (\phn -> aliasRuleMaker als (doneRegex++[phn]) (tail toDoRegex) (replaceOccurences ("$"++(show aliasNo)) [phn] output) (aliasNo+1)) (matches (matchAlias als alias))
--                         in
--                         foldr (++) [] mapStuff
--             | otherwise = aliasRuleMaker als (doneRegex++[head toDoRegex]) (tail toDoRegex) output aliasNo
--         matchAlias als alsname = 
--             [x | x<-als, (alias x)==alsname]!!0

-- aliasRules :: String -> IO [AliasRule]
-- aliasRules lang = do
--                 rlz <- aliasRulesAliased "pol"
--                 als <- aliases "pol"
--                 return $ sortAliasRules $ foldr (++) [] (map (unAliasRule als) rlz)

-- sortAliasRules :: [AliasRule] -> [AliasRule]
-- sortAliasRules [] = []
-- sortAliasRules xl = rl ++ ml ++ ll
--     where
--         piv = xl !! 0
--         ll = sortAliasRules [l | l <- xl, length (regex l) < length (regex piv)]
--         ml = [m | m <- xl, length (regex m) == length (regex piv)]
--         rl = sortAliasRules [r | r <- xl, length (regex r) > length (regex piv)]

-- replaceOccurences :: Eq a => a -> [a] -> [a] -> [a]
-- replaceOccurences _ _ [] = []
-- replaceOccurences regex replacer (x:xs) 
--                     | x==regex = replacer++(replaceOccurences regex replacer xs)
--                     | otherwise = [x]++(replaceOccurences regex replacer xs)


