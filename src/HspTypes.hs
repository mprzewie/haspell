module HspTypes (module HspTypes) where

data LangRule = MkLangRule {token :: String, phones :: [Phone]} deriving (Show,Eq)
type Phone = String 

-- instance Foldable Phone where
--     foldMap _ "" = ""
--     foldMap f x = f x
    

data Alias = MkAlias {alias :: String, matches :: [Phone]} deriving (Show,Eq)

data AliasRule = MkAliasRule {regex::[Phone], output::[Phone]} deriving (Show,Eq)
