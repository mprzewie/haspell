module HspTypes (module HspTypes) where

type Phoneme = String

type Word = [Phoneme]

data LangRule = MkLangRule {token :: String, phones :: [Phoneme]} deriving (Show,Eq)

data Alias = MkAlias {alias :: String, matches :: [Phoneme]} deriving (Show,Eq)

data AliasRule = MkAliasRule {regex::[Phoneme], output::[Phoneme]} deriving (Show,Eq)
