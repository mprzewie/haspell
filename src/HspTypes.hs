module HspTypes (module HspTypes) where

type Phoneme = String

data Rule regexType = MkRule {regex :: [regexType], phones :: [Phoneme]} deriving Show

instance Eq r => Eq (Rule r) where
  MkRule a b == MkRule c d = a == c

instance Ord r => Ord (Rule r) where
  MkRule a b <= MkRule c d = a <= c

type PhonemeRule = Rule Char

type AliasRule = Rule Phoneme

-- data LangRule = MkLangRule {token :: String, phones :: [Phoneme]} deriving (Show,Eq)

data Alias = MkAlias {alias :: String, matches :: [Phoneme]} deriving (Show,Eq)

-- data AliasRule = MkAliasRule {regex::[Phoneme], output::[Phoneme]} deriving (Show,Eq)
