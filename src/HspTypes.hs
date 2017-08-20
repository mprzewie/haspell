module HspTypes (module HspTypes) where

type Phoneme = String

data Rule fromType toType = MkRule {regex :: [fromType], output :: [toType]} deriving Show

instance Eq r => Eq (Rule r x) where
  MkRule a b == MkRule c d = a == c

instance Ord r => Ord (Rule r x) where
  MkRule a b <= MkRule c d = a <= c

type PhonemeRule = Rule Char Phoneme

type AliasRule = Rule Phoneme Phoneme

-- data LangRule = MkLangRule {token :: String, phones :: [Phoneme]} deriving (Show,Eq)

data Alias = MkAlias {alias :: String, matches :: [Phoneme]} deriving (Show,Eq)

-- data AliasRule = MkAliasRule {regex::[Phoneme], output::[Phoneme]} deriving (Show,Eq)
