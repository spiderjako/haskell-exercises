{-# LANGUAGE NamedFieldPuns #-}

module XMLParser where

import Control.Applicative
import Data.Char
import ParserUtils
import Prelude hiding (span)

type Attribute = (String, String)

data TagElement = TagElement
  { name :: String,
    attributes :: [Attribute],
    children :: [XMLObject]
  }
  deriving (Show, Read, Eq)

data XMLObject
  = Text String
  | Element TagElement
  deriving (Show, Read, Eq)

attributeParser :: Parser Attribute
attributeParser =
  (\n _ _ v _ -> (n, v))
    <$> (ws *> span (\a -> isAlphaNum a || isPunctuation a) <* ws)
    <*> char '='
    <*> (ws *> char '\"')
    <*> span (\a -> isAlphaNum a || (a /= '\"' && isPunctuation a) || isSpace a)
    <*> (char '\"' <* ws)

allAttributes :: Parser [Attribute]
allAttributes = many attributeParser

parseText :: Parser XMLObject
parseText = Text <$> atLeast (\a -> (isAlphaNum a) || (isSpace a) || (isPunctuation a) || (isSymbol a && a /= '<'))

parseElement :: Parser XMLObject
parseElement =
  (\_ n a _ c _ _ m _ -> Element TagElement {
    name=n, attributes=a, children=c
  })
    <$> (ws *> char '<' <* ws)
    <*> span (\a -> isAlphaNum a || (isPunctuation a && a /= '/'))
    <*> (ws *> allAttributes <* ws)
    <*> char '>'
    <*> (ws *> many xmlParser <* ws)
    <*> char '<'
    <*> (ws *> char '/')
    <*> (span (\a -> isAlphaNum a || (isPunctuation a && a /= '/')) <* ws)
    <*> (char '>' <* ws)

xmlParser :: Parser XMLObject
xmlParser = parseElement <|> parseText
