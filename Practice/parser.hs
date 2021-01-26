{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char
import Prelude

data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show)

singleton :: a -> Tree a
singleton a = Node a Empty Empty

type ParseError = String
type ParseResult a = Either ParseError (String, a)

newtype Parser a = Parser
  { runParser :: String -> ParseResult a
  }

nom :: Parser Char
nom = 
    Parser $ 
        (
            \input -> case input of
                                [] -> Left "empty string"
                                (h:t) -> Right (t,h)
        )

charP :: Char -> Parser Char
charP c =
    Parser $
        (
            \input -> case input of
                                (h : t) | h==c -> Right (t,h)
                                (h : t) | h==c -> Right (t,h)
                                []                 -> Left "empty string"
        )

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
      case p input of
          Left error -> Left error
          Right (rest, a) -> Right (rest, f a)

instance Applicative Parser where
    pure a = Parser $ \input -> Right (input, a)
    (Parser pf) <*> (Parser pa) = Parser $ \i -> do
        (rest, f) <- pf i
        (rest2, a) <- pa rest
        return (rest2, f a)
        
    --   case pf i o
    --     Left err -> Left err
    --     Right (rest, f) -> case pa rest of
    --       Left err2 -> Left err2
    --       Right (rest2, a) -> Right (rest2, f a)

instance Monad Parser where
    (Parser pa) >>= apb = Parser $ \input -> do
        (rest, a) <- pa input
        runParser (apb a) rest

stringP :: String -> Parser String
stringP = mapM charP

instance Alternative Parser where
    empty = Parser $ \_ -> Left "empty parser"
    (Parser pa) <|> (Parser pb) = Parser $ \input -> do
        case pa input of
            Left error -> pb input
            result -> result

trueParser :: Parser Bool
trueParser = True <$ stringP "true"

falseParser :: Parser Bool
falseParser = False <$ stringP "false"

boolP :: Parser Bool
boolP = trueParser <|> falseParser

tryRead :: (Read a) => Parser String -> Parser a
tryRead (Parser ps) = Parser $ \input -> do
    (rest, val) <- ps input
    case reads val of
        [] -> Left "could not read parsed string"
        [(a, _)] -> Right (rest, a)

spanP :: (Char -> Bool) -> Parser String
spanP predicate = Parser $ \input ->
    let (matched, rest) = span predicate input
    in Right (rest, matched)

integerP :: Parser Integer
integerP = tryRead $ spanP isDigit

wsP :: Parser String
wsP = spanP isSpace

parseEmpty :: Parser (Tree a)
parseEmpty = Empty <$ charP '*'

-- parseNode :: Parser (Tree a)
-- parseNode parserA =

exampleP =
    (\_ _ a _ _ _ b _ _ -> a+b)
    <$> charP '('
    <*> wsP
    <*> integerP
    <*> wsP
    <*> charP ','
    <*> wsP
    <*> integerP
    <*> wsP
    <*> charP ')'
