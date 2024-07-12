{-# LANGUAGE InstanceSigs, LambdaCase #-}

module HaScheme where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P $ \case
    [] -> []
    (x:xs) -> [(x,xs)]

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \str ->
        case parse p str of
            [] -> []
            [(x,y)] -> [(f x, y)]

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = P $ \str -> [(x,str)]
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    f <*> p = P $ \str ->
        case parse f str of
            [] -> []
            [(x,y)] -> parse (fmap x p) y

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P $ \str ->
        case parse p str of
            [] -> []
            [(x,y)] -> parse (f x) y

instance Alternative Parser where
    empty :: Parser a
    empty = P $ const []
    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \str ->
        case parse p str of
            [] -> parse q str
            [(x,y)] -> [(x,y)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
    if p x then return x
           else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char = sat . (==)

string :: String -> Parser String
string []     = return []
string (x:xs) = char x >> string xs >> return (x:xs)

ident :: Parser String
ident = return (:) <*> lower <*> many alphanum

nat :: Parser Int
nat = read <$> some digit

space :: Parser ()
space = void . many $ sat isSpace

int :: Parser Int
int = char '-' >> negate <$> nat <|> nat

token :: Parser a -> Parser a
token p = space >> p >>= \x -> space >> return x

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token $ string xs

quote :: Parser a -> Parser a
quote p = symbol "'(" >> p >>= \x -> symbol ")" >> return x

parens :: Parser a -> Parser a
parens p = symbol "(" >> p >>= \x -> symbol ")" >> return x

nats :: Parser [Int]
nats = quote $ return (:) <*> natural <*> many natural

expr :: Parser Int
expr = parens $ do symbol "+" >> return (+) <*> expr' <*> expr'
            <|> do symbol "-" >> return (-) <*> expr' <*> expr'
            <|> do symbol "*" >> return (*) <*> expr' <*> expr'
    where expr' = expr <|> natural

eval :: String -> Int
eval xs = case parse expr xs of
    [(n,[])] -> n
    [(_,o)]  -> error $ "Unused input " ++ o
    []       -> error "Invalid input"
