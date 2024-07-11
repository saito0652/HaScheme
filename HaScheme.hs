{-# LANGUAGE InstanceSigs, LambdaCase #-}

module HaScheme where

import Control.Applicative
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
