{-# LANGUAGE InstanceSigs, LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module HaScheme where

import Control.Applicative
import Control.Monad
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Quote

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
ident = liftA2 (:) lower $ many alphanum

nat :: Parser Exp
nat = LitE . IntegerL . read <$> some digit

space :: Parser ()
space = void . many $ sat isSpace

int :: Parser Exp
int = do char '-' >> negate <$> nat
    <|> nat

token :: Parser a -> Parser a
token p = space >> p >>= \x -> space >> return x

identifier :: Parser String
identifier = token ident

natural :: Parser Exp
natural = token nat

integer :: Parser Exp
integer = token int

symbol :: String -> Parser String
symbol xs = token $ string xs

quote :: Parser a -> Parser a
quote p = symbol "'(" >> p >>= \x -> symbol ")" >> return x

parens :: Parser a -> Parser a
parens p = symbol "(" >> p >>= \x -> symbol ")" >> return x

nats :: Parser [Exp]
nats = quote $ liftA2 (:) natural $ many natural

instance Num Exp where
    (+),(-),(*) :: Exp -> Exp -> Exp
    x + y = ParensE $ UInfixE x (VarE $ mkName "+") y
    x - y = ParensE $ UInfixE x (VarE $ mkName "-") y
    x * y = ParensE $ UInfixE x (VarE $ mkName "*") y
    negate :: Exp -> Exp
    negate = AppE (VarE $ mkName "negate")

expr :: Parser Exp
expr = do parens $
            do symbol "+" >> liftA2 (+) expOrInt expOrInt
        <|> do symbol "-" >> liftA2 (-) expOrInt expOrInt
        <|> do symbol "*" >> liftA2 (*) expOrInt expOrInt
    <|> integer
    where expOrInt = expr <|> integer

eval :: String -> Exp
eval xs = case parse expr xs of
    [(n,[])] -> n
    [(_,o)]  -> error $ "Unused input " ++ o
    []       -> error "Invalid input"

scheme :: QuasiQuoter
scheme = QuasiQuoter { quoteExp = return . eval }
