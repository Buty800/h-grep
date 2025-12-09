-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p 

item :: Parser Char
item = P $ \inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)]

-- Sequencing parsers

instance Functor Parser where
   fmap g p = P $ \inp -> [ (g v, out) | (v, out) <- parse p inp ]

instance Applicative Parser where
   pure v = P $ \inp -> [(v,inp)]

   pg <*> px = P $ \inp -> concat [ parse (fmap g px) out | (g, out) <- parse pg inp ]

instance Monad Parser where
   p >>= f = P $ \inp -> concat [ parse (f v) out | (v, out) <- parse p inp ]

-- Making choices

instance Alternative Parser where
   empty = P $ const []

   p <|> q = P $ \inp -> case parse p inp of
                           []  -> parse q inp
                           res -> res

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

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
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = char x >> string xs >> return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = read <$> some digit

int :: Parser Int
int = (char '-' >> negate <$> nat) <|> nat

-- Handling spacing

space :: Parser ()
space = do 
    _ <- many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do 
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


--misc 

--Left asociative operators
(<|) :: Parser a -> Parser (a->a->a) -> Parser a
p <| op = p >>= rest 
    where
        rest x = do 
            f <- op
            y <- p
            rest $ f x y
            <|> return x

--Right unary operators
postfix :: Parser b -> Parser a ->  (b->b) -> Parser b
postfix p d f = do 
    x <- p
    _ <- d
    return $ f x

between :: String -> Parser a -> String -> Parser a
between open p close = do 
    _ <- string open
    x <- p 
    _ <- string close
    return x