module RegEx (RegEx, match, regex, line) where

import Parsing
import Data.Char
import  Data.Functor ((<&>))


ascii :: [Char]
ascii = ['\0'..'\127'] 

letters :: [Char]
letters = [c | c <- ascii, isAlpha c]

numbers :: [Char]
numbers = [c | c <- ascii, isDigit c]

data RegEx = 
    Void                    --
    | Lambda                -- 
    | Symbol Char           -- 'a'
    | Union RegEx RegEx     -- a|b
    | Concat RegEx RegEx    -- ab
    | Kleen RegEx           -- a*
    deriving (Show,Eq)

charClass :: [Char] -> RegEx
charClass [] = Lambda
charClass s = foldr1 Union (map Symbol s)  

line :: RegEx -> RegEx
line rx = Concat (Kleen (charClass ascii)) $ Concat rx (Kleen (charClass ascii))

-- Parser

-- term = (term | factor) | factor
-- factor = factor.term | factorterm |term
-- expr = term * | term + | term
-- primitive = [a..z] | [0..9] | ( expr ) | void

expr :: Parser RegEx
expr = term <| concatOp <| unionOp

term :: Parser RegEx
term =
    runariy macro (char '*') Kleen <|> 
    runariy macro (char '+') (\r -> Concat r $ Kleen r) <|>
    runariy macro (char '?') (Union Lambda) <|>
    macro

macro :: Parser RegEx
macro = 
    (char '.' >> return (charClass ascii)) <|> 
    (string "[d]" >> return (charClass numbers)) <|> 
    (string "[a]" >> return (charClass letters)) <|>
    factor

factor :: Parser RegEx
factor = 
    (alphanum <&> Symbol) <|> 
    (char '\\' >> (item <&> Symbol)) <|> 
    (string "()" >> return Lambda) <|> between "(" expr ")" 

concatOp :: Parser (RegEx -> RegEx -> RegEx)
concatOp = return Concat  

unionOp :: Parser (RegEx -> RegEx -> RegEx)
unionOp = char '|' >> return Union

instance Read RegEx where 
    readsPrec _ = parse expr

--Matching 

nullable :: RegEx -> Bool
nullable rx = case rx of
    Void -> False
    Lambda -> True
    Symbol _ -> False
    Union rx1 rx2 -> nullable rx1 || nullable rx2
    Concat rx1 rx2 -> nullable rx1 && nullable rx2
    Kleen _ -> True 

simplify :: RegEx -> RegEx
simplify (Concat rx1 rx2) = case (simplify rx1, simplify rx2) of 
    (Void,_) -> Void
    (_,Void) -> Void
    (Lambda,rx) -> rx
    (rx,Lambda) -> rx
    (rx1',rx2') -> Concat rx1' rx2'
simplify (Union rx1 rx2) = case (simplify rx1, simplify rx2) of 
    (Void,r) -> r
    (r,Void) -> r
    (rx1',rx2') -> Union rx1' rx2'
simplify (Kleen rx) = case simplify rx of
    Void -> Lambda
    Lambda -> Lambda
    rx' -> Kleen rx'
simplify rx = rx

-- Brzozowski derivative
(-:) :: RegEx -> Char -> RegEx
rx -: c = simplify $ case rx of
    Void -> Void
    Lambda -> Void
    Symbol a -> if a==c then Lambda else Void
    Union rx1 rx2 -> Union (rx1 -: c) (rx2 -: c)
    Concat rx1 rx2 -> if nullable rx1 then Union (Concat (rx1-:c) rx2) (rx2-:c) else Concat (rx1-:c) rx2
    Kleen rx1 -> Concat (rx1 -: c) (Kleen rx1)

consume :: RegEx -> String -> RegEx
consume = foldl (-:)

match :: RegEx -> String -> Bool
match rx s = nullable (consume rx s)  

-- Longest prefix match
regex :: RegEx -> Parser String
regex rx = P $ \input ->
    let 
        states = scanl (-:) rx input        

        indexedStates = zip [0..] states
        
        -- Stop at Void State
        validStates = takeWhile (\(_, r) -> r /= Void) indexedStates        
        
        matches = [ i | (i, r) <- validStates, nullable r ]
    in 
        if null matches 
        then [] 
        else 
           let longestLen = last matches
               (prefix, suffix) = splitAt longestLen input
           in [(prefix, suffix)]
