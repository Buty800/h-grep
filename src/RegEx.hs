module RegEx (RegEx, match, regex) where

import Parsing
import Data.Char
import  Data.Functor ((<&>))


ascii :: [Char]
ascii = ['\0'..'\127'] 

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
    (string "[d]" >> return (charClass [c | c <- ascii, isDigit c])) <|> 
    (string "[a]" >> return (charClass [c | c <- ascii, isAlpha c])) <|>
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

--Brzozowski derivative
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

--Longest prefix match
regex :: String -> Parser String
regex s = P $ \input ->
    case [ (prefix, suffix) | i <- reverse [0..length input], let (prefix, suffix) = splitAt i input, match rx prefix] of
        (x:_) -> [x]
        []    -> []
    where rx = read s
