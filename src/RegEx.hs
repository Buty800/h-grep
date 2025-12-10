module RegEx (RegEx, match, regex, line, debugConsume) where

import Parsing
import Data.Char
import Data.Set (Set, member, toList, fromList, union)


ascii :: Set Char
ascii = fromList ['\0'..'\127'] 

letters :: Set Char
letters = fromList $ filter isAlpha ['\0'..'\127']

numbers :: Set Char
numbers = fromList $ filter isDigit ['\0'..'\127']

data RegEx = 
    Void                    --
    | Lambda                -- 
    | Symbol Char           -- 'a'
    | Union RegEx RegEx     -- a|b
    | Concat RegEx RegEx    -- ab
    | Kleen RegEx           -- a*
    | Class (Set Char)
    deriving (Eq)

instance Show RegEx where
    show Void = "∅"
    show Lambda = "()"
    show (Symbol c) = [c]
    show (Union r1 r2) = "(" ++ show r1 ++ "|" ++ show r2 ++ ")"
    show (Concat r1 r2) = show r1 ++ show r2
    show (Kleen r) = show r ++ "*"
    show (Class s) 
        | s == ascii = "."
        | s == numbers = "\\d"
        | s == letters = "\\a"
        | otherwise = "[" ++ show (toList s) ++ "]"

line :: RegEx -> RegEx  
line rx = Concat (Kleen (Class ascii)) $ Concat rx (Kleen (Class ascii))

-- Parser

-- term = (term | factor) | factor
-- factor = factor.term | factorterm |term
-- expr = term * | term + | term
-- primitive = [a..z] | [0..9] | ( expr ) | void

expr :: Parser RegEx
expr = term <| concatOp <| unionOp

term :: Parser RegEx
term =
    postfix macro (char '*') Kleen <|> 
    postfix macro (char '+') (\r -> Concat r $ Kleen r) <|>
    postfix macro (char '?') (Union Lambda) <|>
    macro

macro :: Parser RegEx
macro = 
    (char '.' >> return (Class ascii)) <|> 
    (string "\\d" >> return (Class numbers)) <|> 
    (string "\\a" >> return (Class letters)) <|>
    factor

factor :: Parser RegEx
factor = 
    (Symbol <$> alphanum) <|> 
    (char '\\' >> (Symbol <$> item)) <|> 
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
    Kleen  _ -> True 
    Class _ -> False 

simplify :: RegEx -> RegEx
simplify (Concat rx1 rx2) = case (simplify rx1, simplify rx2) of 
    (Void,_) -> Void
    (_,Void) -> Void
    (Lambda,rx) -> rx
    (rx,Lambda) -> rx
    (Kleen rx1', Kleen rx2') | rx1' == rx2' -> Kleen rx1'
    (Class s1, Class s2) -> Class $ union s1 s2
    (rx1' , Concat rx2' rx3') -> simplify $ Concat (Concat rx1' rx2') rx3' 
    (rx1',rx2') -> Concat rx1' rx2'
simplify (Union rx1 rx2) = case (simplify rx1, simplify rx2) of 
    (Void,r) -> r
    (r,Void) -> r
    (Union rx1' rx2', rx3') -> simplify $ Union rx1' (Union rx2' rx3') 
    (rx1',rx2') | rx1' == rx2' -> rx1' 
                | otherwise -> Union rx1' rx2' 
simplify (Kleen rx) = case simplify rx of
    Void -> Lambda
    Lambda -> Lambda
    Kleen rx' -> rx' 
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
    Class s -> if member c s then Lambda else Void

consume :: RegEx -> String -> RegEx
consume = foldl (-:)

debugConsume :: RegEx -> String -> IO RegEx
debugConsume rx [] = print rx >> return rx 
debugConsume rx (c:cs) = debugConsume (rx -: c) cs

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
