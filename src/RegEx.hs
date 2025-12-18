module RegEx (RegEx(..), (-:), match, line, nullable, consume, regex) where

import Parsing 

import Data.Char
import Data.List (intercalate)
import Data.Set (Set, member, toList, fromList, union, singleton, size, insert, partition)
import qualified Data.Set as Set 

data RegEx = 
    Void                    
    | Lambda                 
    | Union (Set RegEx)      
    | Concat [RegEx]         
    | Kleen RegEx            
    | Class (Set Char)
    deriving (Eq, Ord)

instance Show RegEx where
    show s 
        | s == ascii = "."
        | s == numbers = "\\d"
        | s == letters = "\\a" 
    show Void = "∅"
    show Lambda = "()"
    show (Union rxs) = "(" ++ intercalate "|" (map show (toList rxs)) ++ ")" 
    show (Concat rxs) = intercalate "" (map show rxs)
    show (Kleen rx) = show rx ++ "*"
    show (Class s) 
        | size s == 1 = [head (toList s)]
        | otherwise = "[" ++ show (toList s) ++ "]"  

character :: Char -> RegEx
character = Class . singleton

ascii :: RegEx
ascii = Class $ fromList ['\0'..'\127'] 

letters :: RegEx
letters = Class $ fromList $ filter isAlpha ['\0'..'\127']

numbers :: RegEx
numbers = Class $ fromList $ filter isDigit ['\0'..'\127']

line :: RegEx -> RegEx  
line rx = Concat [Kleen ascii , rx , Kleen ascii] 

--Regex Parser

-- term = (term | factor) | factor
-- factor = factor.term | factorterm |term
-- expr = term * | term + | term
-- primitive = [a..z] | [0..9] | ( expr ) | void
expr :: Parser RegEx
expr = term <| concatOp <| unionOp

term :: Parser RegEx
term =
    postfix macro (char '*') Kleen <|> 
    postfix macro (char '+') (\r -> Concat [r , Kleen r]) <|>
    postfix macro (char '?') (\r -> Union $ fromList [Lambda , r]) <|>
    macro

macro :: Parser RegEx
macro = 
    (char '.'     >> return ascii)   <|> 
    (string "\\d" >> return numbers) <|> 
    (string "\\a" >> return letters) <|>
    factor

factor :: Parser RegEx
factor = 
    (character <$> alphanum) <|> 
    (char '\\' >> character <$> item) <|> 
    (string "()" >> return Lambda) <|> between "(" expr ")" 

concatOp :: Parser (RegEx -> RegEx -> RegEx)
concatOp = return (\rx1 rx2 -> Concat [rx1, rx2])  

unionOp :: Parser (RegEx -> RegEx -> RegEx)
unionOp = char '|' >> return (\rx1 rx2 -> Union (fromList [rx1, rx2]))

instance Read RegEx where 
    readsPrec _ = parse expr

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

--Matching 

nullable :: RegEx -> Bool
nullable rx = case rx of
    Void -> False
    Lambda -> True
    Union rxs -> any nullable rxs
    Concat rxs -> all nullable rxs
    Kleen  _ -> True 
    Class _ -> False 

simplify :: RegEx -> RegEx
simplify (Concat rxs) = cleanConcat $ map simplify rxs
simplify (Union rxs) = cleanUnion $ Set.map simplify rxs 
simplify (Kleen rx) = case simplify rx of
    Void -> Lambda
    Lambda -> Lambda
    Kleen rx' -> rx' 
    rx' -> Kleen rx'
simplify (Class s) | null s = Void 
simplify rx = rx


cleanUnion :: Set RegEx -> RegEx
cleanUnion rxs = 
    let 
        flattened = flatUnion rxs
        filtered = Set.filter (/= Void) flattened
        merged = mergeUnion filtered
        -- valid = if any nullable merged then Set.filter (/= Lambda) merged else merged 
    in 
        case size merged of
            0 -> Void 
            1 -> head $ toList merged
            _ -> Union merged

flatUnion :: Set RegEx -> Set RegEx
flatUnion = Set.foldr merge Set.empty
    where 
        merge :: RegEx -> Set RegEx -> Set RegEx 
        merge (Union rxs1) rxs2 = rxs1 `union` rxs2 
        merge rx acc = insert rx acc 

mergeUnion :: Set RegEx -> Set RegEx 
mergeUnion rxs = 
    let 
        (toMerge, others) = partition isClass rxs
    in
        if null toMerge then 
            rxs
        else
            let 
                mergedClasses = Class $ Set.foldr (\r acc -> getChars r `union` acc) Set.empty toMerge
            in 
                insert mergedClasses others
    where
        isClass (Class _) = True 
        isClass _ = False 

        getChars (Class s) = s 
        getChars _ = Set.empty 



cleanConcat :: [RegEx] -> RegEx
cleanConcat [] = Lambda 
cleanConcat rxs 
    | Void `elem` rxs = Void
    | otherwise = 
    let 
        flattened = flatConcat rxs 
        filtered = filter (/= Lambda) flattened 
        merged = mergeConcat filtered 
    in 
        case merged of 
            [] -> Lambda 
            [rx] -> rx 
            list -> Concat list 


flatConcat :: [RegEx] -> [RegEx]
flatConcat = foldr merge []
    where 
        merge :: RegEx -> [RegEx] -> [RegEx] 
        merge (Concat rxs1) rxs2 = rxs1 ++ rxs2 
        merge rx acc = rx:acc 

mergeConcat :: [RegEx] -> [RegEx]
mergeConcat (Kleen rx1:Kleen rx2:rxs) | rx1 == rx2 = mergeConcat (Kleen rx1:rxs) 
mergeConcat (rx1:rxs) = rx1:mergeConcat rxs 
mergeConcat [] = [] 

-- Brzozowski derivative
(-:) :: RegEx -> Char -> RegEx
rx -: c = simplify $ case rx of
    Void -> Void
    Lambda -> Void
    Union rs -> Union $ Set.map (-: c) rs
    Concat [] -> Void   
    Concat (r:rs) -> 
        let 
            firstPart = Concat ((r -: c):rs)
        in 
            if nullable r then
                Union (fromList [firstPart, Concat rs -: c])
            else 
                firstPart 
    Kleen rx1 -> Concat [rx1 -: c , Kleen rx1] 
    Class s -> if member c s then Lambda else Void

consume :: RegEx -> String -> RegEx
consume = foldl (-:)

match :: RegEx -> String -> Bool
match rx = nullable . consume rx