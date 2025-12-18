{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Output (
    highlight, 
    printError, 
    printInfo, 
    printSeparator,
    repo,
    header
) where 


import RegEx (RegEx,regex)
import Parsing (parse)

import System.IO (hPutStrLn, stderr)

type Color = String

red, green, yellow, blue, cyan, reset :: Color 
red    = "\x1b[31m"
green  = "\x1b[32m"
yellow = "\x1b[33m"
blue   = "\x1b[34m"
cyan   = "\x1b[36m"
reset  = "\x1b[0m"

colorize :: Color -> String -> String
colorize color text = color ++ text ++ reset

highlight :: RegEx -> String -> String
highlight _ [] = []
highlight pattern input@(c:cs) = 
    case parse (regex pattern) input of
        [(m, rest)] | not (null m) -> colorize blue m ++ highlight pattern rest
        _ -> c : highlight pattern cs 

printError :: String -> IO ()
printError msg = hPutStrLn stderr $ colorize red "h-grep: " ++ msg

printInfo :: String -> IO ()
printInfo = putStrLn . colorize cyan

printSeparator :: String -> IO ()
printSeparator msg = 
    do 
        let n = div (100 - length msg) 2  
        putStrLn ""
        putStr (colorize cyan $ replicate n '=' ++ " " ++ msg ++ " " ++ replicate n '=') 
        putStrLn ""

header :: String
header = "Use: h-grep [OPTIONS] PATERN [FILE/PATH]"

repo :: String
repo = "More info in proyect repo: https://github.com/Buty800/h-grep"