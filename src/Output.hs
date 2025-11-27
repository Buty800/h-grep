{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Output (highlight, printError, printInfo) where 
import RegEx (RegEx, regex)
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
highlight pattern input = 
    case parse (regex pattern) input of
        [(m, rest)] | not (null m) -> colorize blue m ++ highlight pattern rest
        _ -> head input : highlight pattern (tail input)

printError :: String -> IO ()
printError msg = hPutStrLn stderr $ colorize red "h-grep error: " ++ msg

printInfo :: String -> IO ()
printInfo msg = putStrLn "" >> putStrLn (colorize cyan msg) >> putStrLn ""  