module Main where

import System.Environment (getArgs) 
import Control.Monad (forM_)        
import RegEx (RegEx, match)

main :: IO ()
main = do

    args <- getArgs    
    case args of
        [pattern, filename] -> processFile pattern filename
        _                   -> putStrLn "Use: h-grep <patern> <file>"

processFile :: String -> String -> IO ()
processFile pattern filename = do

    let grepPatternString = ".*(" ++ pattern ++ ").*"

    let rx :: RegEx
        rx = read grepPatternString

    putStrLn $ "Looking for patern: " ++ pattern
    putStrLn $ "In file: " ++ filename
    putStrLn "---"

    content <- readFile filename
    
    let allLines = lines content
    
    let matchingLines = filter (match rx) allLines
    
    forM_ matchingLines putStrLn