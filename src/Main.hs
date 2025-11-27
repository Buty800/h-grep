module Main where
import RegEx (RegEx, match)

import System.Environment (getArgs) 
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Control.Exception (catch, IOException)

main :: IO ()
main = do

    args <- getArgs    
    case args of
        [pattern, path] -> processPath pattern path
        _               -> putStrLn "Use: h-grep <patern> <file>"

processPath :: String -> FilePath -> IO ()
processPath pattern path = do
    isDir <- doesDirectoryExist path
    if isDir 
    then do
        names <- listDirectory path 
        forM_ names $ \name -> processPath pattern (path </> name)
    else
        processFile pattern path

processFile :: String -> FilePath -> IO ()
processFile pattern filename = catch (do

    let grepPatternString = ".*(" ++ pattern ++ ").*"

    let rx :: RegEx
        rx = read grepPatternString

    content <- readFile filename
    let allLines = lines content
    let matchingLines = filter (match rx) allLines

    if null matchingLines then 
        return ()
    else do  
        putStrLn $ "Looking for patern: " ++ pattern
        putStrLn $ "In file: " ++ filename
        putStrLn "---"
        forM_ matchingLines putStrLn 
    
    ) handleErrors

handleErrors :: IOException -> IO ()
handleErrors _ = do
    return ()