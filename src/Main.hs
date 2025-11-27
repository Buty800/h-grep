module Main where
import RegEx (RegEx, match)
import Output (highlight, printError, printInfo)

import System.Environment (getArgs) 
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_, unless)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)

main :: IO ()
main = do

    args <- getArgs    
    case args of
        [pattern, path] -> processPath pattern path
        [pattern]       -> processPath pattern "."
        _               -> putStrLn "Use: h-grep <patern> [path/file]"

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

    unless (null matchingLines) $ do
        printInfo $ "Looking In File: " ++ filename 
        forM_ matchingLines $ putStrLn . highlight pattern

    
    ) $ handleErrors filename

handleErrors :: String -> IOException -> IO ()
handleErrors filename e 
    | isDoesNotExistError e = printError $ filename ++ ": No such file or directory"
    | otherwise = return ()