module Main where
import RegEx (RegEx, match, line)
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
        [pattern, path] -> processPath (read pattern) path
        [pattern]       -> processPath (read pattern) "."
        _               -> putStrLn "Use: h-grep <patern> [path/file]"

processPath :: RegEx -> FilePath -> IO ()
processPath pattern path = do
    isDir <- doesDirectoryExist path
    if isDir 
    then do
        names <- listDirectory path 
        forM_ names $ \name -> processPath pattern (path </> name)
    else
        processFile pattern path

processFile :: RegEx -> FilePath -> IO ()
processFile pattern filename = catch (do

    let rx = line pattern

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