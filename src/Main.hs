module Main where
import RegEx (RegEx, match, line, debugConsume)
import Output (highlight, printError, printInfo, printSeparator, header,repo)

import System.Environment (getArgs) 
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_, unless)
import Control.Exception (catch, IOException)
import Data.Bifunctor (second)
import System.IO.Error (isDoesNotExistError)
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))

data Options = Options
    { optHelp        :: Bool
    , optLineNumbers :: Bool
    , optVerbose     :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optHelp        = False
    , optLineNumbers = False
    , optVerbose     = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]    (NoArg (\o -> o { optHelp = True }))        "Shows help message"
    , Option ['n'] ["number"]  (NoArg (\o -> o { optLineNumbers = True })) "Shows lines number"
    , Option ['v'] ["verbose"] (NoArg (\o -> o { optVerbose = True }))     "Shows runtime errors"
    ]

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv = 
    case getOpt Permute options argv of
        (actions, nonOptions, []) -> return (foldl (flip id) defaultOptions actions, nonOptions)
        _ -> printInfo header >> exitWith (ExitFailure 1)


main :: IO ()
main = do
    rawArgs <- getArgs
    (opts, args) <- parseArgs rawArgs

    if optHelp opts then 
        printInfo $ usageInfo header options ++ repo
    else 
        case args of
            [pattern, path] -> recPath (processFile opts $ read pattern) path 
            [pattern]       -> recPath (processFile opts $ read pattern) "."
            _               -> printInfo header >> printInfo "Try 'h-grep -h' for more info"

-- processPath :: Options -> RegEx -> FilePath -> IO ()
-- processPath opts pattern path = do
--     isDir <- doesDirectoryExist path
--     if isDir 
--     then do
--         names <- listDirectory path 
--         forM_ names $ \name -> processPath opts pattern (path </> name)
--     else
--         processFile pattern path


recPath :: (FilePath -> IO ()) -> FilePath -> IO ()
recPath f path = do
    isDir <- doesDirectoryExist path
    if isDir 
    then do
        names <- listDirectory path 
        forM_ names $ \name -> recPath f (path </> name) 
    else
        f path

-- debug :: Options -> RegEx -> FilePath -> IO ()
-- debug _ pattern filename = do 
--     let rx = line pattern
--     content <- readFile filename
--     let allLines = lines content
--     forM_ allLines (debugConsume rx) 

processFile :: Options -> RegEx -> FilePath  -> IO ()
processFile opts pattern filename = catch (do

    let rx = line pattern

    content <- readFile filename
    let allLines = zip [1 :: Int ..] $ lines content
    let matchingLines = filter (match rx . snd) allLines
    let highlightedLines = map (second $ highlight pattern) matchingLines
    let finalLines = if optLineNumbers opts 
        then map (\(i,s) -> show i ++ ": " ++ s) highlightedLines
        else map snd highlightedLines


    unless (null matchingLines) $ do
        printSeparator $ "Looking In File: " ++ filename 
        forM_ finalLines putStrLn

    
    ) $ handleErrors opts filename

handleErrors :: Options -> FilePath -> IOException -> IO ()
handleErrors opts filename e 
    | isDoesNotExistError e = printError $ filename ++ ": No such file or directory"
    | optVerbose opts = printError $ show e
    | otherwise = return ()