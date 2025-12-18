module Main where
import RegEx (RegEx, match, line, consume)
import Output (highlight, printError, printInfo, printSeparator, header, repo)

import System.Environment (getArgs) 
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Exit (exitWith, ExitCode(..))
import System.Console.GetOpt
import Control.Monad (forM_, unless, when)
import Control.Exception (catch, IOException)
import Data.Time (getCurrentTime, diffUTCTime)

data Options = Options
    { optHelp        :: Bool
    , optLineNumbers :: Bool
    , optVerbose     :: Bool
    , optTime        :: Bool
    , optDebug       :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optHelp        = False
    , optLineNumbers = False
    , optVerbose     = False
    , optTime        = False
    , optDebug       = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]    (NoArg (\o -> o { optHelp = True }))        "Shows help message"
    , Option ['n'] ["number"]  (NoArg (\o -> o { optLineNumbers = True })) "Shows lines number"
    , Option ['v'] ["verbose"] (NoArg (\o -> o { optVerbose = True }))     "Shows runtime errors"
    , Option ['t'] ["time"]    (NoArg (\o -> o { optTime = True }))        "Shows total execution time"
    , Option ['d'] ["debug"]   (NoArg (\o -> o { optDebug = True }))       "Debug information"
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

    start <- getCurrentTime

    let process = if optDebug opts then debug else processFile 
    
    if optHelp opts then 
        printInfo $ usageInfo header options ++ repo
    else 
        case args of
            [pattern, path] -> recPath (process opts (read pattern)) path 
            [pattern]       -> recPath (process opts (read pattern)) "."
            _               -> printInfo header >> printInfo "Try 'h-grep -h' for more info"

    end <- getCurrentTime
    let time = diffUTCTime end start
    
    when (optTime opts) $ printInfo $ "\nTotal time: " ++ show time 

recPath :: (FilePath -> IO ()) -> FilePath -> IO ()
recPath f path = do
    isDir <- doesDirectoryExist path
    if isDir then do
        names <- listDirectory path 
        forM_ names $ recPath f . (path </>)
    else
        f path

debug :: Options -> RegEx -> FilePath  -> IO ()
debug opts pattern filename = catch (do 
    let rx = line pattern
    content <- readFile filename
    let allLines = lines content
    forM_ allLines $ print . consume rx  
    ) $ handleErrors opts filename


processFile :: Options -> RegEx -> FilePath  -> IO ()
processFile opts pattern filename = catch (do

    let rx = line pattern

    content <- readFile filename
    
    let allLines = lines content
    
    let highlightedLines = map (highlight pattern) allLines
    let numberedLines = zip [1 :: Int ..] highlightedLines
    let matchingLines = filter (match rx . snd) numberedLines
    let finalLines = if optLineNumbers opts 
        then map (\(i,s) -> show i ++ ": " ++ s) matchingLines
        else map snd matchingLines


    unless (null matchingLines) $ do
        printSeparator $ "Looking In File: " ++ filename 
        forM_ finalLines putStrLn

    
    ) $ handleErrors opts filename

handleErrors :: Options -> FilePath -> IOException -> IO ()
handleErrors opts filename e 
    | isDoesNotExistError e = printError $ filename ++ ": No such file or directory"
    | optVerbose opts = printError $ show e
    | otherwise = return ()