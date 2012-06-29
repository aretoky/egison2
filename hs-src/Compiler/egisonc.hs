module Main where
import Language.Egison.Core
import Language.Egison.Types
--import Language.Egison.Variables
import Control.Monad.Error
import System.Cmd (system)
import System.Console.GetOpt
import System.FilePath (dropExtension)
import System.Environment
import System.Directory (copyFile)
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.IO
import Paths_egison

templateFile :: String
templateFile = "etc/template.hs"

main :: IO ()
main = do 
  args <- getArgs
  let (actions, nonOpts, _) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options {optOutput = output} = opts
  if null nonOpts
     then showUsage
     else do
        let inFile = nonOpts !! 0
            outExec = case output of
              Just outFile -> outFile
              Nothing -> dropExtension inFile
        process inFile outExec

-- |Data type to handle command line options that take parameters
data Options = Options {
    optOutput :: Maybe String -- Executable file to write
    }

-- |Print a usage message
showUsage :: IO ()
showUsage = do
  putStrLn "egisonc: no input files"
  
-- |Default values for the command line options
defaultOptions :: Options
defaultOptions = Options {
    optOutput = Nothing
    }

options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['V'] ["version"] (NoArg showVersion) "show version number",
  Option ['h', '?'] ["help"] (NoArg showHelp) "show usage information",
  Option ['o'] ["output"] (ReqArg writeExec "FILE") "output file to write"
  ]

-- |Print version information
showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn Language.Egison.Core.egisonVersion
  exitWith ExitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
  putStrLn "Usage: egisonc [options] file"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help                Display this information"
  putStrLn "  --version             Display egison version information"
  putStrLn "  --output filename     Write executable to the given filename"
  putStrLn ""
  exitWith ExitSuccess

-- |Determine executable file to write. 
--  This version just takes a name from the command line option
writeExec arg opt = return opt { optOutput = Just arg }
  
-- |High level code to compile the given file
process :: String -> String -> IO ()
process inFile outExec = do
  result <- (runIOThrows $ liftM show $ createHaskellFile inFile outExec)
  case result of
   Just errMsg -> putStrLn errMsg
   _ -> compileHaskellFile outExec

createHaskellFile :: String -> String -> IOThrowsError ()
createHaskellFile inFile outExec = do
  templatePath <- liftIO $ getDataFileName templateFile
  liftIO $ copyFile templatePath "./_tmp.hs"
  egisonProgram <- liftIO $ readFile inFile
  let pLines = lines egisonProgram
  liftIO $ appendFile "./_tmp.hs" "\nprogram :: String\n"
  liftIO $ appendFile "./_tmp.hs" "program = "
  liftIO $ mapM_ (appendFile "./_tmp.hs") $ map (\pLine -> "  \"" ++ escapeDoubleQuote pLine ++ "\\n\" ++\n") pLines
  liftIO $ appendFile "./_tmp.hs" "  \"\"\n"
  return ()

escapeDoubleQuote :: String -> String
escapeDoubleQuote [] = []
escapeDoubleQuote (c:cs) = case c of
                             '"' -> '\\':'"':(escapeDoubleQuote cs)
                             _ -> c:(escapeDoubleQuote cs)
  
-- |Compile the intermediate haskell file using GHC
compileHaskellFile :: String -> IO()
compileHaskellFile filename = do
  let ghc = "ghc"
--  compileStatus <- system $ ghc ++ " " ++ " -cpp --make -package ghc -fglasgow-exts -o " ++ filename ++ " _tmp.hs"
  _ <- system $ ghc ++ " -o " ++ filename ++ " _tmp.hs"
  return ()
