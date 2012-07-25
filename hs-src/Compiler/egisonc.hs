module Main where
import Language.Egison.Core
import Language.Egison.Types
import Language.Egison.Parser
import Control.Monad.Error
import System.Cmd (system)
import System.Console.GetOpt
import System.FilePath (dropExtension)
import System.Environment
import System.Directory (doesFileExist, copyFile, removeFile)
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.IO
import Paths_egison

templateFile :: String
templateFile = "etc/template.hs"

main :: IO ()
main = do 
  args <- getArgs
  let (actions, nonOpts, _) = getOpt Permute options args
  let opts = foldl (flip id) defaultOptions actions
  case opts of
    Options {optShowHelp = True} -> printHelp
    Options {optShowVersion = True} -> printVersionNumber
    Options {optOutput = output, optProf = prof} ->
      if null nonOpts
        then showUsage
        else do
          let inFile = nonOpts !! 0
              outExec = case output of
                Just outFile -> outFile
                Nothing -> dropExtension inFile
          process prof inFile outExec

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optOutput :: Maybe String,
    optProf :: Bool
    }

showUsage :: IO ()
showUsage = do
  putStrLn "egisonc: no input files"
  
defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optOutput = Nothing,
    optProf = False
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option ['o'] ["output"]
    (ReqArg (\out opts -> opts {optOutput = Just out})
            "FILE")
    "output file to write",
  Option ['p'] ["prof"]
    (NoArg (\opts -> opts {optProf = True}))
    "use profiling system"
  ]

printVersionNumber :: IO ()
printVersionNumber = do
  putStrLn egisonVersion
  exitWith ExitSuccess

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: egisonc [options] file"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help                Display this information"
  putStrLn "  --version             Display egison version information"
  putStrLn "  --output filename     Write executable to the given filename"
  putStrLn "  --prof                Make use of GHC profiling system"
  putStrLn ""
  exitWith ExitSuccess

process :: Bool -> String -> String -> IO ()
process prof inFile outExec = do
  result <- (runIOThrows $ liftM show $ createHaskellFile inFile)
  case result of
   Just errMsg -> putStrLn errMsg
   _ -> compileHaskellFile prof outExec

appendLoadExprForCoreLibraries :: [TopExpr] -> [TopExpr]
appendLoadExprForCoreLibraries topExprs = [(Load "lib/core/base.egi"),
                                           (Load "lib/core/number.egi"),
                                           (Load "lib/core/collection.egi"),
                                           (Load "lib/core/array.egi")
                                           ] ++ topExprs
   
createHaskellFile :: String -> IOThrowsError ()
createHaskellFile inFile = do
  templatePath <- liftIO $ getDataFileName templateFile
  liftIO $ copyFile templatePath "./_tmp.hs"
  egisonProgram <- liftIO $ readFile inFile
  topExprs <- liftThrows $ readTopExprList egisonProgram
  topExprs2 <- expandLoadExprs $ appendLoadExprForCoreLibraries topExprs
  liftIO $ appendFile "./_tmp.hs" "\ntopExprs :: [TopExpr]\n"
  liftIO $ appendFile "./_tmp.hs" "topExprs = "
  liftIO $ appendFile "./_tmp.hs" $ show topExprs2
  return ()

expandLoadExprs :: [TopExpr] -> IOThrowsError [TopExpr]
expandLoadExprs [] = return []
expandLoadExprs ((LoadFile filename):topExprs) = do
  result <- liftIO $ doesFileExist filename
  if result
    then do
      loadProgram <- liftIO $ readFile filename
      loadTopExprs <- liftThrows $ readTopExprList loadProgram
      rets <- expandLoadExprs topExprs
      return $ loadTopExprs ++ rets
    else throwError $ Default $ "File does not exist: " ++ filename
expandLoadExprs ((Load libname):topExprs) = do
  filename <- liftIO (getDataFileName libname)
  result <- liftIO $ doesFileExist filename
  if result
    then do
      loadProgram <- liftIO $ readFile filename
      loadTopExprs <- liftThrows $ readTopExprList loadProgram
      rets <- expandLoadExprs topExprs
      return $ loadTopExprs ++ rets
    else throwError $ Default $ "Library does not exist: " ++ libname
expandLoadExprs (topExpr:topExprs) = do
  rets <- expandLoadExprs topExprs
  return $ topExpr:rets
  
compileHaskellFile :: Bool -> String -> IO ()
compileHaskellFile prof filename = do
  let ghc = "ghc"
  if prof
    then system $ ghc ++ " -prof -auto-all -o " ++ filename ++ " _tmp.hs"
    else system $ ghc ++ " -O2 -o " ++ filename ++ " _tmp.hs"
  removeFile "./_tmp.hs"
  removeFile "./_tmp.hi"
  removeFile "./_tmp.o"
  return ()
