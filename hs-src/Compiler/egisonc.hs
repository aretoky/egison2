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

templateFileForTest :: String
templateFileForTest = "etc/template-for-test.hs"

main :: IO ()
main = do 
  args <- getArgs
  let (actions, nonOpts, _) = getOpt Permute options args
  let opts = foldl (flip id) defaultOptions actions
  case opts of
    Options {optShowHelp = True} -> printHelp
    Options {optShowVersion = True} -> printVersionNumber
    Options {optOutput = output} ->
      if null nonOpts
        then showUsage
        else do
          let inFile = nonOpts !! 0
              outExec = case output of
                Just outFile -> outFile
                Nothing -> dropExtension inFile
          process opts inFile outExec

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optOutput :: Maybe String,
    optProf :: Bool,
    optTest :: Bool
    }

showUsage :: IO ()
showUsage = do
  putStrLn "egisonc: no input files"
  
defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optOutput = Nothing,
    optProf = False,
    optTest = False
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
    "use profiling system",
  Option ['t'] ["test"]
    (NoArg (\opts -> opts {optTest = True}))
    "execute test expressions"
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
  putStrLn "  --test                Execute test expressions"
  putStrLn ""
  exitWith ExitSuccess

process :: Options -> String -> String -> IO ()
process opts inFile outExec = do
  result <- (runIOThrows $ liftM show $ createHaskellFile opts inFile)
  case result of
   Just errMsg -> putStrLn errMsg
   _ -> compileHaskellFile opts outExec

appendLoadExprForCoreLibraries :: [TopExpr] -> [TopExpr]
appendLoadExprForCoreLibraries topExprs = [(Load "lib/core/base.egi"),
                                           (Load "lib/core/number.egi"),
                                           (Load "lib/core/collection.egi"),
                                           (Load "lib/core/array.egi")
                                           ] ++ topExprs
   
createHaskellFile :: Options -> String -> IOThrowsError ()
createHaskellFile opts inFile =
  case opts of
    Options {optTest = True} -> do
      templatePath <- liftIO $ getDataFileName templateFileForTest
      liftIO $ copyFile templatePath "./_tmp.hs"
      egisonProgram <- liftIO $ readFile inFile
      topExprs <- liftThrows $ readTopExprList egisonProgram
      topExprs2 <- expandLoadExprs opts $ appendLoadExprForCoreLibraries topExprs
      liftIO $ appendFile "./_tmp.hs" "\ntopExprs :: [TopExpr]\n"
      liftIO $ appendFile "./_tmp.hs" "topExprs = "
      liftIO $ appendFile "./_tmp.hs" $ show topExprs2
      return ()
    _ -> do
      templatePath <- liftIO $ getDataFileName templateFile
      liftIO $ copyFile templatePath "./_tmp.hs"
      egisonProgram <- liftIO $ readFile inFile
      topExprs <- liftThrows $ readTopExprList egisonProgram
      topExprs2 <- expandLoadExprs opts $ appendLoadExprForCoreLibraries topExprs
      liftIO $ appendFile "./_tmp.hs" "\ntopExprs :: [TopExpr]\n"
      liftIO $ appendFile "./_tmp.hs" "topExprs = "
      liftIO $ appendFile "./_tmp.hs" $ show topExprs2
      return ()

expandLoadExprs :: Options -> [TopExpr] -> IOThrowsError [TopExpr]
expandLoadExprs _ [] = return []
expandLoadExprs opts ((LoadFile filename):topExprs) = do
  result <- liftIO $ doesFileExist filename
  if result
    then do
      loadProgram <- liftIO $ readFile filename
      loadTopExprs <- liftThrows $ readTopExprList loadProgram
      rets <- expandLoadExprs opts topExprs
      return $ loadTopExprs ++ rets
    else throwError $ Default $ "File does not exist: " ++ filename
expandLoadExprs opts ((Load libname):topExprs) = do
  filename <- liftIO (getDataFileName libname)
  result <- liftIO $ doesFileExist filename
  if result
    then do
      loadProgram <- liftIO $ readFile filename
      loadTopExprs <- liftThrows $ readTopExprList loadProgram
      rets <- expandLoadExprs opts topExprs
      return $ loadTopExprs ++ rets
    else throwError $ Default $ "Library does not exist: " ++ libname
expandLoadExprs opts (topExpr@(Test _):topExprs) =
  case opts of
    Options {optTest = True} -> do
      rets <- expandLoadExprs opts topExprs
      return $ topExpr:rets
    _ -> expandLoadExprs opts topExprs
expandLoadExprs opts (topExpr:topExprs) = do
  rets <- expandLoadExprs opts topExprs
  return $ topExpr:rets
  
compileHaskellFile :: Options -> String -> IO ()
compileHaskellFile opts filename = do
  let ghc = "ghc"
  case opts of
    Options {optProf = True} ->
      system $ ghc ++ " -prof -auto-all -o " ++ filename ++ " _tmp.hs"
    _ ->
      system $ ghc ++ " -O2 -o " ++ filename ++ " _tmp.hs"
  removeFile "./_tmp.hs"
  removeFile "./_tmp.hi"
  removeFile "./_tmp.o"
  return ()
