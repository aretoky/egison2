module Main where
import Language.Egison.Core      -- Egison Interpreter
import Language.Egison.Parser
import Language.Egison.Types     -- Egison data types
import Language.Egison.Variables -- Egison variable operations
import System.Console.GetOpt
import Control.Monad.Error
import System.IO
import System.Environment
import System.Console.Haskeline
import System.Exit (ExitCode (..), exitWith, exitFailure)
import Text.Regex.Posix

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOpts, _) = getOpt Permute options args
  let opts = foldl (flip id) defaultOptions actions
  case opts of
    Options {optShowHelp = True} -> printHelp
    Options {optShowVersion = True} -> printVersionNumber
    Options {optPrompt = prompt, optShowBanner = bannerFlag} ->
      if null nonOpts
        then do if bannerFlag
                   then showBanner
                   else return ()
                runRepl prompt
                if bannerFlag
                  then showByebyeMessage
                  else return ()
        else do runOne $ nonOpts

data Options = Options {
    optShowVersion :: Bool,
    optShowHelp :: Bool,
    optShowBanner :: Bool,
    optPrompt :: String
    }

showUsage :: IO ()
showUsage = do
  putStrLn "egisonc: no input files"
  
defaultOptions :: Options
defaultOptions = Options {
    optShowVersion = False,
    optShowHelp = False,
    optShowBanner = True,
    optPrompt = "> "
    }

options :: [OptDescr (Options -> Options)]
options = [
  Option ['v', 'V'] ["version"]
    (NoArg (\opts -> opts {optShowVersion = True}))
    "show version number",
  Option ['h', '?'] ["help"]
    (NoArg (\opts -> opts {optShowHelp = True}))
    "show usage information",
  Option [] ["no-banner"]
    (NoArg (\opts -> opts {optShowBanner = False}))
    "show usage information",
  Option ['p'] ["prompt"]
    (ReqArg (\prompt opts -> opts {optPrompt = prompt})
            "String")
    "output file to write"
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
  putStrLn "  --no-banner           Don't show banner"
  putStrLn "  --prompt string       Set prompt of the interpreter"
  putStrLn ""
  exitWith ExitSuccess

-- REPL Section
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Execute a single egison file from the command line
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings
  -- Load standard library
  _ <- loadLibraries env
  result <- runIOThrows $ liftM show $ evalTopExpr env $ Load (args !! 0)
  case result of
    Just errMsg -> putStrLn errMsg
    _  -> do 
      -- Call into (main) if it exists...
      alreadyDefined <- liftIO $ isBound env ("main", [])
      when alreadyDefined (do
        mainResult <- runIOThrows $ liftM show $ evalMain env $ args
        case mainResult of
          Just errMsg -> putStrLn errMsg
          _  -> return ())

-- |Start the REPL (interactive interpreter)
runRepl :: String -> IO ()
runRepl prompt = do
    env <- primitiveBindings
    _ <- loadLibraries env
    runInputT defaultSettings (loop env prompt "")
    where
        loop :: Env -> String -> String -> InputT IO ()
        loop env prompt2 input0 = do
            minput <- getInputLine prompt2
            case minput of
                Nothing -> return ()
                Just "" -> if input0 == ""
                             then loop env prompt2 ""
                             else loop env (take (length prompt) (repeat ' ')) (input0 ++ "\n")
                Just input -> do
                  let newInput = input0 ++ input
                  let eTopExpr = readTopExpr newInput
                  case eTopExpr of
                    Left e -> do
                      if show e =~ "unexpected end of input" -- Is this really OK?
                        then loop env (take (length prompt) (repeat ' ')) $ newInput ++ "\n"
                        else do liftIO $ putStrLn $ show e
                                loop env prompt ""
                    Right _ -> do
                      result <- liftIO (evalString env newInput)
                      if (length result) > 0
                        then do outputStrLn result
                                loop env prompt ""
                        else loop env prompt ""

