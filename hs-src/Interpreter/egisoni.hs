module Main where
import Language.Egison.Core      -- Egison Interpreter
import Language.Egison.Parser
import Language.Egison.Types     -- Egison data types
import Language.Egison.Variables -- Egison variable operations
import Control.Monad.Error
import System.IO
import System.Environment
import System.Console.Haskeline
import Text.Regex.Posix

main :: IO ()
main = do args <- getArgs
          if null args then do showBanner
                               runRepl
                       else runOne $ args

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
runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    _ <- loadLibraries env
    runInputT defaultSettings (loop env "> " "")
    where
        loop :: Env -> String -> String -> InputT IO ()
        loop env prompt input0 = do
            minput <- getInputLine prompt
            case minput of
                Nothing -> liftIO showByebyeMessage
                Just "" -> if input0 == ""
                             then loop env "> " ""
                             else loop env "  " (input0 ++ "\n")
                Just input -> do
                  let newInput = input0 ++ input
                  let eTopExpr = readTopExpr newInput
                  case eTopExpr of
                    Left e -> do
                      if show e =~ "unexpected end of input" -- Is this really OK?
                        then loop env "  " $ newInput ++ "\n"
                        else do liftIO $ putStrLn $ show e
                                loop env "> " ""
                    Right _ -> do
                      result <- liftIO (evalString env newInput)
                      if (length result) > 0
                        then do outputStrLn result
                                loop env "> " ""
                        else loop env "> " ""

