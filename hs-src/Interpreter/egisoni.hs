module Main where
import Paths_egison
import Language.Egison.Core      -- Egison Interpreter
import Language.Egison.Types     -- Egison data types
import Language.Egison.Variables -- Egison variable operations
--import Control.Monad (when)
import Control.Monad.Error
import System.IO
import System.Environment
import System.Console.Haskeline

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

-- |Load standard libraries into the given environment
loadLibraries :: Env -> IO ()
loadLibraries env = do
  baselib <- getDataFileName "lib/base.egi"
  -- Load standard library
  _ <- evalString env $ "(load \"" ++ (escapeBackslashes baselib) ++ "\")"
  return ()

-- |Start the REPL (interactive interpreter)
runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    _ <- loadLibraries env
    runInputT defaultSettings (loop env)
    where
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "> "
            case minput of
                Nothing -> return ()
                Just "" -> loop env -- FUTURE: integrate with strip to ignore inputs of just whitespace
                Just input -> do result <- liftIO (evalString env input)
                                 if (length result) > 0
                                    then do outputStrLn result
                                            loop env
                                    else loop env
-- End REPL Section

-- Begin Util section, of generic functions

{- Remove leading/trailing white space from a string; based on corresponding Python function
   Code taken from: http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/ -}
strip :: String -> String
strip s = dropWhile ws $ reverse $ dropWhile ws $ reverse s
    where ws = (`elem` [' ', '\n', '\t', '\r'])

-- End Util
