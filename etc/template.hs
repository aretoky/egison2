module Main where
import Language.Egison.Core      -- Egison Interpreter
import Language.Egison.Types     -- Egison data types
--import Language.Egison.Variables -- Egison variable operations
--import Language.Egison.Parser -- Egison variable operations
--import Control.Monad (when)
import Control.Monad.Error
--import System.IO
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  env <- primitiveBindings
  ret1 <- runIOThrows $ liftM concat $ mapM (evalTopExpr env) topExprs
  case ret1 of
   Just errMsg -> putStrLn errMsg
   _ -> do
     ret2 <- runIOThrows $ evalTopExpr env $ Execute args
     case ret2 of
       Just errMsg -> putStrLn errMsg
       _ -> return ()
