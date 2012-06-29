module Main where
import Language.Egison.Core      -- Egison Interpreter
import Language.Egison.Types     -- Egison data types
import Language.Egison.Variables -- Egison variable operations
import Language.Egison.Parser -- Egison variable operations
--import Control.Monad (when)
import Control.Monad.Error
import System.IO
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  env <- primitiveBindings
  _ <- loadLibraries env
  _ <- runIOThrows (do topExprs <- liftThrows (readTopExprList program)
                       liftM concat $ mapM (evalTopExpr env) topExprs)
  _ <- runIOThrows $ evalTopExpr env $ Execute args
  return ()

