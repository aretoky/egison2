module Main where
import Language.Egison.Core
import Language.Egison.Types
import Control.Monad.Error

evalTopExprs :: Env -> [TopExpr] -> IO ()
evalTopExprs _ [] = return ()
evalTopExprs env (topExpr:rest) = do
  str <- runIOThrowsREPL $ evalTopExpr env topExpr
  putStrLn str
  evalTopExprs env rest

main :: IO ()
main = do
  env <- primitiveBindings
  evalTopExprs env topExprs

