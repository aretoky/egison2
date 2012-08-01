module Main where
import Language.Egison.Core
import Language.Egison.Types
import Control.Monad.Error

evalTopExprs :: Env -> [TopExpr] -> IO ()
evalTopExprs _ [] = return ()
evalTopExprs env (topExpr:rest) = do
  str <- runIOThrowsREPL $ evalTopExpr env topExpr
  case topExpr of
    Test _ -> do putStrLn str
                 evalTopExprs env rest
    _ -> evalTopExprs env rest

main :: IO ()
main = do
  env <- primitiveBindings
  evalTopExprs env topExprs

