module Main where
import Language.Egison.Core
import Language.Egison.Types
import Control.Monad.Error
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
