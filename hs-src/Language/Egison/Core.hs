module Language.Egison.Core
    (
    -- * Egison code evaluation
      eval
    , evalString
    , evalMain
    , evalTopExpr
    -- * Core data
    , primitiveBindings
    , version
    -- * Utility functions
    , escapeBackslashes
    , showBanner
    , showByebyeMessage
    , libraries
--    , substr
    ) where
import Language.Egison.Numerical
import Language.Egison.Parser
import Language.Egison.Primitives
import Language.Egison.Types
import Language.Egison.Variables
import Control.Monad.Error
import Data.Array
import qualified Data.Map
import qualified System.Exit
import System.IO
import Data.IORef

-- |egison version number
version :: String
version = "2.0.0"

-- |A utility function to display the egison console banner
showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ version ++ " (c) 2011-2012 Satoshi Egi"
  putStrLn $ "http://hagi.is.s.u-tokyo.ac.jp/~egi/egison/"
  putStrLn $ "Welcome to Egison Interpreter!"

-- |A utility function to display the egison console byebye message
showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison."
  putStrLn $ "Byebye. See you again! (^^)/"

libraries :: [String]
libraries = ["lib/base.egi", "lib/number.egi", "lib/collection.egi"]
  
-- |A utility function to escape backslashes in the given string
escapeBackslashes :: String -> String
escapeBackslashes s = foldr step [] s
  where step x xs  | x == '\\'  = '\\' : '\\' : xs
                   | otherwise =  x : xs 


evalString :: Env -> String -> IO String
evalString env expr = runIOThrowsREPL $ (liftThrows $ readExpr expr) >>= evalTopExpr env
--evalString _ expr = return expr

evalMain :: Env -> [String] -> IOThrowsError EgisonVal
evalMain env args = do
  let argv = map StringExpr args
  eval env $ ApplyExpr (VarExpr "main" []) argv

-- |Evaluate egison top expression that has already been loaded into haskell
evalTopExpr :: Env -> TopExpr -> IOThrowsError String
evalTopExpr env (Test expr) = liftM show $ eval env expr
evalTopExpr env (Define name expr) = do clr <- liftIO $ makeClosure env expr
                                        defineVar env (name, []) clr
                                        return name
evalTopExpr env (Execute args) = do evalMain env args
                                    return ""
evalTopExpr _ (LoadFile filename) = undefined
evalTopExpr _ (Load libname) = undefined

-- |Evaluate egison expression that has already been loaded into haskell
eval :: Env -> EgisonExpr -> IOThrowsError EgisonVal
eval env expr = do
  obj <- cEval1 (Closure env expr)
--  liftIO $ putStrLn $ show obj -- Debug
  case obj of
    Value val -> return val
    Intermidiate iVal -> iEval iVal
    _ -> throwError $ Default "eval: cannot reach here!"

iEval :: IntermidiateVal -> IOThrowsError EgisonVal
iEval (IInductiveData cons argRefs) = do
  args <- mapM cRefEval argRefs
  return $ InductiveData cons args
iEval (ICollection innerValRefs) = do
  innerVals <- mapM innerValRefEval innerValRefs
  return $ Collection innerVals
iEval _ = undefined

innerValRefEval :: InnerValRef -> IOThrowsError InnerVal
innerValRefEval (IElement objRef) = liftM Element $ cRefEval objRef
innerValRefEval (ISubCollection objRef) = liftM SubCollection $ cRefEval objRef

cRefEval :: ObjectRef -> IOThrowsError EgisonVal
cRefEval objRef = do
  obj <- liftIO $ readIORef objRef
  val <- cEval obj
  liftIO $ writeIORef objRef $ Value val
  return val

cRefEval1 :: ObjectRef -> IOThrowsError Object
cRefEval1 objRef = do
  obj <- liftIO $ readIORef objRef
  obj2 <- cEval1 obj
  liftIO $ writeIORef objRef obj2
  return obj2

cEval :: Object -> IOThrowsError EgisonVal
cEval (Closure env expr) = eval env expr
cEval (Value val) = return val
cEval (Intermidiate iVal) = iEval iVal

cEval1 :: Object -> IOThrowsError Object
cEval1 (Closure _ (NumberExpr contents)) = return $ Value (Number contents)
cEval1 (Closure _ (FloatExpr contents)) = return $ Value (Float contents)
cEval1 (Closure env (VarExpr name numExprs)) = do
  numVals <- mapM (eval env) numExprs
  nums <- mapM (\nVal -> case nVal of
                           Number num -> return num
                           _ -> throwError  $ Default "cEval1: cannot reach here!")
               numVals
  objRef <- getVar env (name, nums)
  obj <- cRefEval1 objRef
  return obj
cEval1 (Closure env (InductiveDataExpr cons argExprs)) = do
  args <- liftIO $ mapM (makeClosure env) argExprs
  return $ Intermidiate $ IInductiveData cons args
cEval1 (Closure env (CollectionExpr innerExprs)) = do
  innerRefs <- liftIO $ mapM (makeInnerValRef env) innerExprs
  return $ Intermidiate $ ICollection innerRefs
cEval1 (Closure env (ApplyExpr opExpr argExprs)) = do
  op <- cEval1 (Closure env opExpr)
  case op of
    Value (IOFunc fn) -> undefined
    Value (PrimitiveFunc fn) -> do args <- mapM (eval env) argExprs
                                   val <- liftThrows $ fn args
                                   return $ Value val
    Value (Func args body cEnv) -> undefined
    _ -> throwError $ Default "not function"
cEval1 val = return val

primitiveBindings :: IO Env
primitiveBindings = do
  initEnv <- nullEnv
  iOFuncs <- mapM (domakeFunc IOFunc) ioPrimitives
  primitiveFuncs <- mapM (domakeFunc PrimitiveFunc) primitives
  extendEnv initEnv (iOFuncs ++ primitiveFuncs)
 where domakeFunc constructor (name, func) = do objRef <- newIORef $ Value $ constructor func
                                                return ((name, []), objRef)

{- I/O primitives
Primitive functions that execute within the IO monad -}
ioPrimitives :: [(String, [EgisonVal] -> IOThrowsError EgisonVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort)]

{- "Pure" primitive functions -}
primitives :: [(String, [EgisonVal] -> ThrowsError EgisonVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
--              ("/", numericBinop (/)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),

              ("+f", floatBinop (+)),
              ("-f", floatBinop (-)),
              ("*f", floatBinop (*)),
              ("/f", floatBinop (/)),

              ("round", floatNumSglop round),
              ("floor", floatNumSglop floor),
              ("ceiling", floatNumSglop ceiling),
              ("truncate", floatNumSglop truncate),

              ("exp", numExp),
              ("log", numLog),

              ("sin", floatSglop sin),
              ("cos", floatSglop cos),
              ("tan", floatSglop tan),
              ("asin", floatSglop asin),
              ("acos", floatSglop acos),
              ("atan", floatSglop atan),
              ("sinh", floatSglop sinh),
              ("cosh", floatSglop cosh),
              ("tanh", floatSglop tanh),
              ("asinh", floatSglop asinh),
              ("acosh", floatSglop acosh),
              ("atanh", floatSglop atanh),

              ("sqrt", numSqrt),
              ("expt", numExpt),

              ("=", numBoolBinop (==)),
              ("gt", numBoolBinop (<)),
              ("gte", numBoolBinop (<=)),
              ("lt", numBoolBinop (>)),
              ("lte", numBoolBinop (>=)),

              ("=f", floatBoolBinop (==)),
              ("gt-f", floatBoolBinop (<)),
              ("gte-f", floatBoolBinop (<=)),
              ("lt-f", floatBoolBinop (>)),
              ("lte-f", floatBoolBinop (>=)),

              ("&&", boolBinop (&&)),
              ("||", boolBinop (||))]

