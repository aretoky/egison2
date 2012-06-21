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

-- |A utility function to display the husk console banner
showBanner :: IO ()
showBanner = do
  putStrLn " Welecome to Egison Interpreter!                                         "
  putStrLn " (c) 2011-2012 Satoshi Egi                                               "
  putStrLn $ " Version " ++ version ++ " "
  putStrLn " http://hagi.is.s.u-tokyo.ac.jp/~egi/egison/                             "
  putStrLn "                                                                         "

libraries :: [String]
libraries = ["lib/base.egi", "lib/number.egi", "lib/collection.egi"]
  
-- |A utility function to escape backslashes in the given string
escapeBackslashes :: String -> String
escapeBackslashes s = foldr step [] s
  where step x xs  | x == '\\'  = '\\' : '\\' : xs
                   | otherwise =  x : xs 


evalString :: Env -> String -> IO String
evalString env expr = runIOThrowsREPL $ liftM show $ (liftThrows $ readExpr expr) >>= evalTopExpr env
--evalString _ expr = return expr

evalMain :: Env -> [EgisonVal] -> IOThrowsError EgisonVal
evalMain env args = do
  argv <- undefined
  evalTopExpr env $ Test $ ApplyExpr (VarExpr "main" []) argv

-- |Evaluate egison top expression that has already been loaded into haskell
evalTopExpr :: Env -> TopExpr -> IOThrowsError EgisonVal
evalTopExpr env expr = undefined

-- |Evaluate egison expression that has already been loaded into haskell
eval :: Env -> EgisonExpr -> IOThrowsError EgisonVal
eval env expr = undefined

primitiveBindings :: IO Env
--primitiveBindings = nullEnv >>= (flip extendEnv $ map (domakeFunc IOFunc) ioPrimitives
--                                               ++ map (domakeFunc PrimitiveFunc) primitives)
--  where domakeFunc constructor (name, func) = ((name, []), Value $ constructor func)
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

