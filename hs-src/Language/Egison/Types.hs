module Language.Egison.Types where

import Control.Monad.Error
import Data.Complex()
import Data.Array()
import Data.Dynamic()
import Data.IORef
import qualified Data.Map
-- import Data.Maybe
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

--
-- Error
--
data EgisonError = NumArgs Integer [EgisonVal]
  | TypeMismatch String EgisonVal
  | Parser ParseError
  | BadSpecialForm String [EgisonVal]
  | NotFunction String String
  | UnboundVar String String
  | DivideByZero
  | NotImplemented String
  | InternalError String
  | Default String

showError :: EgisonError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
showError (BadSpecialForm message args) = message ++ ": " ++ unwordsList args
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (DivideByZero) = "Division by zero"
showError (NotImplemented message) = "Not implemented: " ++ message
showError (InternalError message) = "An internal error occurred: " ++ message
showError (Default message) = "Error: " ++ message

instance Show EgisonError where show = showError
instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either EgisonError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Unexpected error in extractValue; "

type IOThrowsError = ErrorT EgisonError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrowsREPL :: IOThrowsError String -> IO String
runIOThrowsREPL action = runErrorT (trapError action) >>= return . extractValue

runIOThrows :: IOThrowsError String -> IO (Maybe String)
runIOThrows action = do
    runState <- runErrorT action
    case runState of
        Left err -> return $ Just (show err)
        Right _ -> return $ Nothing

--
-- Expression
--
data TopExpr = Define String EgisonExpr
  | Test EgisonExpr
  | Execute [String]
  | LoadFile String
  | Load String
        
data EgisonExpr = CharExpr Char
  | StringExpr String
  | BoolExpr Bool
  | NumberExpr Integer
  | FloatExpr Double
  | VarExpr String [EgisonExpr]
  | SymbolExpr String [EgisonExpr]
  | PatVarExpr String [EgisonExpr]
  | WildCardExpr
  | PatVarOmitExpr EgisonExpr
  | CutPatExpr EgisonExpr
  | NotPatExpr EgisonExpr
  | AndPatExpr [EgisonExpr]
  | OrPatExpr [EgisonExpr]
  | PredPatExpr String [EgisonExpr]
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [InnerExpr]
  | CollectionExpr [InnerExpr]
  | FuncExpr ArgsExpr EgisonExpr Env
  | LoopExpr {loopVar :: String,
              indexVar :: String,
              rangeExpr :: EgisonExpr,
              loopExpr :: EgisonExpr,
              tailExpr :: EgisonExpr
              }
  | ParamsExpr String EgisonExpr EgisonExpr
  | LetExpr Bindings EgisonExpr
  | LetRecExpr Bindings EgisonExpr
  | TypeExpr Bindings
  | TypeRefExpr EgisonExpr String
  | DestructorExpr DestructInfoExpr
  | MatchExpr {target :: EgisonExpr,
               typ :: EgisonExpr,
               clauses :: [MatchClause]
               }
  | MatchAllExpr {target :: EgisonExpr,
                  typ :: EgisonExpr,
                  clause :: MatchClause
                  }
  | ApplyExpr {operator :: EgisonExpr,
               operands :: [EgisonExpr]
               }

data ArgsExpr = AVarExpr VarExpr
  | ATupleExpr [ArgsExpr]
               
type MatchClause = (EgisonExpr, EgisonExpr)

data PrimitivePattern = PWirldCard
  | PPatChar Char
  | PPatInteger Integer
  | PPatDouble Double
  | PPatVar String
  | PInducivePat String [PrimitivePattern]
  | PEmptyPat
  | PConsPat PrimitivePattern PrimitivePattern
  | PSnocPat PrimitivePattern PrimitivePattern

data InnerExpr = ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
  
type Bindings = [(ArgsExpr, EgisonExpr)]
  
type DestructInfoExpr = [(String, [EgisonExpr], [(PrimitivePattern, EgisonExpr)])]

--
-- Value
--

type ObjectRef = IORef Object

data Object = Closure Env EgisonExpr
            | Value EgisonVal
            | Intermidiate IntermidiateVal
  
data EgisonVal = World [Action]
  | Char Char
  | String String
  | Bool Bool
  | Number Integer
  | Float Double
  | WildCard
  | PatVar String [Integer]
  | PredPat String [EgisonVal]
  | CutPat EgisonVal
  | NotPat EgisonVal
  | AndPat [EgisonVal]
  | OrPat [EgisonVal]
  | InductiveData String [EgisonVal]
  | Tuple [InnerVal]
  | Collection [InnerVal]
  | Type Frame
  | Func Args EgisonExpr Env
  | PrimitiveFunc ([EgisonVal] -> ThrowsError EgisonVal)
  | IOFunc ([EgisonVal] -> IOThrowsError EgisonVal)
  | Port String Handle
  | EOF

data IntermidiateVal = IPredPat String [ObjectRef]
  | ICutPat ObjectRef
  | INotPat ObjectRef
  | IAndPat [ObjectRef]
  | IOrPat [ObjectRef]
  | IInductiveData String [ObjectRef]
  | ITuple [InnerValRef]
  | ICollection [InnerValRef]
  
data Action = Read EgisonVal
  | Write EgisonVal
  | Print String
  | OpenInputFile String
  | OpenOutputFile String
  | CloseInputFile String
  | CloseOutputFile String
  | ReadFromPort String EgisonVal
  | WriteToPort String EgisonVal
  | PrintToPort String String

data Args = AVar Var
  | ATuple [Args]
  
data InnerVal = Element EgisonVal
  | SubCollection EgisonVal

data InnerValRef = IElement ObjectRef
  | ISubCollection ObjectRef

type DestructInfo = [(String, ObjectRef, [(Env, PrimitivePattern, EgisonExpr)])]

--
-- Internal Data
--

type VarExpr = (String, [EgisonExpr])

type Var = (String, [Integer])

type Frame = Data.Map.Map Var ObjectRef

type FrameRef = IORef Frame

data Env = Environment {
        parentEnv :: (Maybe Env), 
        bindings :: FrameRef
    }

nullEnv :: IO Env
nullEnv = do nullBindings <- newIORef $ Data.Map.fromList []
             return $ Environment Nothing nullBindings

makeClosure :: Env -> EgisonExpr -> IO ObjectRef
makeClosure env expr = newIORef $ Closure env expr
             
data PClosure = PClosure {pcFrame :: Frame,
                          pcBody :: ObjectRef
                          }

data MAtom = MAtom {pClosure :: PClosure,
                    maTyp :: ObjectRef,
                    maTarget :: ObjectRef
                    }

data MState = MState {msFrame :: Frame,
                      mAtoms :: [MAtom]
                      }

--
-- Type Class
--
-- |Convert a list of Egison objects into a space-separated string
unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

-- |Convert a list of Egison objects into a '_'-separated string
unwordsNums :: Show a => [a] -> String
unwordsNums [] = ""
unwordsNums (n:ns) = "_" ++ show n ++ unwordsNums ns

showVar :: (String, [Integer]) -> String
showVar (name, nums) = name ++ unwordsNums nums

showBindings :: Bindings -> String
showBindings [] = "{}"
showBindings bind = "{" ++ unwords (map showBind bind) ++ "}"
 where showBind (_,expr) = "[$" ++ "..." ++ " " ++ show expr ++ "]" 

showExpr :: EgisonExpr -> String
showExpr (CharExpr chr) = [chr]
showExpr (StringExpr contents) = contents
showExpr (BoolExpr True) = "#t"
showExpr (BoolExpr False) = "#f"
showExpr (NumberExpr contents) = show contents
showExpr (FloatExpr contents) = show contents
showExpr (VarExpr name nums) = name ++ unwordsNums nums
showExpr (SymbolExpr name nums) = "#" ++ name ++ unwordsNums nums
showExpr (PatVarExpr name nums) = "$" ++ name ++ unwordsNums nums
showExpr WildCardExpr = "_"
showExpr (PatVarOmitExpr pvar) = "(omit " ++ show pvar ++ ")"
showExpr (CutPatExpr _) = "#<cut-pat>"
showExpr (NotPatExpr _) = "#<not-pat>"
showExpr (AndPatExpr _) = "#<and-pat>"
showExpr (OrPatExpr _) = "#<or-pat>"
showExpr (PredPatExpr _ _) = "#<pred-pat>"
showExpr (InductiveDataExpr cons args) = "<" ++ cons ++ "...>"
showExpr (TupleExpr innerExprs) = "[...]"
showExpr (CollectionExpr innerExprs) = "{...}"
showExpr (FuncExpr _ _ _) =
  "(lambda [" ++ "..." ++ "] ...)"
showExpr (LoopExpr lVar iVar rExpr lExpr tExpr) =
  "(loop $" ++ lVar ++ " $" ++ iVar ++ " " ++ show rExpr ++ " " ++  show lExpr ++ " " ++ show tExpr ++ ")"
showExpr (ParamsExpr pVar pExpr body) =
  "(loop $" ++ pVar ++ " " ++ show pExpr ++ " " ++ show body ++ ")"
showExpr (LetExpr bindings body) =
  "(let " ++ showBindings bindings ++ " " ++ show body ++ ")"
showExpr (LetRecExpr bindings body) =
  "(letrec " ++ showBindings bindings ++ " " ++ show body ++ ")"
showExpr (TypeExpr bindings) =
  "(type " ++ showBindings bindings ++ ")"
showExpr (TypeRefExpr typExpr name) =
  "(type-ref " ++ show typExpr ++ " " ++ name ++ ")"
showExpr (DestructorExpr _) = "(destructor ...)"
showExpr (MatchExpr tgtExpr typExpr cls) =
  "(match " ++ show tgtExpr ++ " " ++ show typExpr ++ " ...)"
showExpr (MatchAllExpr tgtExpr typExpr clss) =
  "(match-all " ++ show tgtExpr ++ " " ++ show typExpr ++ " ...)"
showExpr (ApplyExpr opExpr argExprs) =
  "(" ++ show opExpr ++ " " ++ unwordsList argExprs ++ ")"
  
-- |Allow conversion of egisonexpr instances to strings
instance Show EgisonExpr where show = showExpr
                      
eqv :: [EgisonVal] -> ThrowsError EgisonVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(Char arg1), (Char arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqVal :: EgisonVal -> EgisonVal -> Bool
eqVal a b = do
  let result = eqv [a, b]
  case result of
    Left _ -> False
    Right (Bool val) -> val
    _ -> False -- Is this OK?

instance Eq EgisonVal where
  x == y = eqVal x y

showVal :: EgisonVal -> String
showVal (Char chr) = [chr]
showVal (World _) = "#<world>"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal WildCard = "_"
showVal (PatVar name nums) =  "$" ++ name ++ unwordsNums nums
showVal (CutPat _) = "#<cut-pat>"
showVal (NotPat _) = "#<not-pat>"
showVal (AndPat _) = "#<and-pat>"
showVal (OrPat _) = "#<or-pat>"
showVal (PredPat _ _) = "#<pred-pat>"
showVal (InductiveData cons args) = "<" ++ cons ++ " " ++ unwordsList args ++ ">"
showVal (Tuple innerVals) = "{" ++ "..." ++ "}"
showVal (Collection innerVals) = "[" ++ "..." ++ "]"
showVal (Type _) = "#<type>"
showVal (Func _ _ _) =
  "(lambda [" ++ "..." ++ "] ...)"
showVal (PrimitiveFunc _) = "#<primitive>"
showVal (IOFunc _) = "#<IO primitive>"
showVal (Port _ _) = "#<IO port>"
showVal EOF = "#!EOF"

-- |Allow conversion of egisonval instances to strings
instance Show EgisonVal where show = showVal

showIVal :: IntermidiateVal -> String
showIVal (IPredPat _ _) = "#<pred-pat>"
showIVal (ICutPat _) = "#<cut-pat>"
showIVal (INotPat _) = "#<not-pat>"
showIVal (IAndPat _) = "#<and-pat>"
showIVal (IOrPat _) = "#<or-pat>"
showIVal (IInductiveData cons args) = "<" ++ cons ++ " " ++ "..." ++ ">"
showIVal (ITuple fInnerValRefs) = "[" ++ "..." ++ "]"
showIVal (ICollection fInnerValRefs) = "{" ++ "..." ++ "}"

-- |Allow conversion of egisonfixedval instances to strings
instance Show IntermidiateVal where show = showIVal

showObj :: Object -> String
showObj (Closure _ expr) = "(Closure env " ++  show expr ++ ")"
showObj (Value val) = "(Value " ++ show val ++ ")"
showObj (Intermidiate val) = "(Intermidiate " ++ show val ++ ")"

-- |Allow conversion of egisonval instances to strings
instance Show Object where show = showObj

-- - utility
stringToCharCollection :: String -> IO EgisonVal
stringToCharCollection = undefined