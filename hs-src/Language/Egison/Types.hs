module Language.Egison.Types where

import Control.Monad.Error
import Data.Complex()
import Data.Array
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
  | TypeMismatch String [EgisonVal]
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
                                  ++ ", found " ++ unwordsList found
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
 deriving (Show)
        
data EgisonExpr = CharExpr Char
  | StringExpr String
  | BoolExpr Bool
  | NumberExpr Integer
  | FloatExpr Double
  | VarExpr String [EgisonExpr]
  | MacroVarExpr String [EgisonExpr]
  | PatVarOmitExpr EgisonExpr
  | VarOmitExpr EgisonExpr
  | PatVarExpr String [EgisonExpr]
  | WildCardExpr
  | ValuePatExpr EgisonExpr
  | CutPatExpr EgisonExpr
  | NotPatExpr EgisonExpr
  | AndPatExpr [EgisonExpr]
  | OrPatExpr [EgisonExpr]
  | PredPatExpr String [EgisonExpr]
  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [InnerExpr]
  | CollectionExpr [InnerExpr]
  | ArrayExpr [EgisonExpr]
  | FuncExpr Args EgisonExpr
  | MacroExpr [String] EgisonExpr
  | LoopExpr String String EgisonExpr EgisonExpr EgisonExpr
  | ParamsExpr String EgisonExpr EgisonExpr
  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetExpr Bindings EgisonExpr
  | LetRecExpr RecursiveBindings EgisonExpr
  | DoExpr Bindings EgisonExpr
  | TypeExpr RecursiveBindings
  | TypeRefExpr EgisonExpr String
  | DestructorExpr DestructInfoExpr
  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause
  | ApplyExpr EgisonExpr EgisonExpr
 deriving (Show)

type ArgsExpr = Args
               
type MatchClause = (EgisonExpr, EgisonExpr)

data PrimitivePatPattern = PPWildCard
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
 deriving (Show)

data PrimitivePattern = PWildCard
  | PPatVar String
  | PInductivePat String [PrimitivePattern]
  | PEmptyPat
  | PConsPat PrimitivePattern PrimitivePattern
  | PSnocPat PrimitivePattern PrimitivePattern
  | PPatBool Bool
  | PPatChar Char
  | PPatNumber Integer
  | PPatFloat Double
 deriving (Show)

data InnerExpr = ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show)

type Bindings = [(Args, EgisonExpr)]

type RecursiveBindings = [(String, EgisonExpr)]
  
type DestructInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitivePattern, EgisonExpr)])]

--
-- Value
--
type ObjectRef = IORef Object

data Object = Closure Env EgisonExpr
            | Value EgisonVal
            | Intermidiate IntermidiateVal
            | Loop String String ObjectRef EgisonExpr EgisonExpr
  
data EgisonVal = World [Action]
  | Char Char
  | String String
  | Bool Bool
  | Number Integer
  | Float Double
  | WildCard
  | PatVar String [Integer]
  | ValuePat ObjectRef
  | PredPat String [ObjectRef]
  | CutPat ObjectRef
  | NotPat ObjectRef
  | AndPat [ObjectRef]
  | OrPat [ObjectRef]
  | InductiveData String [EgisonVal]
  | Tuple [InnerVal]
  | Collection [InnerVal]
  | Array (Array Int EgisonVal)
  | Type Frame
  | Destructor DestructInfo
  | Func Args EgisonExpr Env
  | Macro [String] EgisonExpr
  | PrimitiveFunc ([EgisonVal] -> ThrowsError EgisonVal)
  | IOFunc ([EgisonVal] -> IOThrowsError EgisonVal)
  | Port String Handle
  | EOF

data IntermidiateVal = IInductiveData String [ObjectRef]
  | ITuple [InnerValRef]
  | ICollection [InnerValRef]
  
data Action = OpenInputPort String
  | OpenOutputPort String
  | ClosePort String
  | FlushPort String
  | ReadFromPort String String
  | WriteToPort String String

data Args = AVar String
  | ATuple [Args]
 deriving (Show)
  
data InnerVal = Element EgisonVal
  | SubCollection EgisonVal

innerValsToList :: [InnerVal] -> [EgisonVal]
innerValsToList [] = []
innerValsToList ((Element val):rest) = val:(innerValsToList rest)
innerValsToList ((SubCollection (Collection iVals)):rest) = (innerValsToList iVals) ++ (innerValsToList rest)

tupleToList :: EgisonVal -> [EgisonVal]
tupleToList (Tuple innerVals) = innerValsToList innerVals
tupleToList val = [val]
  
collectionToList :: EgisonVal -> [EgisonVal]
collectionToList (Collection innerVals) = innerValsToList innerVals

valsToObjRefList :: [EgisonVal] -> IO [ObjectRef]
valsToObjRefList vals = mapM newIORef (map Value vals)

makeTupleFromValList :: [EgisonVal] -> EgisonVal
makeTupleFromValList vals = Tuple $ map Element vals

makeCollectionFromValList :: [EgisonVal] -> EgisonVal
makeCollectionFromValList vals = Collection $ map Element vals

data InnerValRef = IElement ObjectRef
  | ISubCollection ObjectRef

type DestructInfo = [(PrimitivePatPattern, ObjectRef, [(Env, PrimitivePattern, EgisonExpr)])]

--
-- Internal Data
--
type VarExpr = (String, [EgisonExpr])

type Var = (String, [Integer])

type FrameList = [(Var, ObjectRef)]

type Frame = Data.Map.Map Var ObjectRef

type FrameRef = IORef Frame

data Env = Environment {
        parentEnv :: (Maybe Env), 
        topFrameRef :: FrameRef
    }

nullEnv :: IO Env
nullEnv = do nullBindings <- newIORef $ Data.Map.fromList []
             return $ Environment Nothing nullBindings

makeClosure :: Env -> EgisonExpr -> IO ObjectRef
makeClosure env expr = newIORef $ Closure env expr

makeInnerValRef :: Env -> InnerExpr -> IO InnerValRef
makeInnerValRef env (ElementExpr expr) = do
  objRef <- makeClosure env expr
  return $ IElement objRef
makeInnerValRef env (SubCollectionExpr expr) = do
  objRef <- makeClosure env expr
  return $ ISubCollection objRef

data MatchFlag = MAll | MOne
  
data PClosure = PClosure {pcFrame :: FrameList,
                          pcBody :: ObjectRef
                          }

data MAtom = MAtom {pClosure :: PClosure,
                    maTyp :: ObjectRef,
                    maTarget :: ObjectRef
                    }

data MState = MState {msFrame :: FrameList,
                      mAtoms :: [MAtom]
                      }

--
-- Type Class
--
-- |Convert a list of Egison objects into a space-separated string
unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

-- |Convert a list of Egison expressions into a space-separated string
unwordsExpr :: [EgisonExpr] -> String
unwordsExpr = unwords . map showExpr

-- |Convert a list of Egison expressions into a '_'-separated string
unwordsNumExprs :: [EgisonExpr] -> String
unwordsNumExprs [] = ""
unwordsNumExprs (n:ns) = "_" ++ showExpr n ++ unwordsNumExprs ns

-- |Convert a list of Egison objects into a '_'-separated string
unwordsNums :: Show a => [a] -> String
unwordsNums [] = ""
unwordsNums (n:ns) = "_" ++ show n ++ unwordsNums ns

showVar :: (String, [Integer]) -> String
showVar (name, nums) = name ++ unwordsNums nums

showBindings :: Bindings -> String
showBindings [] = "{}"
showBindings bindings = "{" ++ unwords (map showBinding bindings) ++ "}"
 where showBinding (_,expr) = "[$" ++ "..." ++ " " ++ showExpr expr ++ "]" 

showRecursiveBindings :: RecursiveBindings -> String
showRecursiveBindings [] = "{}"
showRecursiveBindings bind = "{" ++ unwords (map showBinding bind) ++ "}"
 where showBinding (_,expr) = "[$" ++ "..." ++ " " ++ showExpr expr ++ "]" 

showExpr :: EgisonExpr -> String
showExpr (CharExpr chr) = [chr]
showExpr (StringExpr contents) = contents
showExpr (BoolExpr True) = "#t-expr"
showExpr (BoolExpr False) = "#f-expr"
showExpr (NumberExpr contents) = show contents
showExpr (FloatExpr contents) = show contents
showExpr (VarExpr name nums) = name ++ unwordsNumExprs nums
showExpr (MacroVarExpr name nums) = "%" ++ name ++ unwordsNumExprs nums
showExpr (PatVarExpr name nums) = "$" ++ name ++ unwordsNumExprs nums
showExpr WildCardExpr = "_"
showExpr (ValuePatExpr expr) = "," ++ showExpr expr
showExpr (PatVarOmitExpr pvar) = "$~" ++ showExpr pvar
showExpr (VarOmitExpr pvar) = "~" ++ showExpr pvar
showExpr (CutPatExpr _) = "#<cut-pat>"
showExpr (NotPatExpr _) = "#<not-pat>"
showExpr (AndPatExpr _) = "#<and-pat>"
showExpr (OrPatExpr _) = "#<or-pat>"
showExpr (PredPatExpr _ _) = "#<pred-pat>"
showExpr (InductiveDataExpr cons []) = "<" ++ cons ++ ">"
showExpr (InductiveDataExpr cons exprs) = "<" ++ cons ++ " " ++ unwordsExpr exprs ++ ">"
showExpr (TupleExpr _) = "[...]"
showExpr (CollectionExpr _) = "{...}"
showExpr (FuncExpr _ _) =
  "(lambda [" ++ "..." ++ "] ...)"
showExpr (MacroExpr _ _) =
  "(macro [" ++ "..." ++ "] ...)"
showExpr (LoopExpr lVar iVar rExpr lExpr tExpr) =
  "(loop $" ++ lVar ++ " $" ++ iVar ++ " " ++ showExpr rExpr ++ " " ++  showExpr lExpr ++ " " ++ showExpr tExpr ++ ")"
showExpr (ParamsExpr pVar pExpr body) =
  "(loop $" ++ pVar ++ " " ++ showExpr pExpr ++ " " ++ showExpr body ++ ")"
showExpr (IfExpr condExpr expr1 expr2) =
  "(if " ++ showExpr condExpr ++ " " ++ showExpr expr1 ++ " " ++ showExpr expr2 ++ ")"
showExpr (LetExpr bindings body) =
  "(let " ++ showBindings bindings ++ " " ++ showExpr body ++ ")"
showExpr (LetRecExpr bindings body) =
  "(letrec " ++ showRecursiveBindings bindings ++ " " ++ showExpr body ++ ")"
showExpr (DoExpr bindings body) =
  "(do " ++ showBindings bindings ++ " " ++ showExpr body ++ ")"
showExpr (TypeExpr bindings) =
  "(type " ++ showRecursiveBindings bindings ++ ")"
showExpr (TypeRefExpr typExpr name) =
  "(type-ref " ++ showExpr typExpr ++ " " ++ name ++ ")"
showExpr (DestructorExpr _) = "(destructor ...)"
showExpr (MatchExpr tgtExpr typExpr _) =
  "(match " ++ showExpr tgtExpr ++ " " ++ showExpr typExpr ++ " ...)"
showExpr (MatchAllExpr tgtExpr typExpr _) =
  "(match-all " ++ showExpr tgtExpr ++ " " ++ showExpr typExpr ++ " ...)"
showExpr (ApplyExpr opExpr argExpr) =
  "(" ++ showExpr opExpr ++ " " ++ showExpr argExpr ++ ")"
  
---- |Allow conversion of egisonexpr instances to strings
--instance Show EgisonExpr where show = showExpr
                      
eqv :: [EgisonVal] -> ThrowsError EgisonVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(Char arg1), (Char arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(InductiveData cons1 args1), (InductiveData cons2 args2)] =
  if (cons1 == cons2)
    then return $ Bool $ eqValList args1 args2
    else return $ Bool False
eqv [(Tuple innerVals1), (Tuple innerVals2)] =
  return $ Bool $ eqValList (innerValsToList innerVals1) (innerValsToList innerVals2)
eqv [(Collection innerVals1), (Collection innerVals2)] =
  return $ Bool $ eqValList (innerValsToList innerVals1) (innerValsToList innerVals2)
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqValList :: [EgisonVal] -> [EgisonVal] -> Bool
eqValList [] [] = True
eqValList (val1:vals1) (val2:vals2) =
  if eqVal val1 val2
    then eqValList vals1 vals2
    else False
eqValList _ _ = False

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
showVal (World _) = "#<world>"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char chr) = "'" ++ [chr] ++ "'"
showVal (String str) = "\"" ++ str ++ "\""
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal WildCard = "_"
showVal (PatVar name nums) =  "$" ++ name ++ unwordsNums nums
showVal (ValuePat _) = "#<value-pat>"
showVal (CutPat _) = "#<cut-pat>"
showVal (NotPat _) = "#<not-pat>"
showVal (AndPat _) = "#<and-pat>"
showVal (OrPat _) = "#<or-pat>"
showVal (PredPat _ _) = "#<pred-pat>"
showVal (InductiveData cons []) = "<" ++ cons ++ ">"
showVal (InductiveData cons args) = "<" ++ cons ++ " " ++ unwordsList args ++ ">"
showVal (Tuple innerVals) = "[" ++ showInnerVals innerVals ++ "]"
showVal (Collection innerVals) = "{" ++ showInnerVals innerVals ++ "}"
showVal (Array _) = "#<array>"
showVal (Type _) = "#<type>"
showVal (Destructor _) = "#<destructor>"
showVal (Func _ _ _) = "(lambda [" ++ "..." ++ "] ...)"
showVal (Macro _ _ ) = "(macro [" ++ "..." ++ "] ...)"
showVal (PrimitiveFunc _) = "#<primitive>"
showVal (IOFunc _) = "#<IO primitive>"
showVal (Port _ _) = "#<IO port>"
showVal EOF = "#!EOF"

-- |Allow conversion of egisonval instances to strings
instance Show EgisonVal where show = showVal

--showInnerVals :: [InnerVal] -> String
--showInnerVals [] = ""
--showInnerVals ((Element val):rest) = show val ++ showInnerVals' rest
--showInnerVals ((SubCollection val):rest) = "@" ++ show val ++ showInnerVals' rest

--showInnerVals' :: [InnerVal] -> String
--showInnerVals' [] = ""
--showInnerVals' ((Element val):rest) = " " ++ show val ++ showInnerVals' rest
--showInnerVals' ((SubCollection val):rest) = " @" ++ show val ++ showInnerVals' rest

showInnerVals :: [InnerVal] -> String
showInnerVals iVals = unwordsList $ innerValsToList iVals

showIVal :: IntermidiateVal -> String
showIVal (IInductiveData cons []) = "<" ++ cons ++ ">"
showIVal (IInductiveData cons _) = "<" ++ cons ++ " " ++ "..." ++ ">"
showIVal (ITuple _) = "[" ++ "..." ++ "]"
showIVal (ICollection _) = "{" ++ "..." ++ "}"

-- |Allow conversion of egisonfixedval instances to strings
instance Show IntermidiateVal where show = showIVal

showObj :: Object -> String
showObj (Closure _ expr) = "(Closure env " ++  showExpr expr ++ ")"
showObj (Value val) = "(Value " ++ show val ++ ")"
showObj (Intermidiate val) = "(Intermidiate " ++ show val ++ ")"
showObj (Loop _ _ _ _ _) = "#<loop>"

-- |Allow conversion of egison object instances to strings
instance Show Object where show = showObj

showFrameList :: FrameList -> String
showFrameList [] = "{}"
showFrameList (((name,nums),_):rest) = "{[" ++ name ++ unwordsNums nums ++ ": _]" ++ loop rest
 where loop [] = "}"
       loop (((name2,nums2),_):rest2) = " [" ++ name2 ++ unwordsNums nums2 ++ ": _]" ++ loop rest2

-- - utility
stringToCharCollection :: String -> IO EgisonVal
stringToCharCollection = undefined