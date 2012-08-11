module Language.Egison.Core where
import Language.Egison.Numerical
import Language.Egison.Parser
import Language.Egison.Primitives
import Language.Egison.Types
import Language.Egison.Variables
import Language.Egison.Macro
import Control.Monad.Error
import Data.Array
import qualified Data.Map
import qualified System.Exit ()
import System.Directory (doesFileExist)
import System.IO
import qualified System.IO.Strict as SIO
import Data.IORef
import Data.Version
import Paths_egison

egisonVersion :: String
egisonVersion = showVersion version

-- |A utility function to display the egison console banner
showBanner :: IO ()
showBanner = do
  putStrLn $ "Egison Version " ++ egisonVersion ++ " (c) 2011-2012 Satoshi Egi"
  putStrLn $ "http://hagi.is.s.u-tokyo.ac.jp/~egi/egison/"
  putStrLn $ "Welcome to Egison Interpreter!"

-- |A utility function to display the egison console byebye message
showByebyeMessage :: IO ()
showByebyeMessage = do
  putStrLn $ "Leaving Egison."
  putStrLn $ "Byebye. See you again! (^^)/"

-- |Load standard libraries into the given environment
loadLibraries :: Env -> IO ()
loadLibraries env = do
  -- Load standard library
  _ <- evalString env $ "(load \"lib/core/base.egi\")"
  _ <- evalString env $ "(load \"lib/core/number.egi\")"
  _ <- evalString env $ "(load \"lib/core/collection.egi\")"
  _ <- evalString env $ "(load \"lib/core/array.egi\")"
  return ()

  
-- |A utility function to escape backslashes in the given string
escapeBackslashes :: String -> String
escapeBackslashes s = foldr step [] s
  where step x xs  | x == '\\'  = '\\' : '\\' : xs
                   | otherwise =  x : xs 


evalString :: Env -> String -> IO String
evalString env str = runIOThrowsREPL $ (liftThrows $ readTopExpr str) >>= evalTopExpr env

evalMain :: Env -> [String] -> IOThrowsError EgisonVal
evalMain env args = do
  let mainExpr = VarExpr "main" []
  main <- cEval1 $ Closure env mainExpr
  mainRef <- liftIO $ newIORef main
  let argvExpr = CollectionExpr $ map (ElementExpr . StringExpr) args
  argv <- cEval1 $ Closure env argvExpr
  argvRef <- liftIO $ newIORef argv
  worldRef <- liftIO $ newIORef $ Value $ World []
  argsRef <- liftIO $ newIORef $ Intermidiate $ ITuple $ [worldRef, argvRef]
  ret <- cApply1 mainRef argsRef
  cEval ret

-- |Evaluate egison top expression that has already been loaded into haskell
evalTopExpr :: Env -> TopExpr -> IOThrowsError String
evalTopExpr env (Test expr) = liftM show $ eval env expr
evalTopExpr env (Define name expr) = do clr <- liftIO $ makeClosure env expr
                                        defineVar env (name, []) clr
                                        return name
evalTopExpr env (Execute args) = do evalMain env args
                                    return ""
evalTopExpr env (LoadFile filename) = do
  result <- liftIO $ doesFileExist filename
  if result
    then do (liftIO $ SIO.run $ SIO.readFile filename) >>= load env
            return $ filename ++ " loaded."
    else throwError $ Default $ "File does not exist: " ++ filename
evalTopExpr env (Load libname) = do
  filename <- liftIO (getDataFileName libname)
  result <- liftIO $ doesFileExist filename
  if result
    then do (liftIO $ SIO.run $ SIO.readFile filename) >>= load env
            return $ filename ++ " loaded."
    else throwError $ Default $ "Library does not exist: " ++ libname

  
load :: Env -> String -> IOThrowsError ()
load env str = do
  exprs <- liftThrows $ readTopExprList str
  mapM (evalTopExpr env) exprs
  return ()

-- |Evaluate egison expression that has already been loaded into haskell
eval :: Env -> EgisonExpr -> IOThrowsError EgisonVal
eval env expr = do
  obj <- cEval1 (Closure env expr)
  case obj of
    Value val -> return val
    Intermidiate iVal -> iEval iVal
    _ -> throwError $ Default $ "eval: cannot reach here!: " ++ show obj

iEval :: IntermidiateVal -> IOThrowsError EgisonVal
iEval (IInductiveData cons argRefs) = do
  args <- mapM cRefEval argRefs
  return $ InductiveData cons args
iEval (ICollection innerRefs) = do
  vals <- innerRefsEval innerRefs
  return $ Collection vals
iEval (ITuple objRefs) = do
  vals <- mapM cRefEval objRefs
  return $ Tuple vals

innerRefsEval :: [InnerValRef] -> IOThrowsError [EgisonVal]
innerRefsEval [] = return []
innerRefsEval ((IElement objRef):rest) = do
  val <- cRefEval objRef
  vals <- innerRefsEval rest
  return (val:vals)
innerRefsEval ((ISubCollection subObjRef):rest) = do
  subVal <- cRefEval subObjRef
  case subVal of
    Collection vals -> do
      retRest <- innerRefsEval rest
      return $ vals ++ retRest
    _ -> throwError $ Default "innerRefsEval: not collection for subcollection"

cRefEval :: ObjectRef -> IOThrowsError EgisonVal
cRefEval objRef = do
  obj <- liftIO $ readIORef objRef
  val <- cEval obj
  liftIO $ writeIORef objRef $ Value val
  return val

cRefEval1 :: ObjectRef -> IOThrowsError Object
cRefEval1 objRef = do
  obj <- liftIO $ readIORef objRef
  case obj of
    Closure _ _ -> do
      obj2 <- cEval1 obj
      liftIO $ writeIORef objRef obj2
      return obj2
    _ -> return obj

cEval :: Object -> IOThrowsError EgisonVal
cEval (Closure env expr) = eval env expr
cEval (Value val) = return val
cEval (Intermidiate iVal) = iEval iVal
cEval (Loop _ _ _ _ _) = throwError $ Default "cEval: cannot reach here: loop object cannot be evaluated"

cEval1 :: Object -> IOThrowsError Object
cEval1 (Closure _ (BoolExpr contents)) = return $ Value (Bool contents)
cEval1 (Closure _ (CharExpr contents)) = return $ Value (Char contents)
cEval1 (Closure _ (StringExpr contents)) = return $ Value (String contents)
cEval1 (Closure _ (NumberExpr contents)) = return $ Value (Number contents)
cEval1 (Closure _ (FloatExpr contents)) = return $ Value (Float contents)
cEval1 (Closure _ SomethingExpr) = return $ Value Something
cEval1 (Closure env (VarExpr name numExprs)) = do
  numVals <- mapM (eval env) numExprs
  nums <- mapM (\nVal -> case nVal of
                           Number num -> return num
                           _ -> throwError  $ Default "cEval1: cannot reach here!")
               numVals
  objRef <- getVar env (name, nums)
  obj <- cRefEval1 objRef
  case obj of -- for Macro expansion
    Loop _ _ _ _ _ -> expandLoop env obj
    _ -> return obj
cEval1 (Closure _ (InductiveDataExpr cons [])) = do
  return $ Value $ InductiveData cons []
cEval1 (Closure env (InductiveDataExpr cons argExprs)) = do
  args <- liftIO $ mapM (makeClosure env) argExprs
  return $ Intermidiate $ IInductiveData cons args
cEval1 (Closure _ (TupleExpr [])) = do
  return $ Value $ Tuple []
cEval1 (Closure env (TupleExpr exprs)) = do
  objRefs <- liftIO $ mapM (makeClosure env) exprs
  case objRefs of
    [objRef] -> cRefEval1 objRef
    _ -> return $ Intermidiate $ ITuple objRefs
cEval1 (Closure env (CollectionExpr innerExprs)) = do
  innerRefs <- liftIO $ mapM (makeInnerValRef env) innerExprs
  return $ Intermidiate $ ICollection innerRefs
cEval1 (Closure env (ArrayExpr innerArrayExprs)) = do
  let dimension = calcDimension innerArrayExprs
  let size = calcSize innerArrayExprs
  elemExprs <- liftThrows $ calcElemExprs innerArrayExprs
  vals <- mapM (eval env) elemExprs
  return $ Value $ Array dimension size $ listArray (1, (fromIntegral (length vals))) vals
 where calcDimension aExprs = helper 1 aExprs
        where helper n [] = n
              helper n ((AElementExpr _):_) = n
              helper n ((AInnerArrayExpr inner):_) = helper (n + 1) inner
       calcSize aExprs = helper [] aExprs
        where helper ns [] = ns ++ [0]
              helper ns aExprs2@((AElementExpr _):_) = ns ++ [(fromIntegral (length aExprs2))]
              helper ns aExprs2@((AInnerArrayExpr inner):_) = helper (ns ++ [(fromIntegral (length aExprs2))]) inner
       calcElemExprs [] = return []
       calcElemExprs aExprs@((AElementExpr _):_) = mapM (\aExpr -> case aExpr of
                                                                     AElementExpr expr -> return expr
                                                                     _ -> throwError $ Default "type error in array")
                                                        aExprs
       calcElemExprs aExprs@((AInnerArrayExpr _):_) = liftM concat $ mapM (\aExpr -> case aExpr of
                                                                                       AInnerArrayExpr aExprs2 -> calcElemExprs aExprs2
                                                                                       _ -> throwError $ Default "type error in array")
                                                                          aExprs
cEval1 (Closure _ WildCardExpr) = return $ Value WildCard
cEval1 (Closure env (PatVarExpr name numExprs)) = do
  numVals <- mapM (eval env) numExprs
  nums <- mapM (\nVal -> case nVal of
                           Number num -> return num
                           _ -> throwError $ Default "cEval1: cannot reach here!")
               numVals
  return $ Value $ PatVar name nums
cEval1 (Closure env (ValuePatExpr expr)) = do
  objRef <- liftIO $ makeClosure env expr
  return $ Value $ ValuePat objRef
cEval1 (Closure env (PredPatExpr predExpr argExprs)) = do
  predObjRef <- liftIO $ makeClosure env predExpr
  argObjRefs <- liftIO $ mapM (makeClosure env) argExprs
  return $ Value $ PredPat predObjRef argObjRefs
cEval1 (Closure env (CutPatExpr patExpr)) = do
  patObjRef <- liftIO $ makeClosure env patExpr
  return $ Value $ CutPat patObjRef
cEval1 (Closure env (NotPatExpr patExpr)) = do
  patObjRef <- liftIO $ makeClosure env patExpr
  return $ Value $ NotPat patObjRef
cEval1 (Closure env (OrPatExpr patExprs)) = do
  patObjRefs <- liftIO $ mapM (makeClosure env) patExprs
  return $ Value $ OrPat patObjRefs
cEval1 (Closure env (AndPatExpr patExprs)) = do
  patObjRefs <- liftIO $ mapM (makeClosure env) patExprs
  return $ Value $ AndPat patObjRefs
cEval1 (Closure env (FuncExpr args body)) = do
  argsObjRef <- liftIO $ makeClosure env args
  return $ Value $ Func argsObjRef body env
cEval1 (Closure _ (MacroExpr args body)) = do
  return $ Value $ Macro args body
cEval1 (Closure env (IfExpr condExpr expr1 expr2)) = do
  obj <- cEval1 $ Closure env condExpr
  case obj of
    Value (Bool True) -> cEval1 $ Closure env expr1
    Value (Bool False) -> cEval1 $ Closure env expr2
    _ -> throwError $ Default "if: condition is not bool value"
cEval1 (Closure env (LetExpr bindings body)) = do
  newEnv <- extendLet env bindings
  cEval1 (Closure newEnv body)
cEval1 (Closure env (LetRecExpr bindings body)) = do
  newEnv <- liftIO $ extendLetRec env bindings
  cEval1 $ Closure newEnv body
cEval1 (Closure env (DoExpr [] body)) = do
  cEval1 $ Closure env body
cEval1 (Closure env (DoExpr (binding:bindings) body)) = do
  newEnv <- extendLet env [binding]
  cEval1 $ Closure newEnv $ DoExpr bindings body
cEval1 (Closure env (TypeExpr destructInfoExpr)) = do
  destructInfo <- liftIO $ makeDestructInfo destructInfoExpr
  return $ Value $ Type destructInfo
 where makeDestructInfo [] = return []
       makeDestructInfo ((pppat, typExpr, pmcs):rest) = do
         typObjRef <- liftIO $ makeClosure env typExpr
         let epmcs = map (\(ppat, body) -> (env, ppat, body)) pmcs
         retRest <- makeDestructInfo rest
         return ((pppat, typObjRef, epmcs):retRest)
cEval1 (Closure env (MatchAllExpr tgtExpr typExpr (patExpr, body))) = do
  patObjRef <- liftIO $ makeClosure env patExpr
  tgtObjRef <- liftIO $ makeClosure env tgtExpr
  typObjRef <- liftIO $ makeClosure env typExpr
  matchs <- patternMatch MAll [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]
  rets <- mapM (\match -> do newEnv <- liftIO $ extendEnv env match
                             objRef <- liftIO $ newIORef (Closure newEnv body)
                             return objRef)
               matchs
  return $ Intermidiate $ ICollection $ map IElement rets
cEval1 (Closure env (MatchExpr tgtExpr typExpr mcs)) = do
  tgtObjRef <- liftIO $ makeClosure env tgtExpr
  typObjRef <- liftIO $ makeClosure env typExpr
  mcLoop tgtObjRef typObjRef mcs
 where mcLoop _ _ [] = throwError $ Default "end of match clauses"
       mcLoop tgtObjRef typObjRef ((patExpr, body):rest) = do
         patObjRef <- liftIO $ makeClosure env patExpr
         matchs <- patternMatch MOne [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]
         case matchs of
           [] -> mcLoop tgtObjRef typObjRef rest
           [match] -> do newEnv <- liftIO $ extendEnv env match
                         cEval1 (Closure newEnv body)
           _ -> throwError $ Default "cEval1.mcLoop: cannot reach here"
cEval1 (Closure env (LoopExpr loopVar indexVar rangeExpr loopExpr tailExpr)) = do
  rangeObjRef <- liftIO $ makeClosure env rangeExpr
  let loopObj = Loop loopVar indexVar rangeObjRef loopExpr tailExpr
  expandLoop env loopObj
cEval1 (Closure env (GenerateArrayExpr fnExpr rangeExpr)) = do
  fnObjRef <- liftIO $ makeClosure env fnExpr
  rangeVal <- eval env rangeExpr
  ms <- liftThrows $ mapM unpackNum $ tupleToList rangeVal
  let d = fromIntegral $ length ms
  let is = map (\iss -> (Value . Tuple) $ map Number iss) $ indexList ms
  isRefs <- liftIO $ mapM newIORef is
  vals <- mapM (cApply fnObjRef) isRefs
  return $ Value $ Array d ms $ listArray (1, (fromIntegral (length vals))) vals
cEval1 (Closure env (ApplyExpr opExpr argExpr)) = do
  op <- cEval1 (Closure env opExpr)
  case op of
    Value (IOFunc fn) -> do arg <- eval env argExpr
                            val <- fn (tupleToList arg)
                            return $ Value val
    Value (PrimitiveFunc fn) -> do arg <- eval env argExpr
                                   val <- liftThrows $ fn (tupleToList arg)
                                   return $ Value val
    Value (Func fArgs body cEnv) -> do argObjRef <- liftIO $ makeClosure env argExpr
                                       frame <- makeFrame fArgs argObjRef
                                       newEnv <- liftIO $ extendEnv cEnv frame
                                       cEval1 (Closure newEnv body)
    Value (Macro mArgs body) -> do argExprs <- liftThrows $ tupleExprToExprList argExpr
                                   newBody <- expandMacro (Data.Map.fromList (zip mArgs argExprs)) body
                                   liftIO $ putStrLn $ showExpr newBody
                                   cEval1 (Closure env newBody)
    _ -> throwError $ Default "not function"
cEval1 (Closure _ UndefinedExpr) = do
  throwError ReachToUndefined
cEval1 val = do
  return val

cApply :: ObjectRef -> ObjectRef -> IOThrowsError EgisonVal
cApply fnObjRef argObjRef = do
  fnObj <- cRefEval1 fnObjRef
  case fnObj of
    Value (IOFunc fn) -> do arg <- cRefEval argObjRef
                            val <- fn (tupleToList arg)
                            return $ val
    Value (PrimitiveFunc fn) -> do arg <- cRefEval argObjRef
                                   val <- liftThrows $ fn (tupleToList arg)
                                   return $ val
    Value (Func fArgs body cEnv) -> do frame <- makeFrame fArgs argObjRef
                                       newEnv <- liftIO $ extendEnv cEnv frame
                                       cEval (Closure newEnv body)
    _ -> throwError $ Default "cApply1 not function"

cApply1 :: ObjectRef -> ObjectRef -> IOThrowsError Object
cApply1 fnObjRef argObjRef = do
  fnObj <- cRefEval1 fnObjRef
  case fnObj of
    Value (IOFunc fn) -> do arg <- cRefEval argObjRef
                            val <- fn (tupleToList arg)
                            return $ Value val
    Value (PrimitiveFunc fn) -> do arg <- cRefEval argObjRef
                                   val <- liftThrows $ fn (tupleToList arg)
                                   return $ Value val
    Value (Func fArgs body cEnv) -> do frame <- makeFrame fArgs argObjRef
                                       newEnv <- liftIO $ extendEnv cEnv frame
                                       cEval1 (Closure newEnv body)
    _ -> throwError $ Default "cApply1: not function"


expandLoop :: Env -> Object -> IOThrowsError Object
expandLoop env (Loop loopVar indexVar rangeObjRef loopExpr tailExpr) = do
  cRefEval1 rangeObjRef
  b <- isEmptyCollection rangeObjRef
  if b
    then cEval1 $ Closure env tailExpr
    else do (carObjRef,cdrObjRef) <- consDestruct rangeObjRef
            loopObjRef <- liftIO $ newIORef $ Loop loopVar indexVar cdrObjRef loopExpr tailExpr
            newEnv <- liftIO $ extendEnv env [((loopVar,[]),loopObjRef),((indexVar,[]),carObjRef)]
            cEval1 $ Closure newEnv loopExpr
expandLoop _ obj = throwError $ Default $ "expandLoop: cannot reach here: " ++ show obj
           
extendLet :: Env -- ^ Environment 
          -> [(EgisonExpr, EgisonExpr)] -- ^ Extensions to the environment
          -> IOThrowsError Env -- ^ Extended environment
extendLet env binding = do
 frame <- helper binding
 liftIO $ extendEnv env frame
 where helper :: [(EgisonExpr, EgisonExpr)] -> IOThrowsError FrameList
       helper [] = return []
       helper ((patExpr, tgtExpr):rest) = do
         patObjRef <- liftIO $ makeClosure env patExpr
         tgtObjRef <- liftIO $ makeClosure env tgtExpr
         frame <- makeFrame patObjRef tgtObjRef
         retRest <- helper rest
         return $ frame ++ retRest

makeFrame :: ObjectRef -> ObjectRef -> IOThrowsError FrameList
makeFrame patObjRef tgtObjRef = do
  patObjRefs <- tupleToObjRefs patObjRef
  let n = length patObjRefs
  if n == 1
    then do typObjRef <- liftIO $ newIORef $ Value Something
            matchs <- patternMatch MOne [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]           
            case matchs of
              [match] -> return match
              _ -> throwError $ Default "makeFrame: binding error"
    else do typObjRef <- liftIO $ newIORef $ (Value . Tuple) $ replicate (length patObjRefs) Something
            matchs <- patternMatch MOne [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]           
            case matchs of
              [match] -> do return match
              _ -> throwError $ Default "makeFrame: binding error"

tupleExprToExprList :: EgisonExpr -> ThrowsError [EgisonExpr]
tupleExprToExprList (TupleExpr exprs) = return exprs
tupleExprToExprList expr = return [expr]

innerExprsToExprList :: [InnerExpr] -> ThrowsError [EgisonExpr]
innerExprsToExprList [] = return []
innerExprsToExprList ((ElementExpr expr):rest) = do retRest <- innerExprsToExprList rest
                                                    return (expr:retRest)
innerExprsToExprList ((SubCollectionExpr _):_) = throwError $ Default "innerExprsToExprList: subcollection is not supported"
    
innerValRefsToObjRefs :: [InnerValRef] -> IOThrowsError [ObjectRef]
innerValRefsToObjRefs [] = return []
innerValRefsToObjRefs (innerRef:rest) = do
  restRet <- innerValRefsToObjRefs rest
  case innerRef of
    IElement objRef -> return $ objRef:restRet
    ISubCollection objRef -> do
      obj2 <- cRefEval1 objRef
      case obj2 of
        Intermidiate (ICollection innerRefs) -> do
          objRefs <- innerValRefsToObjRefs innerRefs
          return $ objRefs ++ restRet
        Value (Collection vals) -> do
          objRefs <- liftIO $ mapM newIORef $ map Value vals
          return $ objRefs ++ restRet
        _ -> throwError $ Default "innerValRefsToObjRefs: not collection"

patternMatch :: MatchFlag -> [MState] -> IOThrowsError [FrameList]
patternMatch _ [] = return []
patternMatch MAll ((MState frame []):rest) = do
  frames <- patternMatch MAll rest
  return (frame:frames)
patternMatch MOne ((MState frame []):_) = do
  return [frame]
patternMatch flag ((MState frame ((MAtom (PClosure bf patObjRef) tgtObjRef typObjRef)
                                  :atoms))
                 :states) = do
  patObj <- liftIO $ readIORef patObjRef
  case patObj of
    Closure env expr ->
      do newEnv <- liftIO $ extendEnv env bf
         patObj2 <- cEval1 $ Closure newEnv expr
         patObjRef2 <- liftIO $ newIORef patObj2
         patternMatch flag ((MState frame ((MAtom (PClosure [] patObjRef2) tgtObjRef typObjRef)
                                           :atoms))
                            :states)
    Value WildCard -> patternMatch flag ((MState frame atoms):states)
    Value (PatVar name nums) -> do
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type deconsInfo) -> do
          indRet <- inductiveMatch deconsInfo patObjRef tgtObjRef
          case indRet of
            (inTypObjRefs, inPatObjRefs, inTgtObjRefss) -> do
              patternMatch flag ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                            (zip3 inPatObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                      inTgtObjRefss) ++ states)
        Value Something ->
          patternMatch flag $ (MState (((name, nums), tgtObjRef):frame)
                                      (map (\(MAtom (PClosure bf2 pat2) tgt2 typ2) ->
                                             (MAtom (PClosure (((name, nums), tgtObjRef):bf2) pat2) tgt2 typ2))
                                           atoms)):states
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Value (ValuePat _) -> do -- same with Inductive Pattern
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type deconsInfo) -> do
          indRet <- inductiveMatch deconsInfo patObjRef tgtObjRef
          case indRet of
            (inTypObjRefs, inPatObjRefs, inTgtObjRefss) -> do
              patternMatch flag ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                            (zip3 inPatObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                      inTgtObjRefss) ++ states)
        Value Something -> throwError $ Default "patternMatch: Only pattern variable can be pattern matched with Something"
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Value (InductiveData _ _) -> do -- same with ValuePat
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type deconsInfo) -> do
          indRet <- inductiveMatch deconsInfo patObjRef tgtObjRef
          case indRet of
            (inTypObjRefs, inPatObjRefs, inTgtObjRefss) -> do
              patternMatch flag ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                            (zip3 inPatObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                      inTgtObjRefss) ++ states)
        Value Something -> throwError $ Default "patternMatch: Only pattern variable can be pattern matched with Something"
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Intermidiate (IInductiveData _ _) -> do -- same with ValuePat
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type deconsInfo) -> do
          indRet <- inductiveMatch deconsInfo patObjRef tgtObjRef
          case indRet of
            (inTypObjRefs, inPatObjRefs, inTgtObjRefss) -> do
              patternMatch flag ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                            (zip3 inPatObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                      inTgtObjRefss) ++ states)
        Value Something -> throwError $ Default "patternMatch: Only pattern variable can be pattern matched with Something"
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Intermidiate (ITuple patObjRefs) -> do
      typObjRefs <- tupleToObjRefs typObjRef
      if length typObjRefs == 1
        then do
          let tgtObjRefs = [tgtObjRef]
          if (length typObjRefs == length patObjRefs) && (length typObjRefs == length tgtObjRefs)
            then patternMatch flag $ (MState frame ((map (\(pat,tgt,typ) -> MAtom (PClosure bf pat) tgt typ) (zip3 patObjRefs tgtObjRefs typObjRefs)) ++ atoms)):states
            else throwError $ Default "patternMatch(ITuple-1): number of types, patterns and targets are different"
        else do
          tgtObjRefs <- tupleToObjRefs tgtObjRef
          if (length typObjRefs == length patObjRefs) && (length typObjRefs == length tgtObjRefs)
            then patternMatch flag $ (MState frame ((map (\(pat,tgt,typ) -> MAtom (PClosure bf pat) tgt typ) (zip3 patObjRefs tgtObjRefs typObjRefs)) ++ atoms)):states
            else throwError $ Default "patternMatch(ITuple): number of types, patterns and targets are different"
    Value (Tuple pats) -> do
      patObjRefs <- liftIO $ mapM (newIORef . Value) pats
      typObjRefs <- tupleToObjRefs typObjRef
      if length typObjRefs == 1
        then do
          let tgtObjRefs = [tgtObjRef]
          if (length typObjRefs == length patObjRefs) && (length typObjRefs == length tgtObjRefs)
            then patternMatch flag $ (MState frame ((map (\(pat,tgt,typ) -> MAtom (PClosure bf pat) tgt typ) (zip3 patObjRefs tgtObjRefs typObjRefs)) ++ atoms)):states
            else throwError $ Default "patternMatch(Tuple-1): number of types, patterns and targets are different"
        else do
          tgtObjRefs <- tupleToObjRefs tgtObjRef
          if (length typObjRefs == length patObjRefs) && (length typObjRefs == length tgtObjRefs)
            then patternMatch flag $ (MState frame ((map (\(pat,tgt,typ) -> MAtom (PClosure bf pat) tgt typ) (zip3 patObjRefs tgtObjRefs typObjRefs)) ++ atoms)):states
            else throwError $ Default "patternMatch(Tuple): number of types, patterns and targets are different"
    Value (PredPat predObjRef patObjRefs) -> do
      argsObjRef <- liftIO $ newIORef $ Intermidiate $ ITuple $ patObjRefs ++ [tgtObjRef]
      ret <- cApply1 predObjRef argsObjRef
      case ret of
        Value (Bool True) -> patternMatch flag ((MState frame atoms):states)
        Value (Bool False) -> patternMatch flag states
        _ -> throwError (Default "patternMatch: return value of pred-pattern is not boolean value")
    Value (NotPat patObjRef2) -> do
      retFrames <- patternMatch MOne [(MState frame [(MAtom (PClosure bf patObjRef2) tgtObjRef typObjRef)])]
      case retFrames of
        [] -> patternMatch flag ((MState frame atoms):states)
        _ -> patternMatch flag states
    Value (AndPat patObjRefs) ->
      patternMatch flag ((MState frame ((map (\patObjRef2 -> (MAtom (PClosure bf patObjRef2) tgtObjRef typObjRef)) patObjRefs) ++ atoms)):states)
    Value (OrPat patObjRefs) ->
      patternMatch flag ((map (\patObjRef2 -> (MState frame ((MAtom (PClosure bf patObjRef2) tgtObjRef typObjRef):atoms)))
                              patObjRefs) ++ states)
    Value (CutPat patObjRef2) -> do
      retFrames <- patternMatch flag [(MState frame ((MAtom (PClosure bf patObjRef2) tgtObjRef typObjRef):atoms))]
      case retFrames of
        [] -> return []
        _ -> case flag of
               MAll -> do restFrames <- patternMatch flag states
                          return (retFrames ++ restFrames)
               MOne -> return retFrames
    _ -> throwError $ Default $ "invalid pattern: " ++ show patObj
        
inductiveMatch :: DestructInfo -> ObjectRef -> ObjectRef -> IOThrowsError ([ObjectRef], [ObjectRef], [[ObjectRef]])
inductiveMatch [] _ _ = throwError $ Default "inductiveMatch: not matched any primitive pattern clauses"
inductiveMatch ((pppat, typObjRef, pclss):rest) patObjRef tgtObjRef = do
  mRet <- primitivePatPatternMatch pppat patObjRef
  case mRet of
    Nothing -> inductiveMatch rest patObjRef tgtObjRef
    Just (patObjRefs, frame) -> do
      mPpmRet <- helper pclss
      case mPpmRet of
        Nothing -> throwError $ Default "inductiveMatch: not matched any primitive clauses"
        Just tgtObjRefs -> do
          typObjRefs <- tupleToObjRefs typObjRef
          if length typObjRefs == 1
            then do let tgtObjRefss = map (\x -> [x]) tgtObjRefs
                    return (typObjRefs, patObjRefs, tgtObjRefss)
            else do tgtObjRefss <- mapM tupleToObjRefs tgtObjRefs
                    return (typObjRefs, patObjRefs, tgtObjRefss)
     where helper [] = return Nothing
           helper ((env, ppat, expr):pclss2) = do
             mPpmRet <- primitivePatternMatch ppat tgtObjRef
             case mPpmRet of
               Nothing -> helper pclss2
               Just ppmRet -> do
                 newEnv <- liftIO $ extendEnv env (ppmRet ++ frame)
                 retObjRef <- liftIO $ makeClosure newEnv expr
                 tgtObjRefs <- collectionToObjRefs retObjRef
                 return $ Just tgtObjRefs

primitivePatPatternMatch :: PrimitivePatPattern -> ObjectRef -> IOThrowsError (Maybe ([ObjectRef], FrameList))
primitivePatPatternMatch PPWildCard patObjRef = return $ Just ([patObjRef], [])
primitivePatPatternMatch (PPValuePat name) patObjRef = do
  patObj <- cRefEval1 patObjRef
  case patObj of
    Value (ValuePat objRef) -> return $ Just ([], [((name, []), objRef)])
    _ -> return $ Nothing
primitivePatPatternMatch (PPInductivePat ppcons pppats) patObjRef = do
  patObj <- cRefEval1 patObjRef
  case patObj of
    Intermidiate (IInductiveData pcon patObjRefs) ->
      if ppcons == pcon
        then primitivePatPatternMatchList pppats patObjRefs
        else return Nothing
    Value (InductiveData pcon patVals) -> do
      patObjRefs <- liftIO $ mapM (newIORef . Value) patVals
      if ppcons == pcon
        then primitivePatPatternMatchList pppats patObjRefs
        else return Nothing
    _ -> return $ Nothing

primitivePatPatternMatchList :: [PrimitivePatPattern] -> [ObjectRef] -> IOThrowsError (Maybe ([ObjectRef], FrameList))
primitivePatPatternMatchList [] [] = return $ Just ([], [])
primitivePatPatternMatchList (pppat:pppats) (patObjRef:patObjRefs) = do
  mRet <- primitivePatPatternMatch pppat patObjRef
  case mRet of
    Nothing -> return Nothing
    Just (retPatObjRefs, retFrame) -> do
      mRetRest <- primitivePatPatternMatchList pppats patObjRefs
      case mRetRest of
        Nothing -> return Nothing
        Just (retRestPatObjRefs, retRestFrame) -> return $ Just (retPatObjRefs ++ retRestPatObjRefs, retFrame ++ retRestFrame)
primitivePatPatternMatchList pppats _ = throwError $ Default $ "primitivePatPatternMatch: number of pppat and pat are different: " ++ show pppats
    
primitivePatternMatch :: PrimitivePattern -> ObjectRef -> IOThrowsError (Maybe FrameList)
primitivePatternMatch (PPatBool bool) objRef = do
  val <- cRefEval objRef
  case val of
    Bool bool2 -> if bool == bool2
                   then return (Just [])
                   else return Nothing
    _ -> return Nothing
primitivePatternMatch (PPatChar chr) objRef = do
  val <- cRefEval objRef
  case val of
    Char chr2 -> if chr == chr2
                   then return (Just [])
                   else return Nothing
    _ -> return Nothing
primitivePatternMatch (PPatNumber num) objRef = do
  val <- cRefEval objRef
  case val of
    Number num2 -> if num == num2
                     then return (Just [])
                     else return Nothing
    _ -> return Nothing
primitivePatternMatch (PPatFloat d) objRef = do
  val <- cRefEval objRef
  case val of
    Float d2 -> if d == d2
                  then return (Just [])
                  else return Nothing
    _ -> return Nothing
primitivePatternMatch PWildCard _ = return $ Just []
primitivePatternMatch (PPatVar name) objRef = return (Just [((name,[]), objRef)])
primitivePatternMatch (PInductivePat pCons pPats) objRef =  do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (IInductiveData cons objRefs) ->
      if pCons == cons
        then primitivePatternMatchList pPats objRefs
        else return Nothing
    Value (InductiveData cons vals) ->
      if pCons == cons
        then do objRefs <- liftIO $ mapM (newIORef . Value) vals
                primitivePatternMatchList pPats objRefs
        else return Nothing
    _ -> return Nothing
primitivePatternMatch PEmptyPat objRef = do
  cRefEval1 objRef
  b <- isEmptyCollection objRef
  if b
    then return (Just [])
    else return Nothing
primitivePatternMatch (PConsPat carPat cdrPat) objRef = do
  cRefEval1 objRef
  b <- isEmptyCollection objRef
  if b
    then return Nothing
    else do (carObjRef, cdrObjRef) <- consDestruct objRef
            mCarFrame <- primitivePatternMatch carPat carObjRef
            case mCarFrame of
              Nothing -> return Nothing
              Just carFrame -> do mCdrFrame <- primitivePatternMatch cdrPat cdrObjRef
                                  case mCdrFrame of
                                    Nothing -> return Nothing
                                    Just cdrFrame -> return (Just (carFrame ++ cdrFrame))
primitivePatternMatch (PSnocPat rdcPat racPat) objRef = do
  cRefEval1 objRef
  b <- isEmptyCollectionForSnoc objRef
  if b
    then return Nothing
    else do (racObjRef, rdcObjRef) <- snocDestruct objRef
            mRacFrame <- primitivePatternMatch racPat racObjRef
            case mRacFrame of
              Nothing -> return Nothing
              Just racFrame -> do mRdcFrame <- primitivePatternMatch rdcPat rdcObjRef
                                  case mRdcFrame of
                                    Just rdcFrame -> return (Just (racFrame ++ rdcFrame))
                                    Nothing -> return Nothing
            
primitivePatternMatchList :: [PrimitivePattern] -> [ObjectRef] -> IOThrowsError (Maybe FrameList)
primitivePatternMatchList [] [] = return (Just [])
primitivePatternMatchList (pat:pats) (objRef:objRefs) = do
  mFrame <- primitivePatternMatch pat objRef
  case mFrame of
    Nothing -> return Nothing
    Just frame -> do mRestFrame <- primitivePatternMatchList pats objRefs
                     case mRestFrame of
                       Nothing -> return Nothing
                       Just restFrame -> return (Just (frame ++ restFrame))
primitivePatternMatchList _ _ = throwError $ Default "primitivePatternMatchList : number of patterns and targets are different"


objectRefToInnerRefs :: ObjectRef -> IOThrowsError [InnerValRef]
objectRefToInnerRefs objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> return innerRefs
    Value (Collection val) -> do
      objRefs <- liftIO $ mapM (newIORef . Value) val
      return $ map IElement objRefs
    _ -> throwError $ Default "objectRefToInnerRefs: subcollection must be collection"

isEmptyCollection :: ObjectRef -> IOThrowsError Bool
isEmptyCollection objRef = do
  obj <- liftIO $ readIORef objRef
  case obj of
    Intermidiate (ICollection []) -> return True
    Intermidiate (ICollection ((IElement _):_)) -> return False
    Intermidiate (ICollection ((ISubCollection subObjRef):rest)) -> do
      innerRefs <- objectRefToInnerRefs subObjRef
      liftIO $ writeIORef objRef $ Intermidiate $ ICollection $ innerRefs ++ rest
      isEmptyCollection objRef
    Value (Collection []) -> return True
    Value (Collection _) -> return False
    _ -> throwError $ Default $ "isEmptyCollection: not collection:" ++ show obj

isEmptyCollectionForSnoc :: ObjectRef -> IOThrowsError Bool
isEmptyCollectionForSnoc objRef = do
  obj <- liftIO $ readIORef objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> do
      case reverse innerRefs of
        [] -> return True
        ((IElement _):_) -> return False
        ((ISubCollection subObjRef):rest) -> do
          subInnerRefs <- objectRefToInnerRefs subObjRef
          liftIO $ writeIORef objRef $ Intermidiate $ ICollection $ reverse rest ++ subInnerRefs
          isEmptyCollectionForSnoc objRef
    Value (Collection []) -> return True
    Value (Collection _) -> return False
    _ -> throwError $ Default $ "isEmptyCollectionForSnoc: not collection:" ++ show obj

consDestruct :: ObjectRef -> IOThrowsError (ObjectRef, ObjectRef)
consDestruct objRef = do
  obj <- liftIO $ readIORef objRef
  case obj of
    Intermidiate (ICollection []) -> throwError $ Default "consDestructInnerRefs: empty collection"
    Intermidiate (ICollection ((IElement carObjRef):rest)) -> do
      cdrObjRef <- liftIO $ newIORef $ Intermidiate $ ICollection rest
      return (carObjRef, cdrObjRef)
    Intermidiate (ICollection ((ISubCollection subObjRef):rest)) -> do
      innerRefs <- objectRefToInnerRefs subObjRef
      liftIO $ writeIORef objRef $ Intermidiate $ ICollection $ innerRefs ++ rest
      consDestruct objRef
    Value (Collection (val:vals)) -> do
      carObjRef <- liftIO $ newIORef $ Value val
      cdrObjRef <- liftIO $ newIORef $ Value $ Collection vals
      return (carObjRef, cdrObjRef)
    _ -> throwError $ Default "consDestruct: not collection"

snocDestruct :: ObjectRef -> IOThrowsError (ObjectRef, ObjectRef)
snocDestruct objRef = do
  obj <- liftIO $ readIORef objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> do
      case reverse innerRefs of
        [] -> throwError $ Default "snocDestructInnerRefs: empty collection"
        ((IElement racObjRef):rest) -> do
          rdcObjRef <- liftIO $ newIORef $ Intermidiate $ ICollection $ reverse rest
          return (racObjRef, rdcObjRef)
        ((ISubCollection subObjRef):rest) -> do
          subInnerRefs <- objectRefToInnerRefs subObjRef
          liftIO $ writeIORef objRef $ Intermidiate $ ICollection $ reverse rest ++ subInnerRefs
          snocDestruct objRef
    Value (Collection vals) -> do
      case reverse vals of
        (val:rest) -> do
          racObjRef <- liftIO $ newIORef $ Value val
          rdcObjRef <- liftIO $ newIORef $ Value $ Collection rest
          return (racObjRef, rdcObjRef)
        _ -> throwError $ Default "snocDestruct: not collection"
    _ -> throwError $ Default "snocDestruct: not collection"
    
collectionToObjRefs :: ObjectRef -> IOThrowsError [ObjectRef]
collectionToObjRefs objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> innerRefsToObjRefs innerRefs
    Value (Collection vals) -> liftIO $ mapM (newIORef . Value) vals
    _ -> throwError $ Default "collectionToObjRefs: not collection"

tupleToObjRefs :: ObjectRef -> IOThrowsError [ObjectRef]
tupleToObjRefs objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ITuple objRefs) -> return objRefs
    Value (Tuple vals) -> liftIO $ mapM (newIORef . Value) vals
    _ -> return [objRef]

innerRefsToObjRefs :: [InnerValRef] -> IOThrowsError [ObjectRef]
innerRefsToObjRefs [] = return []
innerRefsToObjRefs ((IElement objRef):rest) = do
  retRest <- innerRefsToObjRefs rest
  return $ objRef:retRest
innerRefsToObjRefs ((ISubCollection objRef):rest) = do
  retObj <- collectionToObjRefs objRef
  retRest <- innerRefsToObjRefs rest
  return $ retObj ++ retRest

  
primitiveBindings :: IO Env
primitiveBindings = do
  initEnv <- nullEnv
  constantsFrameList <- mapM domakeObjRef constants
  iOFuncs <- mapM (domakeFunc IOFunc) ioPrimitives
  primitiveFuncs <- mapM (domakeFunc PrimitiveFunc) primitives
  extendEnv initEnv (constantsFrameList ++ iOFuncs ++ primitiveFuncs)
 where domakeFunc constructor (name, func) = do
         objRef <- newIORef $ Value $ constructor func
         return ((name, []), objRef)
       domakeObjRef (name, val) = do
         objRef <- newIORef $ Value val
         return ((name, []), objRef)


constants :: [(String, EgisonVal)]
constants = [("pi", Float 3.141592653589793)
             ]
         
{- I/O primitives -}
ioPrimitives :: [(String, [EgisonVal] -> IOThrowsError EgisonVal)]
ioPrimitives = [("get-lib-dir-name", getLibDirName),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read-char", readChar),
                ("read-line", readLine),
                ("read", readFromStdin),
                ("write-char", writeChar),
                ("write-string", writeString),
                ("print", writeStringLine),
                ("write", write),
                ("flush", flushStdout),
                ("read-char-from-port", readCharFromPort),
                ("read-line-from-port", readLineFromPort),
                ("read-from-port", readFromPort),
                ("write-char-to-port", writeCharToPort),
                ("write-string-to-port", writeStringToPort),
                ("print-to-port", writeStringLineToPort),
                ("write-to-port", writeToPort),
                ("flush-port", flushPort)
                ]

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

              ("eq?", eqv),
              
              ("eq-n?", numBoolBinop (==)),
              ("lt-n?", numBoolBinop (<)),
              ("lte-n?", numBoolBinop (<=)),
              ("gt-n?", numBoolBinop (>)),
              ("gte-n?", numBoolBinop (>=)),

              ("eq-f?", floatBoolBinop (==)),
              ("lt-f?", floatBoolBinop (<)),
              ("lte-f?", floatBoolBinop (<=)),
              ("gt-f?", floatBoolBinop (>)),
              ("gte-f?", floatBoolBinop (>=)),

              ("eq-c?", charBoolBinop (==)),
              ("eq-s?", strBoolBinop (==)),

              ("string-append", stringBinop (++)),
              
              ("string-to-chars", stringToChars),
              ("chars-to-string", charsToString),
              
              ("&&", boolBinop (&&)),
              ("||", boolBinop (||)),

              ("array-dimension", arrayDimension),
              ("array-range", arrayRange),
              ("array-size", arraySize),
              ("array-keys", arrayKeys),

              ("array-range?", arrayIsRange),

              ("array-ref", arrayRef),
--              ("array-sub-ref", arrayRef),
              
--              ("array-to-collection", arrayToCollection),
--              ("collection-to-array", collectionToArray),

              ("eof?", isEgisonEOF)

              ]

