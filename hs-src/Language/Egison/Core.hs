module Language.Egison.Core where
import Language.Egison.Numerical
import Language.Egison.Parser
import Language.Egison.Primitives
import Language.Egison.Types
import Language.Egison.Variables
import Control.Monad.Error
import Data.Array
import qualified Data.Map
import qualified System.Exit
import System.Directory (doesFileExist, removeFile)
import System.IO
import Data.IORef
import Paths_egison

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

--libraries :: [String]
--libraries = ["lib/core/base.egi", "lib/core/number.egi", "lib/core/collection.egi"]
  
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
  argsRef <- liftIO $ newIORef $ Intermidiate $ ITuple $ [IElement worldRef, IElement argvRef]
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
    then do (liftIO $ readFile filename) >>= load env
            return $ filename ++ " loaded."
    else throwError $ Default $ "File does not exist: " ++ filename
evalTopExpr env (Load libname) = do
  filename <- liftIO (getDataFileName libname)
  result <- liftIO $ doesFileExist filename
  if result
    then do (liftIO $ readFile filename) >>= load env
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
iEval (ICollection innerValRefs) = do
  innerVals <- mapM innerValRefEval innerValRefs
  return $ Collection innerVals
iEval (ITuple innerValRefs) = do
  innerVals <- mapM innerValRefEval innerValRefs
  return $ Tuple innerVals

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
cEval1 (Closure _ (BoolExpr contents)) = return $ Value (Bool contents)
cEval1 (Closure _ (CharExpr contents)) = return $ Value (Char contents)
cEval1 (Closure _ (StringExpr contents)) = return $ Value (String contents)
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
  case obj of -- for Macro expansion
    Loop _ _ _ _ _ -> expandLoop env obj
    _ -> return obj
cEval1 (Closure env (InductiveDataExpr cons argExprs)) = do
  args <- liftIO $ mapM (makeClosure env) argExprs
  return $ Intermidiate $ IInductiveData cons args
cEval1 (Closure env (TupleExpr innerExprs)) = do
  innerRefs <- liftIO $ mapM (makeInnerValRef env) innerExprs
  objRefs <- innerRefsToObjRefList innerRefs
  case objRefs of
    [objRef] -> cRefEval1 objRef
    _ -> return $ Intermidiate $ ITuple innerRefs
cEval1 (Closure env (CollectionExpr innerExprs)) = do
  innerRefs <- liftIO $ mapM (makeInnerValRef env) innerExprs
  return $ Intermidiate $ ICollection innerRefs
cEval1 (Closure _ WildCardExpr) = return $ Value WildCard
cEval1 (Closure env (PatVarExpr name numExprs)) = do
  numVals <- mapM (eval env) numExprs
  nums <- mapM (\nVal -> case nVal of
                           Number num -> return num
                           _ -> throwError $ Default "cEval1: cannot reach here!")
               numVals
  return $ Value $ PatVar name nums
cEval1 (Closure env (PredPatExpr predName argExprs)) = do
  argObjRefs <- liftIO $ mapM (makeClosure env) argExprs
  return $ Value $ PredPat predName argObjRefs
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
  return $ Value $ Func args body env
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
cEval1 (Closure env (TypeExpr bindings)) = do
  frameRef <- liftIO $ makeLetRecFrame env bindings
  frame <- liftIO $ readIORef frameRef
  return $ Value $ Type frame
cEval1 (Closure env (TypeRefExpr typExpr name)) = do
  obj <- cEval1 (Closure env typExpr)
  case obj of
    Value (Type frame) -> getVarFromFrame frame (name,[]) >>= cRefEval1
    _ -> throwError $ Default "type-ref: not type"
cEval1 (Closure env (DestructorExpr destructInfoExpr)) = do
  destructInfo <- liftIO $ makeDestructInfo destructInfoExpr
  return $ Value $ Destructor destructInfo
 where makeDestructInfo [] = return []
       makeDestructInfo ((cons, typExpr, pmcs):rest) = do
         typObjRef <- liftIO $ makeClosure env typExpr
         let epmcs = map (\(ppat, body) -> (env, ppat, body)) pmcs
         retRest <- makeDestructInfo rest
         return ((cons, typObjRef, epmcs):retRest)
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
  retRef <- mcLoop tgtObjRef typObjRef mcs
  cRefEval1 retRef
 where mcLoop _ _ [] = throwError $ Default "end of match clauses"
       mcLoop tgtObjRef typObjRef ((patExpr, body):rest) = do
         patObjRef <- liftIO $ makeClosure env patExpr
         matchs <- patternMatch MOne [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]
         case matchs of
           [match] -> do newEnv <- liftIO $ extendEnv env match
                         objRef <- liftIO $ newIORef (Closure newEnv body)
                         return objRef
           [] -> mcLoop tgtObjRef typObjRef rest
cEval1 (Closure env (LoopExpr loopVar indexVar rangeExpr loopExpr tailExpr)) = do
  rangeObjRef <- liftIO $ makeClosure env rangeExpr
  let loopObj = Loop loopVar indexVar rangeObjRef loopExpr tailExpr
  expandLoop env loopObj
cEval1 (Closure env (ApplyExpr opExpr argExpr)) = do
  op <- cEval1 (Closure env opExpr)
  case op of
    Value (IOFunc fn) -> do arg <- eval env argExpr
                            val <- fn (tupleToList arg)
                            return $ Value val
    Value (PrimitiveFunc fn) -> do arg <- eval env argExpr
                                   val <- liftThrows $ fn (tupleToList arg)
                                   return $ Value val
    Value (Func fArgs body cEnv) -> do frame <- liftIO (makeClosure env argExpr) >>= makeFrame fArgs
                                       newEnv <- liftIO $ extendEnv cEnv frame
                                       cEval1 (Closure newEnv body)
    _ -> throwError $ Default "not function"
cEval1 val = return val

cApply1 :: ObjectRef -> ObjectRef -> IOThrowsError Object
cApply1 fnObjRef argObjRef = do
  fnObj <- cRefEval1 fnObjRef
  case fnObj of
    Value (IOFunc fn) -> throwError $ Default "undefined ioFunc"
    Value (PrimitiveFunc fn) -> do arg <- cRefEval argObjRef
                                   val <- liftThrows $ fn (tupleToList arg)
                                   return $ Value val
    Value (Func fArgs body cEnv) -> do frame <- makeFrame fArgs argObjRef
                                       newEnv <- liftIO $ extendEnv cEnv frame
                                       cEval1 (Closure newEnv body)
    _ -> throwError $ Default "cApply1: not function"


expandLoop :: Env -> Object -> IOThrowsError Object
expandLoop env (Loop loopVar indexVar rangeObjRef loopExpr tailExpr) = do
  b <- isEmptyCollection rangeObjRef
  if b
    then cEval1 $ Closure env tailExpr
    else do (carObjRef,cdrObjRef) <- consDestruct rangeObjRef
            loopObjRef <- liftIO $ newIORef $ Loop loopVar indexVar cdrObjRef loopExpr tailExpr
            newEnv <- liftIO $ extendEnv env [((loopVar,[]),loopObjRef),((indexVar,[]),carObjRef)]
            cEval1 $ Closure newEnv loopExpr
expandLoop _ obj = throwError $ Default $ "expandLoop: cannot reach here: " ++ show obj
           
-- |Extend given environment by binding a series of values to a new environment for let.
extendLet :: Env -- ^ Environment 
          -> [(Args, EgisonExpr)] -- ^ Extensions to the environment
          -> IOThrowsError Env -- ^ Extended environment
extendLet env abindings = do
  bingingList <- liftM concat $ mapM (\(args, expr) -> do
                                         objRef <- liftIO $ makeClosure env expr
                                         helper args objRef)
                      abindings
  liftIO $ extendEnv env bingingList
 where helper (AVar name) objRef = return [((name, []), objRef)]
       helper (ATuple argss) objRef = do
         obj <- cRefEval1 objRef
         case obj of
           Intermidiate (ITuple innerRefs) -> do
             objRefs <- innerValRefsToObjRefList innerRefs
             liftM concat $ mapM (\(args,objRef3) -> helper args objRef3) $ zip argss objRefs
           Value (Tuple innerVals) -> do
             objRefs <- liftIO $ mapM (newIORef . Value) $ innerValsToList innerVals
             liftM concat $ mapM (\(args,objRef3) -> helper args objRef3) $ zip argss objRefs
           _ -> throwError $ Default "extendLet: not tuple"

makeFrame :: Args -> ObjectRef -> IOThrowsError [(Var, ObjectRef)]
makeFrame (AVar name) objRef = return $ [((name,[]), objRef)]
makeFrame (ATuple []) _ = return $ []
makeFrame (ATuple [fArg]) objRef = makeFrame fArg objRef
makeFrame (ATuple fArgs) objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ITuple innerRefs) -> do
      objRefs <- innerValRefsToObjRefList innerRefs
      frames <- mapM (\(fArg,objRef3) -> makeFrame fArg objRef3) $ zip fArgs objRefs
      return $ concat frames
    Value (Tuple innerVals) -> do
      let vals = innerValsToList innerVals
      objRefs <- liftIO $ valsToObjRefList vals
      frames <- mapM (\(fArg,objRef3) -> makeFrame fArg objRef3) $ zip fArgs objRefs
      return $ concat frames
    _ -> throwError $ Default "makeFrame: not tuple"

innerValRefsToObjRefList :: [InnerValRef] -> IOThrowsError [ObjectRef]
innerValRefsToObjRefList [] = return []
innerValRefsToObjRefList (innerRef:rest) = do
  restRet <- innerValRefsToObjRefList rest
  case innerRef of
    IElement objRef -> return $ objRef:restRet
    ISubCollection objRef -> do
      obj2 <- cRefEval1 objRef
      case obj2 of
        Intermidiate (ICollection innerRefs) -> do
          objRefs <- innerValRefsToObjRefList innerRefs
          return $ objRefs ++ restRet
        Value (Collection innerVals) -> do
          objRefs <- liftIO $ mapM newIORef $ map Value $ innerValsToList innerVals
          return $ objRefs ++ restRet
        _ -> throwError $ Default "innerValRefsToObjRefList: not collection"

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
      typ <- cRefEval1 typObjRef
      case typ of
        Value (Type tf) ->
          let mObjRef = Data.Map.lookup ("var-match",[]) tf in
            case mObjRef of
              Nothing -> throwError (Default "no method in type: var-match")
              Just fnObjRef ->
                do ret <- cApply1 fnObjRef tgtObjRef
                   case ret of
                     Intermidiate (ICollection innerObjRefs) -> do
                       objRefs <- innerValRefsToObjRefList innerObjRefs
                       patternMatch flag $ (map (\objRef -> (MState (((name,nums),objRef):frame)
                                                                    (map (\(MAtom (PClosure bf2 pat2) tgt2 typ2) ->
                                                                            (MAtom (PClosure (((name,nums),objRef):bf2) pat2) tgt2 typ2))
                                                                         atoms)))
                                                objRefs) ++ states
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Intermidiate (IInductiveData con patObjRefs) -> do
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type tf) ->
          let mObjRef = Data.Map.lookup ("inductive-match",[]) tf in
            case mObjRef of
              Nothing -> throwError (Default "no method in type: inductiver-match")
              Just fnObjRef ->
                do fnObj <- cRefEval1 fnObjRef
                   case fnObj of
                     Value (Destructor deconInfo) -> do
                       indRet <- inductiveMatch deconInfo con tgtObjRef
                       case indRet of
                         (nTypObjRef, nTgtsObjRef) -> do
                           inTypObjRefs <- tupleToObjRefList nTypObjRef
                           inTgtsRefs <- collectionToObjRefList nTgtsObjRef
                           inTgtObjRefss <- mapM tupleToObjRefList inTgtsRefs
                           patternMatch flag ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                                         (zip3 patObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                                   inTgtObjRefss) ++ states)
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Value (InductiveData con pats) -> do
      patObjRefs <- liftIO $ mapM (newIORef . Value) pats
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type tf) ->
          let mObjRef = Data.Map.lookup ("inductive-match",[]) tf in
            case mObjRef of
              Nothing -> throwError (Default "no method in type: inductiver-match")
              Just fnObjRef ->
                do fnObj <- cRefEval1 fnObjRef
                   case fnObj of
                     Value (Destructor deconInfo) -> do
                       indRet <- inductiveMatch deconInfo con tgtObjRef
                       case indRet of
                         (nTypObjRef, nTgtsObjRef) -> do
                           inTypObjRefs <- tupleToObjRefList nTypObjRef
                           inTgtsRefs <- collectionToObjRefList nTgtsObjRef
                           inTgtObjRefss <- mapM tupleToObjRefList inTgtsRefs
                           patternMatch flag $ (map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> MAtom (PClosure bf pat) inTgtObjRef inTypObjRef)
                                                                                          (zip3 patObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                                    inTgtObjRefss) ++ states
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
    Intermidiate (ITuple pats) -> do
      patObjRefs <- innerRefsToObjRefList pats
      tgtObjRefs <- tupleToObjRefList tgtObjRef
      typObjRefs <- tupleToObjRefList typObjRef
      patternMatch flag $ (MState frame ((map (\(pat,tgt,typ) -> MAtom (PClosure bf pat) tgt typ) (zip3 patObjRefs tgtObjRefs typObjRefs)) ++ atoms)):states
    Value (Tuple pats) -> do
      patObjRefs <- innerValsToObjRefList pats
      tgtObjRefs <- tupleToObjRefList tgtObjRef
      typObjRefs <- tupleToObjRefList typObjRef
      patternMatch flag $ (MState frame ((map (\(pat,tgt,typ) -> MAtom (PClosure bf pat) tgt typ) (zip3 patObjRefs tgtObjRefs typObjRefs)) ++ atoms)):states
    Value (PredPat predName patObjRefs) -> do
      typObj <- cRefEval1 typObjRef
      case typObj of
        Value (Type tf) ->
          let mObjRef = Data.Map.lookup (predName,[]) tf in
            case mObjRef of
              Nothing -> throwError $ Default $ "no method in type: " ++ predName
              Just fnObjRef -> do
                argsObjRef <- liftIO $ newIORef $ Intermidiate $ ITuple $ map IElement $ patObjRefs ++ [tgtObjRef]
                ret <- cApply1 fnObjRef argsObjRef
                case ret of
                  Value (Bool True) -> patternMatch flag ((MState frame atoms):states)
                  Value (Bool False) -> patternMatch flag states
                  _ -> throwError (Default "patternMatch: return value of pred-pattern is not boolean value")
        _ -> throwError $ Default "patternMatch: second argument of match expressions must be type"
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
        
inductiveMatch :: DestructInfo -> String -> ObjectRef -> IOThrowsError (ObjectRef,ObjectRef)
inductiveMatch [] _ _ = throwError (Default "inductiveMatch: not matched any clauses")
inductiveMatch ((con,_,[]):rest) pcon tgtObjRefRef =
  if (con == pcon)
    then throwError (Default "inductiveMatch: not matched any clauses")
    else inductiveMatch rest pcon tgtObjRefRef
inductiveMatch ((con,typObjRef,((env,ppat,expr):cls)):rest) pcon tgtObjRefRef =
  if (con == pcon)
    then do mPpmRet <- primitivePatternMatch ppat tgtObjRefRef
            case mPpmRet of
              Nothing -> inductiveMatch ((con,typObjRef,cls):rest) pcon tgtObjRefRef
              Just ppmRet -> do newEnv <- liftIO $ extendEnv env ppmRet
                                ret <- liftIO $ makeClosure newEnv expr
                                return (typObjRef,ret)
    else inductiveMatch rest pcon tgtObjRefRef


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
    _ -> return Nothing
primitivePatternMatch PEmptyPat objRef = do
  b <- isEmptyCollection objRef
  if b
    then return (Just [])
    else return Nothing
primitivePatternMatch (PConsPat carPat cdrPat) objRef = do
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
  b <- isEmptyCollection objRef
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
primitivePatternMatchList _ _ = throwError (Default "primitivePatternMatchList : number of patterns and targets are different")


isEmptyCollection :: ObjectRef -> IOThrowsError Bool
isEmptyCollection objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> isEmptyInnerRefs innerRefs
    Value (Collection innerVals) -> isEmptyInnerVals innerVals
    _ -> throwError $ Default $ "isEmptyCollection: not collection:" ++ show obj

isEmptyInnerRefs :: [InnerValRef] -> IOThrowsError Bool
isEmptyInnerRefs [] = return True
isEmptyInnerRefs ((IElement _):_) = return False
isEmptyInnerRefs ((ISubCollection objRef):rest) = do
  b <- isEmptyCollection objRef
  if b
    then isEmptyInnerRefs rest
    else return False

isEmptyInnerVals :: [InnerVal] -> IOThrowsError Bool
isEmptyInnerVals [] = return True
isEmptyInnerVals ((Element _):_) = return False
isEmptyInnerVals ((SubCollection val):rest) = do
  objRef <- liftIO $ newIORef $ Value val
  b <- isEmptyCollection objRef
  if b
    then isEmptyInnerVals rest
    else return False


consDestruct :: ObjectRef -> IOThrowsError (ObjectRef, ObjectRef)
consDestruct objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> consDestructInnerRefs innerRefs
    Value (Collection innerVals) -> consDestructInnerVals innerVals
    _ -> throwError $ Default "consDestruct: not collection"

consDestructInnerRefs :: [InnerValRef] -> IOThrowsError (ObjectRef, ObjectRef)
consDestructInnerRefs [] = throwError $ Default "consDestructInnerRefs: empty collection"
consDestructInnerRefs ((IElement objRef):rest) = do
  cdrObjRef <- liftIO $ newIORef $ Intermidiate $ ICollection rest
  return (objRef, cdrObjRef)
consDestructInnerRefs ((ISubCollection objRef):rest) = do
  b <- isEmptyCollection objRef
  if b
    then consDestructInnerRefs rest
    else do (carObjRef, cdrObjRef) <- consDestruct objRef
            cdrObjRef2 <- liftIO $ newIORef $ Intermidiate $ ICollection $ (ISubCollection cdrObjRef):rest
            return (carObjRef, cdrObjRef2)
    
consDestructInnerVals :: [InnerVal] -> IOThrowsError (ObjectRef, ObjectRef)
consDestructInnerVals [] = throwError $ Default "consDestructInnerVals: empty collection"
consDestructInnerVals ((Element val):rest) = do
  carObjRef <- liftIO $ newIORef $ Value val
  cdrObjRef <- liftIO $ newIORef $ Value $ Collection rest
  return (carObjRef, cdrObjRef)
consDestructInnerVals ((SubCollection val):rest) = do
  objRef <- liftIO $ newIORef $ Value val
  b <- isEmptyCollection objRef
  if b
    then consDestructInnerVals rest
    else do (carObjRef, cdrObjRef) <- consDestruct objRef
            cdrObj <- liftIO $ readIORef cdrObjRef
            case cdrObj of
              Value (Collection innerVals) -> do
                cdrObjRef2 <- liftIO $ newIORef $ Value $ Collection $ (SubCollection (Collection innerVals)):rest
                return (carObjRef, cdrObjRef2)
              _ -> throwError $ Default "consDestructInnerVals: cannot reach here!"


snocDestruct :: ObjectRef -> IOThrowsError (ObjectRef, ObjectRef)
snocDestruct objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> snocDestructInnerRefs innerRefs
    Value (Collection innerVals) -> snocDestructInnerVals innerVals
    _ -> throwError $ Default "snocDestruct: not collection"

snocDestructInnerRefs :: [InnerValRef] -> IOThrowsError (ObjectRef, ObjectRef)
snocDestructInnerRefs [] = throwError $ Default "snocDestructInnerRefs: empty collection"
snocDestructInnerRefs ((IElement objRef):rest) = do
  cdrObjRef <- liftIO $ newIORef $ Intermidiate $ ICollection rest
  return (objRef, cdrObjRef)
snocDestructInnerRefs ((ISubCollection objRef):rest) = do
  b <- isEmptyCollection objRef
  if b
    then snocDestructInnerRefs rest
    else do (carObjRef, cdrObjRef) <- snocDestruct objRef
            cdrObjRef2 <- liftIO $ newIORef $ Intermidiate $ ICollection $ (ISubCollection cdrObjRef):rest
            return (carObjRef, cdrObjRef2)
    
snocDestructInnerVals :: [InnerVal] -> IOThrowsError (ObjectRef, ObjectRef)
snocDestructInnerVals [] = throwError $ Default "snocDestructInnerVals: empty collection"
snocDestructInnerVals ((Element val):rest) = do
  carObjRef <- liftIO $ newIORef $ Value val
  cdrObjRef <- liftIO $ newIORef $ Value $ Collection rest
  return (carObjRef, cdrObjRef)
snocDestructInnerVals ((SubCollection val):rest) = do
  objRef <- liftIO $ newIORef $ Value val
  b <- isEmptyCollection objRef
  if b
    then snocDestructInnerVals rest
    else do (carObjRef, cdrObjRef) <- snocDestruct objRef
            cdrObj <- liftIO $ readIORef cdrObjRef
            case cdrObj of
              Value (Collection innerVals) -> do
                cdrObjRef2 <- liftIO $ newIORef $ Value $ Collection $ (SubCollection (Collection innerVals)):rest
                return (carObjRef, cdrObjRef2)
              _ -> throwError $ Default "snocDestructInnerVals: cannot reach here!"


collectionToObjRefList :: ObjectRef -> IOThrowsError [ObjectRef]
collectionToObjRefList objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ICollection innerRefs) -> innerRefsToObjRefList innerRefs
    Value (Collection innerVals) -> innerValsToObjRefList innerVals
    _ -> throwError $ Default "collectionToObjRefList: not collection"

tupleToObjRefList :: ObjectRef -> IOThrowsError [ObjectRef]
tupleToObjRefList objRef = do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (ITuple innerRefs) -> innerRefsToObjRefList innerRefs
    Value (Tuple innerVals) -> innerValsToObjRefList innerVals
    _ -> throwError $ Default "tupleToObjRefList: not tuple"

innerRefsToObjRefList :: [InnerValRef] -> IOThrowsError [ObjectRef]
innerRefsToObjRefList [] = return []
innerRefsToObjRefList ((IElement objRef):rest) = do
  retRest <- innerRefsToObjRefList rest
  return $ objRef:retRest
innerRefsToObjRefList ((ISubCollection objRef):rest) = do
  retObj <- collectionToObjRefList objRef
  retRest <- innerRefsToObjRefList rest
  return $ retObj ++ retRest

innerValsToObjRefList :: [InnerVal] -> IOThrowsError [ObjectRef]
innerValsToObjRefList [] = return []
innerValsToObjRefList ((Element val):rest) = do
  valRef <- liftIO $ newIORef $ Value val
  retRest <- innerValsToObjRefList rest
  return $ valRef:retRest
innerValsToObjRefList ((SubCollection val):rest) = do
  valRef <- liftIO $ newIORef $ Value val
  retVal <- collectionToObjRefList valRef
  retRest <- innerValsToObjRefList rest
  return $ retVal ++ retRest

  
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
constants = [("pi", Float 3.14)
             ]
         
{- I/O primitives
Primitive functions that execute within the IO monad -}
ioPrimitives :: [(String, [EgisonVal] -> IOThrowsError EgisonVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
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

              ("eq?", numBoolBinop (==)),
              ("lt?", numBoolBinop (<)),
              ("lte?", numBoolBinop (<=)),
              ("gt?", numBoolBinop (>)),
              ("gte?", numBoolBinop (>=)),

              ("eq-f?", floatBoolBinop (==)),
              ("lt-f?", floatBoolBinop (<)),
              ("lte-f?", floatBoolBinop (<=)),
              ("gt-f?", floatBoolBinop (>)),
              ("gte-f?", floatBoolBinop (>=)),

              ("&&", boolBinop (&&)),
              ("||", boolBinop (||))]

