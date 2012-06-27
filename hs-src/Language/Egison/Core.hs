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
  let argv = TupleExpr $ map (ElementExpr . StringExpr) args
  eval env $ ApplyExpr (VarExpr "main" []) argv

-- |Evaluate egison top expression that has already been loaded into haskell
evalTopExpr :: Env -> TopExpr -> IOThrowsError String
evalTopExpr env (Test expr) = liftM show $ eval env expr
evalTopExpr env (Define name expr) = do clr <- liftIO $ makeClosure env expr
                                        defineVar env (name, []) clr
                                        return name
evalTopExpr env (Execute args) = do evalMain env args
                                    return ""
evalTopExpr _ (LoadFile filename) = throwError $ Default "undefined loadfile"
evalTopExpr _ (Load libname) = throwError $ Default "undefined load"

-- |Evaluate egison expression that has already been loaded into haskell
eval :: Env -> EgisonExpr -> IOThrowsError EgisonVal
eval env expr = do
  obj <- cEval1 (Closure env expr)
--  liftIO $ putStrLn $ show obj -- Debug
  case obj of
    Value val -> return val
    Intermidiate iVal -> iEval iVal
    _ -> throwError $ Default "eval: cannot reach here!"

--eval1 :: Env -> EgisonExpr -> IOThrowsError ObjectRef
--eval1 env expr = do
--  objRef <- liftIO $ makeClosure env expr
--  cRefEval1 objRef
    
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
iEval _ = throwError $ Default "undefined iEval"

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
  cRefEval1 objRef
cEval1 (Closure env (InductiveDataExpr cons argExprs)) = do
  args <- liftIO $ mapM (makeClosure env) argExprs
  return $ Intermidiate $ IInductiveData cons args
cEval1 (Closure env (TupleExpr innerExprs)) = do
  innerRefs <- liftIO $ mapM (makeInnerValRef env) innerExprs
  return $ Intermidiate $ ITuple innerRefs
cEval1 (Closure env (CollectionExpr innerExprs)) = do
  innerRefs <- liftIO $ mapM (makeInnerValRef env) innerExprs
  return $ Intermidiate $ ICollection innerRefs
cEval1 (Closure env (PatVarExpr name numExprs)) = do
  numVals <- mapM (eval env) numExprs
  nums <- mapM (\nVal -> case nVal of
                           Number num -> return num
                           _ -> throwError  $ Default "cEval1: cannot reach here!")
               numVals
  return $ Value $ PatVar name nums
cEval1 (Closure env (FuncExpr args body)) = do
  return $ Value $ Func args body env
cEval1 (Closure env (LetExpr bindings body)) = do
  newEnv <- extendLet env bindings
  cEval1 (Closure newEnv body)
cEval1 (Closure env (LetRecExpr bindings body)) = do
  newEnv <- liftIO $ extendLetRec env bindings
  cEval1 (Closure newEnv body)
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
  matchs <- patternMatchAll [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]
  liftIO $ putStrLn ("egiTest2: " ++ show (length matchs))  -- debug
  rets <- mapM (\match -> do liftIO $ putStrLn $ showFrameList match -- debug
                             newEnv <- liftIO $ extendEnv env match
                             objRef <- liftIO $ newIORef (Closure newEnv body)
                             return objRef)
               matchs
  return $ Intermidiate $ ICollection $ map IElement rets
cEval1 (Closure env (MatchExpr tgtExpr typExpr mcs)) = do
  tgtObjRef <- liftIO $ makeClosure env tgtExpr
  typObjRef <- liftIO $ makeClosure env typExpr
  retRef <- mcLoop tgtObjRef typObjRef mcs
  liftIO $ readIORef retRef
 where mcLoop _ _ [] = throwError $ Default "end of match clauses"
       mcLoop tgtObjRef typObjRef ((patExpr, body):rest) = do
         patObjRef <- liftIO $ makeClosure env patExpr
         matchs <- patternMatch [(MState [] [(MAtom (PClosure [] patObjRef) tgtObjRef typObjRef)])]
         case matchs of
           [match] -> do newEnv <- liftIO $ extendEnv env match
                         objRef <- liftIO $ newIORef (Closure newEnv body)
                         return objRef
           [] -> mcLoop tgtObjRef typObjRef rest
cEval1 (Closure env (ApplyExpr opExpr argExpr)) = do
  op <- cEval1 (Closure env opExpr)
  case op of
    Value (IOFunc fn) -> throwError $ Default "undefined ioFunc"
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

patternMatchAll :: [MState] -> IOThrowsError [FrameList]
patternMatchAll [] = return []
patternMatchAll ((MState frame []):rest) = do
  frames <- patternMatchAll rest
  return (frame:frames)
patternMatchAll ((MState frame ((MAtom (PClosure bf patObjRef) tgtObjRef typObjRef)
                                :atoms))
                 :states) = do
  patObj <- liftIO $ readIORef patObjRef
  case patObj of
    Closure env expr ->
      do newEnv <- liftIO $ extendEnv env bf
         patObj2 <- cEval1 $ Closure newEnv expr
         patObjRef2 <- liftIO $ newIORef patObj2
         patternMatchAll ((MState frame ((MAtom (PClosure [] patObjRef2) tgtObjRef typObjRef)
                                         :atoms))
                          :states)
    Value WildCard -> patternMatchAll ((MState frame atoms):states)
    Value (PatVar name nums) -> do
--      liftIO $ putStrLn $ "egiTest: " ++ name -- debug
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
                       patternMatchAll $ (map (\objRef -> (MState (((name,nums),objRef):frame)
                                                                  (map (\(MAtom (PClosure bf2 pat2) tgt2 typ2) ->
                                                                          (MAtom (PClosure (((name,nums),objRef):bf2) pat2) tgt2 typ2))
                                                                       atoms)))
                                                objRefs) ++ states
        _ -> throwError $ Default "patternMatchAll: second argument of match expressions must be type"
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
                           patternMatchAll ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                                       (zip3 patObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                                 inTgtObjRefss) ++ states)
        _ -> throwError $ Default "patternMatchAll: second argument of match expressions must be type"
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
                           patternMatchAll ((map (\inTgtObjRefs -> (MState frame ((map (\(pat,inTgtObjRef,inTypObjRef) -> (MAtom (PClosure bf pat) inTgtObjRef inTypObjRef))
                                                                                       (zip3 patObjRefs inTgtObjRefs inTypObjRefs)) ++ atoms)))
                                                 inTgtObjRefss) ++ states)
        _ -> throwError $ Default "patternMatchAll: second argument of match expressions must be type"
    _ -> throwError $ Default "pattern must not be value"
        
patternMatch :: [MState] -> IOThrowsError [FrameList]
patternMatch _ = throwError $ Default "undefined patternMatch"

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
primitivePatternMatch PWildCard _ = return (Just [])
primitivePatternMatch (PPatVar name) objRef = return (Just [((name,[]), objRef)])
primitivePatternMatch (PInductivePat pCons pPats) objRef =  do
  obj <- cRefEval1 objRef
  case obj of
    Intermidiate (IInductiveData cons objRefs) ->
      if pCons == cons
        then primitivePatternMatchList pPats objRefs
        else return Nothing
    _ -> do throwError $ Default $ "primitivePatternMatch : not inductive value to primitive inductive pattern"
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
isEmptyCollection _ = throwError $ Default "undefined isEmptyCollection"

consDestruct :: ObjectRef -> IOThrowsError (ObjectRef, ObjectRef)
consDestruct _ = throwError $ Default "undefined consDestruct"

snocDestruct :: ObjectRef -> IOThrowsError (ObjectRef, ObjectRef)
snocDestruct _ = throwError $ Default "undefined snocDestruct"


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

iTupleToObjRefList :: IntermidiateVal -> IOThrowsError [ObjectRef]
iTupleToObjRefList (ITuple innerRefs) = innerRefsToObjRefList innerRefs
iTupleToObjRefList _ = throwError $ Default "iTupleToObjRefList: not tuple"

iCollectionToObjRefList :: IntermidiateVal -> IOThrowsError [ObjectRef]
iCollectionToObjRefList (ICollection innerRefs) = innerRefsToObjRefList innerRefs
iCollectionToObjRefList _ = throwError $ Default "iCollectionToObjRefList: not collection"

innerRefsToObjRefList :: [InnerValRef] -> IOThrowsError [ObjectRef]
innerRefsToObjRefList [] = return []
innerRefsToObjRefList ((IElement objRef):rest) = do
  retRest <- innerRefsToObjRefList rest
  return $ objRef:retRest
innerRefsToObjRefList ((ISubCollection objRef):rest) = do
  retObj <- collectionToObjRefList objRef
  retRest <- innerRefsToObjRefList rest
  return $ retObj ++ retRest

tupleValToObjRefList :: EgisonVal -> IOThrowsError [ObjectRef]
tupleValToObjRefList (Tuple innerVals) = innerValsToObjRefList innerVals
tupleValToObjRefList _ = throwError $ Default "tupleToObjRefList: not tuple"
    
collectionValToObjRefList :: EgisonVal -> IOThrowsError [ObjectRef]
collectionValToObjRefList (Collection innerVals) = innerValsToObjRefList innerVals
colectionValToObjRefList _ = throwError $ Default "collectionToObjRefList: not tuple"

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
  iOFuncs <- mapM (domakeFunc IOFunc) ioPrimitives
  primitiveFuncs <- mapM (domakeFunc PrimitiveFunc) primitives
  extendEnv initEnv (iOFuncs ++ primitiveFuncs)
 where domakeFunc constructor (name, func) = do
         objRef <- newIORef $ Value $ constructor func
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

