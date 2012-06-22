module Language.Egison.Variables where
import Language.Egison.Types
import Control.Monad.Error
import Data.IORef
import qualified Data.Map

-- |Show the contents of an environment
printEnv :: Env         -- ^Environment
         -> IO String   -- ^Contents of the env as a string
printEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  l <- mapM showBind $ Data.Map.toList binds 
  return $ unlines l
 where 
  showBind ((name, nums), objRef) = do
    obj <- liftIO $ readIORef objRef
    return $ name ++ unwordsNums nums ++ ": " ++ show obj

-- |Create a copy of an environment
copyEnv :: Env      -- ^ Source environment
        -> IO Env   -- ^ A copy of the source environment
copyEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  bindingListT <- mapM addBinding $ Data.Map.toList binds
  bindingList <- newIORef $ Data.Map.fromList bindingListT
  return $ Environment (parentEnv env) bindingList
 where addBinding (var, val) = do --ref <- newIORef $ liftIO $ readIORef val
                                  x <- liftIO $ readIORef val
                                  ref <- newIORef x
                                  return (var, ref)

-- |Extend given environment by binding a series of values to a new environment.
extendEnv :: Env -- ^ Environment 
          -> [(Var, ObjectRef)] -- ^ Extensions to the environment
          -> IO Env -- ^ Extended environment
extendEnv env abindings = do bindinglist <- newIORef $ Data.Map.fromList abindings
                             return $ Environment (Just env) bindinglist

-- |Extend given environment by binding a series of values to a new environment.
extendEnvRec :: Env -- ^ Environment 
             -> [(Var, EgisonExpr)] -- ^ Extensions to the environment
             -> IO Env -- ^ Extended environment
extendEnvRec env abindings = do bindinglistT <- (mapM addDummyBinding abindings) -- >>= newIORef
                                bindinglist <- newIORef $ Data.Map.fromList bindinglistT
                                let newEnv = Environment (Just env) bindinglist
                                mapM (replaceWithNewEnv newEnv) bindinglistT
                                return newEnv
 where addDummyBinding (var, expr) = do dummy <- nullEnv
                                        objRef <- makeClosure dummy expr
                                        return (var, objRef)
       replaceWithNewEnv newEnv (_, objRef) = do obj <- readIORef objRef
                                                 case obj of
                                                   Closure _ cExpr -> writeIORef objRef (Closure newEnv cExpr)
                                               
-- |Recursively search environments to find one that contains the given variable.
findEnv 
    :: Env      -- ^Environment to begin the search; 
                --  parent env's will be searched as well.
    -> Var      -- ^Variable
    -> IO (Maybe Env) -- ^Environment, or Nothing if there was no match.
findEnv envRef var = do
  found <- liftIO $ isBound envRef var
  if found
     then return (Just envRef)
     else case parentEnv envRef of
               (Just par) -> findEnv par var
               Nothing -> return Nothing

-- |Determine if a variable is bound
isBound 
    :: Env      -- ^ Environment
    -> Var   -- ^ Variable
    -> IO Bool  -- ^ True if the variable is bound
isBound envRef var = 
    (readIORef $ bindings envRef) >>= return . Data.Map.member var

-- |Determine if a variable is bound
--  or a parent of the given environment.
isRecBound 
    :: Env      -- ^ Environment
    -> Var   -- ^ Variable
    -> IO Bool  -- ^ True if the variable is bound
isRecBound envRef var = do
  env <- findEnv envRef var
  case env of
    (Just e) -> isBound e var
    Nothing -> return False

-- |Retrieve the value of a variable defined
getVar :: Env     -- ^ Environment
       -> Var  -- ^ Variable
       -> IOThrowsError ObjectRef -- ^ Contents of the variable
getVar envRef
       var = do binds <- liftIO $ readIORef $ bindings envRef
                case Data.Map.lookup var binds of
                  (Just a) -> return a
                  Nothing -> case parentEnv envRef of
                               (Just par) -> getVar par var
                               Nothing -> (throwError $ UnboundVar "Getting an unbound variable" (showVar var))

-- |Bind a variable
defineVar
    :: Env      -- ^ Environment 
    -> Var   -- ^ Variable
    -> ObjectRef  -- ^ Value
    -> IOThrowsError ()   -- ^ Result
defineVar envRef
          var objRef = do
  liftIO $ do
    env <- readIORef $ bindings envRef
    writeIORef (bindings envRef) (Data.Map.insert var objRef env)
    return ()
