module Language.Egison.Types where
import Language.Egison.Types
import Control.Monad.Error

type MacroFrame = Data.Map.Map Var EgisonExpr

getExpr :: MacroFrame -> Var -> IOThrowsError EgisonExpr
  case Data.Map.lookup var frame of
    (Just a) -> return a
    Nothing -> throwError $ UnboundVar "Getting an unbound variable" (showVar var)

expandMacro :: MacroFrame -> EgisonExpr -> IOThrowsError EgisonExpr
expandMacro frame (InductiveDataExpr cons exprs) = do
  newExprs <- mapM (expandMacro frame) exprs
  return $ InductiveDataExpr cons newExprs
expandMacro frame (TupleExpr innerExprs) = do
  newInnerExprs <- mapM (expandMacroInnerExpr frame) innerExprs
  return $ TupleExpr newInnerExprs
expandMacro frame (CollectionExpr innerExprs) = do
  newInnerExprs <- mapM (expandMacroInnerExpr frame) innerExprs
  return $ CollectionExpr newInnerExprs
expandMacro frame (PredPatExpr predName argExprs) = do
  newExprs <- mapM (expandMacro frame) exprs
  return $ PredPatExpr predName newExprs
expandMacro frame (CutPatExpr patExpr) = do
  newPatExpr <- expandMacro frame patExpr
  return $ CutPatExpr newPatExpr
expandMacro frame (NotPatExpr patExpr) = do
  newPatExpr <- expandMacro frame patExpr
  return $ CutPatExpr newPatExpr
expandMacro frame (OrPatExpr patExprs) = do
  newPatExprs <- mapM (expandMacro frame) patExprs
  return $ OrPatExpr newPatExprs
expandMacro frame (AndPatExpr patExprs) = do
  newPatExprs <- mapM (expandMacro frame) patExprs
  return $ AndPatExpr newPatExprs
expandMacro frame (FuncExpr args body) = undefined
  newBody <- expandMacro frame body
  return $ FuncExpr args newBody
expandMacro frame (IfExpr condExpr expr1 expr2) = do
  newCondExpr <- expandMacro frame condExpr
  newExpr1 <- expandMacro frame expr1
  newExpr2 <- expandMacro frame expr2
  return $ IfExpr newCondExpr newExpr1 newExpr2
expandMacro frame (LoopExpr loopVar indexVar rangeExpr loopExpr tailExpr) = do
  newRangeExpr <- expandMacro frame rangeExpr
  newLoopExpr <- expandMacro frame loopExpr
  newTailExpr <- expandMacro frame tailExpr
  return $ LoopExpr loopVar indexVar newRangeExpr newLoopExpr newTailExpr
--expandMacro frame (LetExpr bindings body) = undefined
--expandMacro frame (LetRecExpr bindings body) = undefined
expandMacro frame (MatchExpr tgtExpr typExpr mcs) = do
  newTgtExpr <- expandMacro frame tgtExpr
  newTypExpr <- expandMacro frame typExpr
  newMcs <- mapM (expandMacroMatchClause frame) mcs
  return $ MatchExpr newTgtExpr newTypExpr newMcs
expandMacro frame (MatchAllExpr tgtExpr typExpr mc) = do
  newTgtExpr <- expandMacro frame tgtExpr
  newTypExpr <- expandMacro frame typExpr
  newMc <- expandMacroMatchClause frame mc
  return $ MatchExpr newTgtExpr newTypExpr newMc
expandMacro _ expr = return expr

expandMacroMatchClause :: MacroFrame -> MatchClause -> IOThrowsError MatchClause
expandMacroMatchClause frame (MatchClause patExpr body) = do
  newPatExpr <- expandMacro frame patExpr
  newBody <- expandMacro frame body
  return $ MatchClause newPatExpr newBody

expandMacroInnerExpr :: MacroFrame -> InnerExpr -> IOThrowsError InnerExpr
expandMacroInnerExpr frame (ElementExpr expr) = do
  newExpr <- expandMacro frame expr
  return $ ElementExpr newExpr
expandMacroInnerExpr frame (SubCollectionExpr expr) = do
  newExpr <- expandMacro frame expr
  return $ SubCollectionExpr newExpr
