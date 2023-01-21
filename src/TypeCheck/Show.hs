--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Show
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used in the middle of type checking to print
-- sections of the program (in modified type checking form) out
-- to the user. It is most often used for printing errors that
-- occur during type checking.
--------------------------------------------------------------------

module TypeCheck.Show where

import           Semantics
import           Semantics.Prgm
import           TypeCheck.Common

showM :: FEnv -> VarMeta -> TypeCheckResult ShowMeta
showM env m = do
  stype <- descriptor env m
  return $ mapMetaDat (ShowMeta stype) m

showExpr :: FEnv -> VExpr -> TypeCheckResult SExpr
showExpr env (CExpr m c) = do
  m' <- showM env m
  return $ CExpr m' c
showExpr env (Value m name) = do
  m' <- showM env m
  return $ Value m' name
showExpr env (Arg m name) = do
  m' <- showM env m
  return $ Arg m' name
showExpr env (HoleExpr m hole) = do
  m' <- showM env m
  return $ HoleExpr m' hole
showExpr env (AliasExpr base alias) = do
  base' <- showExpr env base
  alias' <- showExpr env alias
  return $ AliasExpr base' alias'
showExpr env (TupleApply m (bm, base) arg) = do
  m' <- showM env m
  bm' <- showM env bm
  base' <- showExpr env base
  arg' <- case arg of
    TupleArgI argM argName -> do
      argM' <- showM env argM
      return $ TupleArgI argM' argName
    TupleArgO argM argVal -> do
      argM' <- showM env argM
      argVal' <- showExpr env argVal
      return $ TupleArgO argM' argVal'
    TupleArgIO argM argName argVal -> do
      argM' <- showM env argM
      argVal' <- showExpr env argVal
      return $ TupleArgIO argM' argName argVal'
  return $ TupleApply m' (bm', base') arg'
showExpr env (VarApply m base varName varVal) = do
  m' <- showM env m
  base' <- showExpr env base
  varVal' <- showM env varVal
  return $ VarApply m' base' varName varVal'

showGuard :: FEnv -> VGuard -> TypeCheckResult SGuard
showGuard env (IfGuard e) = do
  e' <- showExpr env e
  return $ IfGuard e'
showGuard _ ElseGuard = return ElseGuard
showGuard _ NoGuard = return NoGuard

showArrow :: FEnv -> VArrow -> TypeCheckResult SArrow
showArrow env (Arrow m guard maybeExpr) = do
  m' <- showM env m
  guard' <- showGuard env guard
  expr' <- mapM (showExpr env) maybeExpr
  return $ Arrow m' guard' expr'

showObjArg :: FEnv -> VObjArg -> TypeCheckResult SObjArg
showObjArg env (m, maybeObj) = do
  m' <- showM env m
  maybeObj' <- mapM (showObjRec env) maybeObj
  return (m', maybeObj')

showObjRec :: FEnv -> VObject -> TypeCheckResult (Object Expr ShowMetaDat)
showObjRec env obj@Object{deprecatedObjArgs, objDupExpr} = do
  m' <- showM env (objM obj)
  vars' <- mapM (showM env) $ objAppliedVars obj
  args' <- mapM (showObjArg env) deprecatedObjArgs
  objDupExpr' <- showExpr env objDupExpr
  return $ obj{deprecatedObjM=m', deprecatedObjVars=vars', deprecatedObjArgs=args', objDupExpr=objDupExpr'}

showObj :: FEnv -> VObject -> TypeCheckResult SObject
showObj env obj = do
  obj' <- showObjRec env obj
  return $ asExprObject obj'

showObjArrow :: FEnv -> VObjectMapItem -> TypeCheckResult SObjectMapItem
showObjArrow env (obj, annots, arrow) = do
  obj' <- showObj env obj
  annots' <- mapM (showExpr env) annots
  arrow' <- mapM (showArrow env) arrow
  return (obj', annots', arrow')

showConHelper :: FEnv -> (Scheme -> Scheme -> SConstraint) -> VarMeta -> VarMeta -> SConstraint
showConHelper env f p1 p2 = f (descriptor env p1) (descriptor env p2)

showCon :: FEnv -> Constraint -> SConstraint
showCon env (EqualsKnown p t) = SEqualsKnown (descriptor env p) t
showCon env (EqPoints p1 p2) = showConHelper env SEqPoints p1 p2
showCon env (BoundedByKnown p t) = SBoundedByKnown (descriptor env p) t
showCon env (BoundedByObjs p) = SBoundedByObjs (descriptor env p)
showCon env (ArrowTo p1 p2) = showConHelper env SArrowTo p1 p2
showCon env (PropEq (p1, name) p2) = showConHelper env (\s1 s2 -> SPropEq (s1, name) s2) p1 p2
showCon env (VarEq (p1, name) p2) = showConHelper env (\s1 s2 -> SVarEq (s1, name) s2) p1 p2
showCon env (AddArg (p1, argName) p2) = showConHelper env (\s1 s2 -> SAddArg (s1, argName) s2) p1 p2
showCon env (AddInferArg p1 p2) = showConHelper env SAddInferArg p1 p2
showCon env (PowersetTo p1 p2) = showConHelper env SPowersetTo p1 p2
showCon env (UnionOf p1 p2s) = SUnionOf (descriptor env p1) (map (descriptor env) p2s)

showPrgm :: FEnv -> VPrgm -> TypeCheckResult SPrgm
showPrgm env (objMap, classGraph, annots) = do
  objMap' <- mapM (showObjArrow env) objMap
  annots' <- mapM (showExpr env) annots
  return (objMap', classGraph, annots')

showConstraints :: FEnv -> [Constraint] -> [SConstraint]
showConstraints env = map (showCon env)

showTraceConstrainEpoch :: FEnv -> TraceConstrainEpoch -> [(SConstraint, [(Pnt, Scheme)])]
showTraceConstrainEpoch env = map mapConstraint . filter (not . null . snd)
  where
    mapConstraint (con, pnts) = (showCon env con, pnts)
