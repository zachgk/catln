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

import           Control.Monad    (forM)
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
  arg' <- showObjArr env arg
  return $ TupleApply m' (bm', base') arg'
showExpr env (VarApply m base varName varVal) = do
  m' <- showM env m
  base' <- showExpr env base
  varVal' <- showM env varVal
  return $ VarApply m' base' varName varVal'

showGuardExpr :: FEnv -> VGuardExpr -> TypeCheckResult (GuardExpr Expr ShowMetaDat)
showGuardExpr env (GuardExpr e g) = do
  e' <- showExpr env e
  g' <- mapM (showExpr env) g
  return $ GuardExpr e' g'

showObjArr :: FEnv -> VObjArr -> TypeCheckResult SObjectMapItem
showObjArr env oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (showGuardExpr env) oaObj
  oaAnnots' <- mapM (showExpr env) oaAnnots
  oaArr' <- forM oaArr $ \(e, m) -> do
    e' <- mapM (showGuardExpr env) e
    m' <- showM env m
    return (e', m')
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

showArrow :: FEnv -> VArrow -> TypeCheckResult SArrow
showArrow env (Arrow m guard maybeExpr) = do
  m' <- showM env m
  guard' <- mapM (showExpr env) guard
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

showObjArrow :: FEnv -> VObjectMapItem -> TypeCheckResult SObjectMapItem
showObjArrow env (obj, annots, arrow) = do
  obj' <- showObjRec env obj
  annots' <- mapM (showExpr env) annots
  arrow' <- mapM (showArrow env) arrow
  return $ asExprObjectMapItem (obj', annots', arrow')

showConHelper :: FEnv -> (Scheme -> Scheme -> SConstraint) -> VarMeta -> VarMeta -> SConstraint
showConHelper env f p1 p2 = f (descriptor env p1) (descriptor env p2)

showCon :: FEnv -> Constraint -> SConstraint
showCon env (EqualsKnown _ p t) = SEqualsKnown (descriptor env p) t
showCon env (EqPoints _ p1 p2) = showConHelper env SEqPoints p1 p2
showCon env (BoundedByKnown _ p t) = SBoundedByKnown (descriptor env p) t
showCon env (BoundedByObjs _ p) = SBoundedByObjs (descriptor env p)
showCon env (ArrowTo _ p1 p2) = showConHelper env SArrowTo p1 p2
showCon env (PropEq _ (p1, name) p2) = showConHelper env (\s1 s2 -> SPropEq (s1, name) s2) p1 p2
showCon env (AddArg _ (p1, argName) p2) = showConHelper env (\s1 s2 -> SAddArg (s1, argName) s2) p1 p2
showCon env (AddInferArg _ p1 p2) = showConHelper env SAddInferArg p1 p2
showCon env (PowersetTo _ p1 p2) = showConHelper env SPowersetTo p1 p2
showCon env (UnionOf _ p1 p2s) = SUnionOf (descriptor env p1) (map (descriptor env) p2s)

showPrgm :: FEnv -> VPrgm -> TypeCheckResult SPrgm
showPrgm env (objMap, classGraph, annots) = do
  objMap' <- mapM (showObjArr env . asExprObjectMapItem) objMap
  annots' <- mapM (showExpr env) annots
  return (objMap', classGraph, annots')

showConstraints :: FEnv -> [Constraint] -> [SConstraint]
showConstraints env = map (showCon env)

showTraceConstrainEpoch :: FEnv -> TraceConstrainEpoch -> [(SConstraint, [(Pnt, Scheme)])]
showTraceConstrainEpoch env = map mapConstraint . filter (not . null . snd)
  where
    mapConstraint (con, pnts) = (showCon env con, pnts)

matchingConstraintHelper :: FEnv -> VarMeta -> VarMeta -> VarMeta -> Bool
matchingConstraintHelper env p p2 p3 = equivalent env p p2 || equivalent env p p3

matchingConstraint :: FEnv -> VarMeta -> Constraint -> Bool
matchingConstraint env p con = any (equivalent env p) $ constraintMetas con

showMatchingConstraints :: FEnv -> VarMeta -> [SConstraint]
showMatchingConstraints env@FEnv{feCons} matchVar = map (showCon env) $ filter (matchingConstraint env matchVar) feCons

mkTracedTypeCheckError :: FEnv -> VarMeta -> CodeRange -> String -> TypeCheckError
mkTracedTypeCheckError env m = TracedTypeCheckError m (showMatchingConstraints env m)

mkConstraintTypeCheckError :: FEnv -> Constraint -> [TypeCheckError] -> TypeCheckError
mkConstraintTypeCheckError env c = ConstraintTypeCheckError c (showCon env c)
