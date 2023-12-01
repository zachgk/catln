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

showObjArr :: FEnv -> VObjArr -> TypeCheckResult SObjArr
showObjArr env oa@ObjArr{oaObj, oaAnnots, oaArr=(arrE, arrM)} = do
  oaObj' <- mapM (showGuardExpr env) oaObj
  oaAnnots' <- mapM (showExpr env) oaAnnots
  arrE' <- mapM (showGuardExpr env) arrE
  arrM' <- showM env arrM
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=(arrE', arrM')}

showConHelper :: FEnv -> (Scheme -> Scheme -> SConstraint) -> VarMeta -> VarMeta -> SConstraint
showConHelper env f p1 p2 = f (descriptor env p1) (descriptor env p2)

showCon :: FEnv -> VConstraint -> SConstraint
showCon env (EqualsKnown i vaenv p t) = EqualsKnown i (descriptorVaenv env vaenv) (descriptor env p) t
showCon env (EqPoints i vaenv p1 p2) = showConHelper env (EqPoints i (descriptorVaenv env vaenv)) p1 p2
showCon env (BoundedByKnown i vaenv p t) = BoundedByKnown i (descriptorVaenv env vaenv)(descriptor env p) t
showCon env (BoundedByObjs i vaenv p) = BoundedByObjs i (descriptorVaenv env vaenv)(descriptor env p)
showCon env (ArrowTo i vaenv p1 p2) = showConHelper env (ArrowTo i (descriptorVaenv env vaenv)) p1 p2
showCon env (PropEq i vaenv (p1, name) p2) = showConHelper env (\s1 s2 -> PropEq i (descriptorVaenv env vaenv)(s1, name) s2) p1 p2
showCon env (AddArg i vaenv (p1, argName) p2) = showConHelper env (\s1 s2 -> AddArg i (descriptorVaenv env vaenv)(s1, argName) s2) p1 p2
showCon env (AddInferArg i vaenv p1 p2) = showConHelper env (AddInferArg i (descriptorVaenv env vaenv)) p1 p2
showCon env (PowersetTo i vaenv p1 p2) = showConHelper env (PowersetTo i (descriptorVaenv env vaenv)) p1 p2
showCon env (UnionOf i vaenv p1 p2s) = UnionOf i (descriptorVaenv env vaenv)(descriptor env p1) (map (descriptor env) p2s)

showConstraints :: FEnv -> [VConstraint] -> [SConstraint]
showConstraints env = map (showCon env)

showTraceConstrainEpoch :: FEnv -> TraceConstrainEpoch -> [(SConstraint, [(Pnt, Scheme)])]
showTraceConstrainEpoch env = map mapConstraint . filter (not . null . snd)
  where
    mapConstraint (con, pnts) = (showCon env con, pnts)

matchingConstraintHelper :: FEnv -> VarMeta -> VarMeta -> VarMeta -> Bool
matchingConstraintHelper env p p2 p3 = equivalent env p p2 || equivalent env p p3

matchingConstraint :: FEnv -> VarMeta -> VConstraint -> Bool
matchingConstraint env p con = any (equivalent env p) $ constraintMetas con

showMatchingConstraints :: FEnv -> VarMeta -> [SConstraint]
showMatchingConstraints env@FEnv{feCons} matchVar = map (showCon env) $ filter (matchingConstraint env matchVar) feCons

mkTracedTypeCheckError :: FEnv -> VarMeta -> CodeRange -> String -> TypeCheckError
mkTracedTypeCheckError env m = TracedTypeCheckError m (showMatchingConstraints env m)

mkConstraintTypeCheckError :: FEnv -> VConstraint -> [TypeCheckError] -> TypeCheckError
mkConstraintTypeCheckError env c = ConstraintTypeCheckError c (showCon env c)
