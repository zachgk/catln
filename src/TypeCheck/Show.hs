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
import           Data.Maybe       (fromJust)
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
showExpr env (EWhere base cond) = do
  base' <- showExpr env base
  cond' <- showExpr env cond
  return $ EWhere base' cond'
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

showObjArr :: FEnv -> VObjArr -> TypeCheckResult SObjArr
showObjArr env oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (showExpr env) oaObj
  oaAnnots' <- mapM (showExpr env) oaAnnots
  oaArr' <- forM oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (showExpr env) arrE
    arrM' <- showM env arrM
    return (arrE', arrM')
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

showConDatHelper :: FEnv -> (Scheme -> Scheme -> SConstraintDat) -> VarMeta -> VarMeta -> SConstraintDat
showConDatHelper env f p1 p2 = f (descriptor env p1) (descriptor env p2)

showConDat :: FEnv -> VConstraintDat -> SConstraintDat
showConDat env (EqualsKnown i p t) = EqualsKnown i (descriptor env p) t
showConDat env (EqPoints i p1 p2) = showConDatHelper env (EqPoints i) p1 p2
showConDat env (BoundedByKnown i p t) = BoundedByKnown i (descriptor env p) t
showConDat env@FEnv{feUnionAllObjs} (BoundedByObjs i p _) = BoundedByObjs i (descriptor env p) (stypeAct $ fromJust $ tcreToMaybe $ descriptor env feUnionAllObjs)
showConDat env (ArrowTo i p1 p2) = showConDatHelper env (ArrowTo i) p1 p2
showConDat env (PropEq i (p1, name) p2) = showConDatHelper env (\s1 s2 -> PropEq i (s1, name) s2) p1 p2
showConDat env (AddArg i (p1, argName) p2) = showConDatHelper env (\s1 s2 -> AddArg i (s1, argName) s2) p1 p2
showConDat env (AddInferArg i p1 p2) = showConDatHelper env (AddInferArg i) p1 p2
showConDat env (PowersetTo i p1 p2) = showConDatHelper env (PowersetTo i) p1 p2
showConDat env (UnionOf i p1 p2s) = UnionOf i (descriptor env p1) (map (descriptor env) p2s)

showCon :: FEnv -> VConstraint -> SConstraint
showCon env (Constraint oa vaenv dat) = Constraint oa (descriptorVaenvIO env vaenv) (showConDat env dat)

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
