--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Show
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Show where

import qualified Data.HashMap.Strict as H

import           Syntax.Prgm
import           TypeCheck.Common

showM :: FEnv -> VarMeta -> ShowMeta
showM env = descriptor env . getPnt

showExpr :: FEnv -> VExpr -> SExpr
showExpr env (CExpr m c) = CExpr (showM env m) c
showExpr env (Value m name) = Value (showM env m) name
showExpr env (Arg m name) = Arg (showM env m) name
showExpr env (TupleApply m (bm, base) argName argVal) = TupleApply (showM env m) (showM env bm, showExpr env base) argName (showExpr env argVal)

showCompAnnot :: FEnv -> VCompAnnot -> SCompAnnot
showCompAnnot env (CompAnnot name args) = CompAnnot name (fmap (showExpr env) args)

showGuard :: FEnv -> VGuard -> SGuard
showGuard env (IfGuard e) = IfGuard (showExpr env e)
showGuard _ ElseGuard = ElseGuard
showGuard _ NoGuard = NoGuard

showArrow :: FEnv -> VArrow -> SArrow
showArrow env (Arrow m annots guard maybeExpr) = case maybeExpr of
    Just expr -> Arrow m' annots' guard' (Just $ showExpr env expr)
    Nothing -> Arrow m' annots' guard' Nothing
  where
    m' = showM env m
    annots' = fmap (showCompAnnot env) annots
    guard' = showGuard env guard

showObjArg :: FEnv -> VObjArg -> SObjArg
showObjArg env (m, maybeObj) = case maybeObj of
    Just obj -> (m', Just $ showObj env obj)
    Nothing -> (m', Nothing)
  where m' = showM env m

showObj :: FEnv -> VObject -> SObject
showObj env (Object m basis name vars args) = Object (showM env m) basis name (fmap (showM env) vars) (fmap (showObjArg env) args)

showObjArrows :: FEnv -> (VObject, [VArrow]) -> (SObject, [SArrow])
showObjArrows env (obj, arrows) = (showObj env obj, map (showArrow env) arrows)

showConHelper :: FEnv -> (Scheme -> Scheme -> SConstraint) -> Pnt -> Pnt -> SConstraint
showConHelper env f p1 p2 = f (descriptor env p1) (descriptor env p2)

showCon :: FEnv -> Constraint -> SConstraint
showCon env (EqualsKnown p t) = SEqualsKnown (descriptor env p) t
showCon env (EqPoints p1 p2) = showConHelper env SEqPoints p1 p2
showCon env (BoundedByKnown p t) = SBoundedByKnown (descriptor env p) t
showCon env (BoundedByObjs b p) = SBoundedByObjs b (descriptor env p)
showCon env (ArrowTo p1 p2) = showConHelper env SArrowTo p1 p2
showCon env (PropEq (p1, name) p2) = showConHelper env (\s1 s2 -> SPropEq (s1, name) s2) p1 p2
showCon env (VarEq (p1, name) p2) = showConHelper env (\s1 s2 -> SVarEq (s1, name) s2) p1 p2
showCon env (AddArg (p1, argNames) p2) = showConHelper env (\s1 s2 -> SAddArg (s1, argNames) s2) p1 p2
showCon env (PowersetTo p1 p2) = showConHelper env SPowersetTo p1 p2
showCon env (UnionOf p1 p2s) = SUnionOf (descriptor env p1) (map (descriptor env) p2s)

showPrgm :: FEnv -> VPrgm -> SPrgm
showPrgm env (objMap, classMap) = (H.fromList $ map (showObjArrows env) objMap, classMap)

showConstraints :: FEnv -> [Constraint] -> [SConstraint]
showConstraints env = map (showCon env)
