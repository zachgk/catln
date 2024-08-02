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

import           Data.Bifunctor   (bimap)
import           Data.Maybe       (fromJust)
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           TypeCheck.Common

showM :: FEnv -> MetaType -> VarMeta -> TypeCheckResult ShowMeta
showM env@FEnv{feTypeEnv} _ m = do
  stype@SType{stypeAct} <- descriptor env m
  let tp' = intersectTypes feTypeEnv (getMetaType m) stypeAct
  return $ mapMetaDat (ShowMeta stype) (mWithType tp' m)

showExpr :: FEnv -> MetaLocation -> VExpr -> TypeCheckResult SExpr
showExpr env = mapMetaM (showM env)

showObjArr :: FEnv -> VObjArr -> TypeCheckResult SObjArr
showObjArr env = mapMetaObjArrM (showM env) Nothing

showConDatHelper :: FEnv -> (Scheme -> Scheme -> SConstraintDat) -> VarMeta -> VarMeta -> SConstraintDat
showConDatHelper env f p1 p2 = f (descriptor env p1) (descriptor env p2)

showConDat :: FEnv -> VConstraintDat -> SConstraintDat
showConDat env (EqualsKnown i p t) = EqualsKnown i (descriptor env p) t
showConDat env (EqPoints i p1 p2) = showConDatHelper env (EqPoints i) p1 p2
showConDat env (BoundedByKnown i p t) = BoundedByKnown i (descriptor env p) t
showConDat env@FEnv{feUnionAllObjs} (BoundedByObjs i p _) = BoundedByObjs i (descriptor env p) (stypeAct $ fromJust $ tcreToMaybe $ descriptor env feUnionAllObjs)
showConDat env (NoReturnArg i p) = NoReturnArg i (descriptor env p)
showConDat env (ArrowTo i p1 p2) = showConDatHelper env (ArrowTo i) p1 p2
showConDat env (PropEq i (p1, name) p2) = showConDatHelper env (\s1 s2 -> PropEq i (s1, name) s2) p1 p2
showConDat env (AddArg i (p1, argName) p2) = showConDatHelper env (\s1 s2 -> AddArg i (s1, argName) s2) p1 p2
showConDat env (AddInferArg i p1 p2) = showConDatHelper env (AddInferArg i) p1 p2
showConDat env (SetArgMode i m p1 p2) = showConDatHelper env (SetArgMode i m) p1 p2
showConDat env (ConWhere i p1 p2 p3) = ConWhere i (descriptor env p1) (descriptor env p2) (descriptor env p3)
showConDat env (UnionOf i p1 p2s) = UnionOf i (descriptor env p1) (map (descriptor env) p2s)

showCon :: FEnv -> VConstraint -> SConstraint
showCon env (Constraint oa vaenv dat) = Constraint oa (descriptorVaenvIO env vaenv) (showConDat env dat)

showConstraints :: FEnv -> [VConstraint] -> [SConstraint]
showConstraints env = map (showCon env)

showTraceConstrainEpoch :: FEnv -> TraceConstrainEpoch -> SConstraintEpoch
showTraceConstrainEpoch env = SConstraintEpoch . map mapConstraint . filter (not . null . snd)
  where
    mapConstraint (con, pnts) = (showCon env con, pnts)

showTraceConstrainPnt :: FEnv -> Pnt -> SConstrainPnt
showTraceConstrainPnt env@FEnv{feTrace=TraceConstrain{tcEpochs}} p = SConstrainPnt $ map (map (bimap (showCon env) (fromJust . lookup p)) . filter (elem p . map fst . snd)) tcEpochs

matchingConstraintHelper :: FEnv -> VarMeta -> VarMeta -> VarMeta -> Bool
matchingConstraintHelper env p p2 p3 = equivalent env p p2 || equivalent env p p3

matchingConstraint :: FEnv -> VarMeta -> VConstraint -> Bool
matchingConstraint env p con = any (equivalent env p) $ constraintMetas con

showMatchingConstraints :: FEnv -> VarMeta -> [SConstraint]
showMatchingConstraints env@FEnv{feCons} matchVar = map (showCon env) $ filter (matchingConstraint env matchVar) feCons

mkTracedTypeCheckError :: FEnv -> VarMeta -> String -> TypeCheckError
mkTracedTypeCheckError env m = TracedTypeCheckError m (showMatchingConstraints env m)

mkConstraintTypeCheckError :: FEnv -> VConstraint -> [TypeCheckError] -> TypeCheckError
mkConstraintTypeCheckError env c = ConstraintTypeCheckError c (showCon env c)
