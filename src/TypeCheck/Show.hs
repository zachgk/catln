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

import           Control.Monad.Identity (Identity (runIdentity))
import           Data.Bifunctor         (bimap)
import           Data.Maybe             (fromJust)
import           MapMeta
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

showCon :: FEnv -> VConstraint -> SConstraint
showCon env@FEnv{feUnionAllObjs} con = case runIdentity $ mapMCon (return . descriptor env) con of
  con'@Constraint{conDat=BoundedByObjs i p _} -> con'{conDat=BoundedByObjs i p (stypeAct $ fromJust $ tcreToMaybe $ descriptor env feUnionAllObjs)}
  con' -> con'

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
