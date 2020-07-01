{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeGraph
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.TypeGraph where

import qualified Data.HashMap.Strict           as H
import           Data.Tuple.Sequence
import           Data.Maybe

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           TypeCheck.Common

buildUnionObj :: FEnv -> [VObject] -> FEnv
buildUnionObj env1 objs = do
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  let (unionTypeObjs, env3) = fresh env2 $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjs"
  let (unionAllObjsPs, env4) = fresh env3 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"
  let (unionTypeObjsPs, env5) = fresh env4 $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjsPs"
  let constraints = [unionObjs unionAllObjs objs, unionObjs unionTypeObjs $ filterTypes objs, PowersetTo unionAllObjs unionAllObjsPs, PowersetTo unionTypeObjs unionTypeObjsPs]
  let unionObjs' = (unionAllObjsPs, unionTypeObjsPs)
  let env6 = (\(FEnv pnts cons (_, graph) pmap errs) -> FEnv pnts cons (unionObjs', graph) pmap errs) env5
  addConstraints env6 constraints
                    where
                      unionObjs pnt os = UnionOf pnt $ map (\(Object m _ _ _ _) -> getPnt m) os
                      filterTypes = filter (\(Object _ basis _ _ _) -> basis == TypeObj)

buildTypeEnv :: FEnv -> VObjectMap -> FEnv
buildTypeEnv env objMap = buildUnionObj env (map fst objMap)

ubFromScheme :: Scheme -> TypeCheckResult Type
ubFromScheme (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme (TypeCheckResE notes) = TypeCheckResE notes

reachesPartial :: FEnv -> PartialType -> TypeCheckResult Type
reachesPartial env@(FEnv _ _ (_, graph) _ _) partial@(partialName, _, _) = do
  let typeArrows = H.lookupDefault [] partialName graph
  schemes <- mapM tryArrow typeArrows
  return $ joinDestTypes $ catMaybes schemes
  where
    tryArrow (obj@(Object (VarMeta objP _) _ _ _ _), arr@(Arrow (VarMeta arrP _) _ _ _)) = do
      let objScheme = descriptor env objP
      let arrScheme = descriptor env arrP
      sequenceT (ubFromScheme objScheme, ubFromScheme arrScheme) >>= \(objUb, arrUb) -> return $ if hasPartial partial objUb
        then Just $ intersectTypes arrUb (arrowDestType partial obj arr)
        else Nothing
    idReach = SumType (joinPartialLeafs [partial])
    joinDestTypes destTypes = unionTypes (idReach:destTypes)

reaches :: FEnv -> Type -> TypeCheckResult Type
reaches _     TopType            = return TopType
reaches _     TypeVar{}            = error "reaches TypeVar"
reaches typeEnv (SumType srcPartials) = do
  resultsByPartials <- mapM (reachesPartial typeEnv) $ splitPartialLeafs srcPartials
  return $ unionTypes resultsByPartials


arrowConstrainUbs :: FEnv -> Type -> Type -> TypeCheckResult (Type, Type)
arrowConstrainUbs _ TopType dest = return (TopType, dest)
arrowConstrainUbs _ TypeVar{} _ = error "arrowConstrainUbs typeVar"
arrowConstrainUbs env (SumType srcPartials) dest = do
  let srcPartialList = splitPartialLeafs srcPartials
  srcPartialList' <- mapM (reachesPartial env) srcPartialList
  let partialMap = H.fromList $ zip srcPartialList srcPartialList'
  let partialMap' = H.filter (`hasType` dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinPartialLeafs srcPartialList''
  let destByGraph = unionTypes destByPartial
  dest' <- tryIntersectTypes dest destByGraph "executeConstraint ArrowTo"
  return (SumType srcPartials', dest')
