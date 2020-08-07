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
  let env6 = (\(FEnv pnts cons (_, graph) pmap) -> FEnv pnts cons (unionObjs', graph) pmap) env5
  addConstraints env6 constraints
                    where
                      unionObjs pnt os = UnionOf pnt $ map (\(Object m _ _ _ _) -> getPnt m) os
                      filterTypes = filter (\(Object _ basis _ _ _) -> basis == TypeObj)

buildTypeEnv :: FEnv -> VObjectMap -> FEnv
buildTypeEnv env objMap = buildUnionObj env (map fst objMap)

ubFromScheme :: FEnv -> Scheme -> TypeCheckResult Type
ubFromScheme _ (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme env (TypeCheckResult _ (SVar _ p))  = ubFromScheme env (descriptor env p)
ubFromScheme _ (TypeCheckResE notes) = TypeCheckResE notes

data ReachesTree
  = ReachesTree (H.HashMap PartialType ReachesTree)
  | ReachesLeaf [Type]
  deriving (Show)

unionReachesTree :: ReachesTree -> Type
unionReachesTree (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = SumType $ joinPartialLeafs keys
  let vals' = map unionReachesTree vals
  unionTypes (keys':vals')
unionReachesTree (ReachesLeaf leafs) = unionTypes leafs

reachesHasCutSubtypeOf :: ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = key `hasPartial` superType || reachesHasCutSubtypeOf val superType
reachesHasCutSubtypeOf (ReachesLeaf leafs) superType = any (`hasType` superType) leafs

reachesPartial :: FEnv -> PartialType -> TypeCheckResult ReachesTree
reachesPartial env@(FEnv _ _ (_, graph) _) partial@(partialName, _, _) = do
  let typeArrows = H.lookupDefault [] partialName graph
  schemes <- mapM tryArrow typeArrows
  return $ ReachesLeaf $ catMaybes schemes
  where
    tryArrow (obj@(Object (VarMeta objP _) _ _ _ _), arr@(Arrow (VarMeta arrP _) _ _ _)) = do
      let objScheme = descriptor env objP
      let arrScheme = descriptor env arrP
      sequenceT (ubFromScheme env objScheme, ubFromScheme env arrScheme) >>= \(objUb, arrUb) -> return $ if hasPartial partial objUb
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then Just $ intersectTypes arrUb (arrowDestType partial obj arr)
        else Nothing

reaches :: FEnv -> Type -> TypeCheckResult ReachesTree
reaches _     TopType            = return $ ReachesLeaf [TopType]
reaches _     TypeVar{}            = error "reaches TypeVar"
reaches typeEnv (SumType src) = do
  let partials = splitPartialLeafs src
  resultsByPartials <- mapM (reachesPartial typeEnv) partials
  return $ ReachesTree $ H.fromList $ zip partials resultsByPartials

rootReachesPartial :: FEnv -> PartialType -> TypeCheckResult (PartialType, ReachesTree)
rootReachesPartial env src = do
  reached <- reachesPartial env src
  let reachedWithId = ReachesTree $ H.singleton src reached
  return (src, reachedWithId)

arrowConstrainUbs :: FEnv -> Type -> Type -> TypeCheckResult (Type, Type)
arrowConstrainUbs _ TopType dest = return (TopType, dest)
arrowConstrainUbs _ TypeVar{} _ = error "arrowConstrainUbs typeVar"
arrowConstrainUbs env (SumType srcPartials) dest = do
  let srcPartialList = splitPartialLeafs srcPartials
  srcPartialList' <- mapM (rootReachesPartial env) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let partialMap' = H.filter (`reachesHasCutSubtypeOf` dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinPartialLeafs srcPartialList''
  let destByGraph = unionTypes $ fmap unionReachesTree destByPartial
  dest' <- tryIntersectTypes dest destByGraph "executeConstraint ArrowTo"
  return (SumType srcPartials', dest')
