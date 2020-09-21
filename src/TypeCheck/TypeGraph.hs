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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.TypeGraph where

import qualified Data.HashMap.Strict           as H
import qualified Data.HashSet        as S
import           Data.Maybe

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           TypeCheck.Common
import           Text.Printf

buildUnionObj :: FEnv -> [VObject] -> FEnv
buildUnionObj env1 objs = do
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  let (unionTypeObjs, env3) = fresh env2 $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjs"
  let (unionAllObjsPs, env4) = fresh env3 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"
  let (unionTypeObjsPs, env5) = fresh env4 $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjsPs"
  let constraints = [unionObjs unionAllObjs objs, unionObjs unionTypeObjs $ filterTypes objs, PowersetTo unionAllObjs unionAllObjsPs, PowersetTo unionTypeObjs unionTypeObjsPs]
  let env6 = (\env -> env{feUnionAllObjs=unionAllObjsPs, feUnionTypeObjs=unionTypeObjsPs}) env5
  addConstraints env6 constraints
                    where
                      unionObjs pnt os = UnionOf pnt $ map (\(Object m _ _ _ _) -> getPnt m) os
                      filterTypes = filter (\(Object _ basis _ _ _) -> basis == TypeObj)

buildTypeEnv :: FEnv -> VObjectMap -> FEnv
buildTypeEnv env objMap = buildUnionObj env (map fst objMap)

inferArgFromPartial :: FEnv -> PartialType -> Type
inferArgFromPartial FEnv{feTypeGraph, feClassMap} (PTypeName partialName, partialVars, partialProps, partialArgs) = do
  let typeArrows = H.lookupDefault [] partialName feTypeGraph
  unionTypes feClassMap $ map tryArrow typeArrows
  where
    tryArrow ((Object _ _ _ _ objArgs), _) = if H.keysSet partialArgs `isSubsetOf` H.keysSet objArgs
      then SumType $ joinPartialLeafs $ map addArg $ S.toList $ S.difference (H.keysSet objArgs) (H.keysSet partialArgs)
      else bottomType
    addArg arg = (PTypeName partialName, partialVars, partialProps, H.insertWith (unionType feClassMap) arg TopType partialArgs)
inferArgFromPartial _ (PClassName _, _, _, _) = bottomType

ubFromScheme :: FEnv -> Scheme -> TypeCheckResult Type
ubFromScheme _ (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme env (TypeCheckResult _ (SVar _ p))  = ubFromScheme env (descriptor env p)
ubFromScheme _ (TypeCheckResE notes) = TypeCheckResE notes

data ReachesTree
  = ReachesTree (H.HashMap PartialType ReachesTree)
  | ReachesLeaf [Type]
  deriving (Show)

unionReachesTree :: ClassMap -> ReachesTree -> Type
unionReachesTree classMap (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = SumType $ joinPartialLeafs keys
  let vals' = map (unionReachesTree classMap) vals
  unionTypes classMap (keys':vals')
unionReachesTree classMap (ReachesLeaf leafs) = unionTypes classMap leafs

reachesHasCutSubtypeOf :: ClassMap -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classMap (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = hasPartial classMap key superType || reachesHasCutSubtypeOf classMap val superType
reachesHasCutSubtypeOf classMap (ReachesLeaf leafs) superType = any (\t -> hasType classMap t superType) leafs

reachesPartial :: FEnv -> PartialType -> TypeCheckResult ReachesTree
reachesPartial env@FEnv{feTypeGraph, feClassMap} partial@(PTypeName partialName, _, _, _) = do
  let typeArrows = H.lookupDefault [] partialName feTypeGraph
  schemes <- mapM tryArrow typeArrows
  return $ ReachesLeaf $ catMaybes schemes
  where
    tryArrow (obj@(Object (VarMeta objP _) _ _ _ _), arr) = do
      let objScheme = descriptor env objP
      ubFromScheme env objScheme >>= \objUb -> do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        let potentialSrc@(SumType potSrcLeafs) = intersectTypes feClassMap (singletonType partial) objUb
        return $ if not (isBottomType potentialSrc)
          -- TODO: Should this line below call `reaches` to make this recursive?
          -- otherwise, no reaches path requiring multiple steps can be found
          then Just $ unionTypes feClassMap [arrowDestType True feClassMap potentialSrcPartial obj arr | potentialSrcPartial <- splitPartialLeafs potSrcLeafs]
          else Nothing
reachesPartial env@FEnv{feClassMap} partial@(PClassName _, _, _, _) = reaches env (expandClassPartial feClassMap partial)

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
arrowConstrainUbs env@FEnv{feUnionAllObjs} TopType dest@SumType{} = do
  unionPnt <- descriptor env feUnionAllObjs
  case unionPnt of
    (SType unionUb@SumType{} _ _) -> do
      (src', dest') <- arrowConstrainUbs env unionUb dest
      return (src', dest')
    _ -> return (TopType, dest)
arrowConstrainUbs _ TopType dest = return (TopType, dest)
arrowConstrainUbs _ (TypeVar v) _ = error $ printf "arrowConstrainUbs typeVar %s" (show v)
arrowConstrainUbs env (SumType srcPartials) dest = do
  let classMap = feClassMap env
  let srcPartialList = splitPartialLeafs srcPartials
  srcPartialList' <- mapM (rootReachesPartial env) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let partialMap' = H.filter (\t -> reachesHasCutSubtypeOf classMap t dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinPartialLeafs srcPartialList''
  let destByGraph = unionTypes classMap $ fmap (unionReachesTree classMap) destByPartial
  dest' <- tryIntersectTypes env dest destByGraph "executeConstraint ArrowTo"
  return (compactType classMap $ SumType srcPartials', dest')
