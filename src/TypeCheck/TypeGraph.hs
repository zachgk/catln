--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeGraph
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles the type graph during type checking.
-- It mainly supports the 'ArrowTo' constraint to compute
-- what resulting types can be produced by using all of the rewrite
-- rules that are in scope. This module also computes what the Any
-- types are which result from joining the types of all objects.
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
import Text.Printf
import Data.List (partition)
import TypeCheck.Show
import Parser.Syntax (emptyMetaN)

buildUnionObj :: FEnv -> [VObject] -> [TObject] -> FEnv
buildUnionObj env1@FEnv{feClassMap} vobjs tobjs = do
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  let (unionTypeObjs, env3) = fresh env2 $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjs"
  let (unionAllObjsPs, env4) = fresh env3 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"
  let (unionTypeObjsPs, env5) = fresh env4 $ TypeCheckResult [] $ SType TopType bottomType "unionTypeObjsPs"

  let typecheckedAllType = makeTypechecked tobjs
  let (typecheckedAllObjs, env6) = fresh env5 $ TypeCheckResult [] $ SType typecheckedAllType bottomType "typecheckedAll"
  let typecheckedAllObjs' = VarMeta typecheckedAllObjs emptyMetaN Nothing

  let typecheckedTypeType = makeTypechecked $ filterTypeObjs tobjs
  let (typecheckedTypeObjs, env7) = fresh env6 $ TypeCheckResult [] $ SType typecheckedTypeType bottomType "typecheckedType"
  let typecheckedTypeObjs' = VarMeta typecheckedTypeObjs emptyMetaN Nothing

  let unionAllObjs' = VarMeta unionAllObjs emptyMetaN Nothing
  let unionTypeObjs' = VarMeta unionTypeObjs emptyMetaN Nothing
  let unionAllObjsPs' = VarMeta unionAllObjsPs emptyMetaN Nothing
  let unionTypeObjsPs' = VarMeta unionTypeObjsPs emptyMetaN Nothing

  let constraints = [
        unionObjs unionAllObjs' typecheckedAllObjs' vobjs,
        unionObjs unionTypeObjs' typecheckedTypeObjs' (filterTypeObjs vobjs),
        PowersetTo unionAllObjs' unionAllObjsPs',
        PowersetTo unionTypeObjs' unionTypeObjsPs'
        ]
  let env8 = (\env -> env{feUnionAllObjs=unionAllObjsPs', feUnionTypeObjs=unionTypeObjsPs'}) env7
  addConstraints env8 constraints
                    where
                      unionObjs pnt known objects = UnionOf pnt (known : map objM objects)
                      filterTypeObjs = filter (\Object{objBasis} -> objBasis == TypeObj)
                      makeTypechecked objs = unionTypes feClassMap $ map (getMetaType . objM) objs

buildTypeEnv :: FEnv -> VObjectMap -> TObjectMap -> FEnv
buildTypeEnv env vobjMap tobjMap = buildUnionObj env (map fst vobjMap) (map fst tobjMap)

inferArgFromPartial :: FEnv -> PartialType -> Type
inferArgFromPartial FEnv{feVTypeGraph, feTTypeGraph, feClassMap} partial@PartialType{ptName=PTypeName name, ptArgs} = do
  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  let vTypes = unionTypes feClassMap $ map tryArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  let tTypes = unionTypes feClassMap $ map tryArrow ttypeArrows

  unionType feClassMap vTypes tTypes
  where
    tryArrow (Object{objArgs}, _) = if H.keysSet ptArgs `isSubsetOf` H.keysSet objArgs
      then UnionType $ joinPartialLeafs $ map addArg $ S.toList $ S.difference (H.keysSet objArgs) (H.keysSet ptArgs)
      else bottomType
    addArg arg = partial{ptArgs=H.insertWith (unionType feClassMap) arg TopType ptArgs}
inferArgFromPartial _ PartialType{ptName=PClassName{}} = bottomType

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _ = False

data ReachesTree
  = ReachesTree (H.HashMap PartialType ReachesTree)
  | ReachesLeaf [Type]
  deriving (Show)

unionReachesTree :: ClassMap -> ReachesTree -> Type
unionReachesTree classMap (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = UnionType $ joinPartialLeafs keys
  let vals' = map (unionReachesTree classMap) vals
  let both = keys':vals'
  case partition isTypeVar both of
    ([onlyVar], []) -> onlyVar
    (_, sums) -> unionTypes classMap sums
unionReachesTree classMap (ReachesLeaf leafs) = unionTypes classMap leafs

hasPartialWithMaybeObj :: (Meta m) => ClassMap -> Maybe (Object m) -> PartialType -> Type -> Bool
hasPartialWithMaybeObj classMap (Just obj) = hasPartialWithObj classMap obj
hasPartialWithMaybeObj classMap Nothing = hasPartial classMap

hasTypeWithMaybeObj :: (Meta m) => ClassMap -> Maybe (Object m) -> Type -> Type -> Bool
hasTypeWithMaybeObj classMap (Just obj) = hasTypeWithObj classMap obj
hasTypeWithMaybeObj classMap Nothing = hasType classMap

reachesHasCutSubtypeOf :: (Meta m) => ClassMap -> Maybe (Object m) -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classMap mObj (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = hasPartialWithMaybeObj classMap mObj key superType || reachesHasCutSubtypeOf classMap mObj val superType
reachesHasCutSubtypeOf classMap mObj (ReachesLeaf leafs) superType = any (\t -> hasTypeWithMaybeObj classMap mObj t superType) leafs

reachesPartial :: FEnv -> PartialType -> TypeCheckResult ReachesTree
reachesPartial env@FEnv{feVTypeGraph, feTTypeGraph, feClassMap} partial@PartialType{ptName=PTypeName name} = do

  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  vtypes <- mapM tryVArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  ttypes <- mapM tryTArrow ttypeArrows

  return $ ReachesLeaf (catMaybes vtypes ++ catMaybes ttypes)
  where
    tryVArrow (obj@Object{objM}, arr) = do
      pointUb env objM >>= \objUb -> do
        -- It is possible to send part of a partial through the arrow, so must compute the valid part
        -- If none of it is valid, then there is Nothing
        let potentialSrc@(UnionType potSrcLeafs) = intersectTypes feClassMap (singletonType partial) objUb
        if not (isBottomType potentialSrc)
          -- TODO: Should this line below call `reaches` to make this recursive?
          -- otherwise, no reaches path requiring multiple steps can be found
          then do
            sobj <- showObj env obj
            sarr <- showArrow env arr
            return $ Just $ unionTypes feClassMap [arrowDestType True feClassMap potentialSrcPartial sobj sarr | potentialSrcPartial <- splitPartialLeafs potSrcLeafs]
          else return Nothing
    tryTArrow (obj@Object{objM}, arr) = do
      -- It is possible to send part of a partial through the arrow, so must compute the valid part
      -- If none of it is valid, then there is Nothing
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes feClassMap (singletonType partial) (getMetaType objM)
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then return $ Just $ unionTypes feClassMap [arrowDestType True feClassMap potentialSrcPartial obj arr | potentialSrcPartial <- splitPartialLeafs potSrcLeafs]
        else return Nothing
reachesPartial env@FEnv{feClassMap} partial@PartialType{ptName=PClassName{}} = reaches env (expandClassPartial feClassMap partial)

reaches :: FEnv -> Type -> TypeCheckResult ReachesTree
reaches _     TopType            = return $ ReachesLeaf [TopType]
reaches _     (TypeVar v)            = error $ printf "reaches with typevar %s" (show v)
reaches typeEnv (UnionType src) = do
  let partials = splitPartialLeafs src
  resultsByPartials <- mapM (reachesPartial typeEnv) partials
  return $ ReachesTree $ H.fromList $ zip partials resultsByPartials

rootReachesPartial :: FEnv -> PartialType -> TypeCheckResult (PartialType, ReachesTree)
rootReachesPartial env src = do
  reached <- reachesPartial env src
  let reachedWithId = ReachesTree $ H.singleton src reached
  return (src, reachedWithId)

arrowConstrainUbs :: FEnv -> Type -> VarMeta -> Type -> VarMeta -> TypeCheckResult (Type, Type)
arrowConstrainUbs env@FEnv{feUnionAllObjs} TopType srcM dest@UnionType{} destM = do
  unionPnt <- descriptor env feUnionAllObjs
  case unionPnt of
    (SType unionUb@UnionType{} _ _) -> do
      (src', dest') <- arrowConstrainUbs env unionUb srcM dest destM
      return (src', dest')
    _ -> return (TopType, dest)
arrowConstrainUbs _ TopType _ dest _ = return (TopType, dest)
arrowConstrainUbs env src@(TypeVar v) srcM dest destM = do
  src' <- resolveTypeVar v srcM
  (_, cdest) <- arrowConstrainUbs env (getMetaType src') srcM dest destM
  return (src, cdest)
arrowConstrainUbs env (UnionType srcPartials) (VarMeta _ _ srcObj) dest _ = do
  let classMap = feClassMap env
  let srcPartialList = splitPartialLeafs srcPartials
  srcPartialList' <- mapM (rootReachesPartial env) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let partialMap' = H.filter (\t -> reachesHasCutSubtypeOf classMap srcObj t dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinPartialLeafs srcPartialList''
  let destByGraph = unionTypes classMap $ fmap (unionReachesTree classMap) destByPartial
  dest' <- tryIntersectTypes env dest destByGraph "executeConstraint ArrowTo"
  return (compactType classMap $ UnionType srcPartials', dest')
