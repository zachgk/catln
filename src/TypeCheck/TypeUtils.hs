--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeUtils
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles some type utilities for type checking.
-- It supports the 'ArrowTo' constraint to compute resulting types through the typeGraph
-- This module also computes what the Any
-- types are which result from joining the types of all objects.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TupleSections    #-}

module TypeCheck.TypeUtils where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe

import           Control.Monad
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.TypeGraph
import           Semantics.Types
import           TypeCheck.Common
import           TypeCheck.Show
import           Utils

-- |
-- The object precedence is used to avoid increasing the scope of objects accidentally.
-- For example, if a data type is defined, functions using that data type shouldn't change the valid arguments to it
-- Especially, not specifying bounds should not turn them into TopType.
-- Similarly, matches or patterns are less effective then functions.
-- TODO May need to differentiate top level of functions from inner levels
objectPrecedence :: ObjectMapItem e m -> [Int]
objectPrecedence (Object{objBasis=TypeObj}, _, _)=    [1]
objectPrecedence (Object{objBasis=FunctionObj}, _, arrs) = [2, declDef]
  where
    declDef = if any hasDefinition arrs
      then 2 -- Definition objects have priority [2,2]
      else 1 -- Declaration objects have priority [2,1], better than definitions
    hasDefinition (Arrow _ _ (Just _)) = True
    hasDefinition (Arrow _ _ Nothing)  = False
objectPrecedence (Object{objBasis=PatternObj}, _, _) = [3]
objectPrecedence (Object{objBasis=MatchObj}, _, _) =   [4]
objectPrecedence (Object{objBasis=ArgObj}, _, _) =   [5]

-- | Finds the 'objectPrecedence' for all types
buildPrecedenceMap :: (Show m, Show (e m), MetaDat m, ExprClass e) => ObjectMap e m -> H.HashMap TypeName [Int]
buildPrecedenceMap = fmap (minimum . map objectPrecedence) . H.fromListWith (++) . map (\(obj, annots, arrs) -> (objPath obj, [(obj, annots, arrs)]))

-- |
-- Prunes an objectMap by precendence. If two objects share the same precendence, only the bigger one(s) will be kept.
-- This is used to ensure that the type of an object can't be changed by other usages, such as a data object by functions using that data
filterBestPrecedence :: (Show m, Show (e m), MetaDat m, ExprClass e) => H.HashMap TypeName [Int] -> ObjectMap e m -> ObjectMap e m
filterBestPrecedence precedenceMap = filter (\omi@(obj, _, _) -> objectPrecedence omi == H.lookupDefault (error "Could not find obj in union") (objPath obj) precedenceMap)

-- | Gets an object and all sub-ojects (recursively) from it's arguments
getRecursiveObjs :: ObjectMapItem e m -> ObjectMap e m
getRecursiveObjs (obj@Object{deprecatedObjArgs}, annots, arr) = (obj, annots, arr) : subObjMap
  where
    subObjMap = concatMap (filter notMatchObj . concatMap (getRecursiveObjs . (,[],Nothing)) . maybeToList . snd) (H.elems deprecatedObjArgs)
    notMatchObj (Object{objBasis}, _, _) = objBasis /= MatchObj

-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: FEnv -> VObjectMap -> TObjectMap -> FEnv
addUnionObjToEnv env1@FEnv{feClassGraph} vobjMap tobjMap = do
  let vobjMapRec = concatMap getRecursiveObjs vobjMap
  let tobjMapRec = concatMap getRecursiveObjs tobjMap

  -- Finds the best precedence for all each object name
  let vPrecedenceMap = buildPrecedenceMap vobjMapRec
  let tPrecedenceMap = buildPrecedenceMap tobjMapRec
  let precedenceMap = H.unionWith min vPrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let vobjs' = map fst3 $ filterBestPrecedence precedenceMap vobjMapRec
  let tobjs' = map fst3 $ filterBestPrecedence precedenceMap tobjMapRec

  -- Builds vars to use for union and union powerset
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjs"
  let (unionAllObjsPs, env3) = fresh env2 $ TypeCheckResult [] $ SType TopType bottomType "unionAllObjsPs"

  let mkVarMeta p = Meta TopType Nothing (VarMetaDat p Nothing H.empty H.empty)

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feClassGraph $ map (getMetaType . objM) tobjs'
  let (typecheckedAllObjs, env4) = fresh env3 $ TypeCheckResult [] $ SType typecheckedAllType bottomType "typecheckedAll"
  let typecheckedAllObjs' = mkVarMeta typecheckedAllObjs

  -- Builds metas to use for union and union powerset
  let unionAllObjs' = mkVarMeta unionAllObjs
  let unionAllObjsPs' = mkVarMeta unionAllObjsPs

  let constraints = [
        UnionOf unionAllObjs' (typecheckedAllObjs' : map objM vobjs'),
        PowersetTo unionAllObjs' unionAllObjsPs'
        ]
  let env5 = (\env -> env{feUnionAllObjs=unionAllObjsPs'}) env4
  addConstraints env5 constraints


inferArgFromPartial :: FEnv -> PartialType -> Type
inferArgFromPartial FEnv{feVTypeGraph, feTTypeGraph, feClassGraph} partial@PartialType{ptName=PTypeName name, ptArgs} = do
  let vtypeArrows = H.lookupDefault [] name feVTypeGraph
  let vTypes = unionAllTypes feClassGraph $ map tryArrow vtypeArrows

  let ttypeArrows = H.lookupDefault [] name feTTypeGraph
  let tTypes = unionAllTypes feClassGraph $ map tryArrow ttypeArrows

  unionTypes feClassGraph vTypes tTypes
  where
    tryArrow (obj, _) = if H.keysSet ptArgs `isSubsetOf` H.keysSet (objAppliedArgsMap obj)
      then UnionType $ joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet $ objAppliedArgsMap obj) (H.keysSet ptArgs)
      else bottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feClassGraph) arg TopType ptArgs}
inferArgFromPartial _ _ = bottomType

mkReachesEnv :: FEnv -> TypeCheckResult (ReachesEnv ())
mkReachesEnv env@FEnv{feClassGraph, feUnionAllObjs, feVTypeGraph, feTTypeGraph} = do
  (SType unionAll _ _) <- descriptor env feUnionAllObjs
  feVTypeGraph' <- forM feVTypeGraph $ \objArrs -> do
    forM objArrs $ \(vobj, varr) -> do
      objUb <- pointUb env (objM vobj)
      soa <- showObjArrow env (vobj, [], Just varr)
      let soa' = mapMetaObjArr clearMetaDat soa
      let soa'' = mapOAObjExpr (exprWithMetaType objUb) soa'
      return soa''
  let feTTypeGraph' = fmap (map (asExprObjectMapItem . (\(o, a) -> (o, [], Just a)))) feTTypeGraph
  let typeGraph = H.unionWith (++) feVTypeGraph' feTTypeGraph'
  return $ ReachesEnv feClassGraph unionAll typeGraph

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
arrowConstrainUbs env@FEnv{feClassGraph} (UnionType srcPartials) (Meta _ _ (VarMetaDat _ _ varEnv argEnv)) dest _ = do
  let srcPartialList = splitUnionType srcPartials
  reachesEnv <- mkReachesEnv env
  srcPartialList' <- mapM (resToTypeCheck . rootReachesPartial reachesEnv) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let partialMap' = H.filter (\t -> reachesHasCutSubtypeOf feClassGraph varEnv argEnv t dest) partialMap
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap'
  let srcPartials' = joinUnionType srcPartialList''
  let destByGraph = unionAllTypes feClassGraph $ fmap (unionReachesTree feClassGraph) destByPartial
  dest' <- tryIntersectTypes env dest destByGraph "executeConstraint ArrowTo"
  return (compactType feClassGraph $ UnionType srcPartials', dest')
