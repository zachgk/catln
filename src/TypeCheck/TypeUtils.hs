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
exprObjectPrecedence :: (Show m, Show (e m)) => ExprObjectMapItem e m -> [Int]
exprObjectPrecedence ObjArr{oaBasis=TypeObj}=                        [1]
exprObjectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=(Nothing, _)} = [2, 1] -- Declaration objects have priority [2,1], better than definitions
exprObjectPrecedence ObjArr{oaBasis=FunctionObj, oaArr=(Just{}, _)}  = [2, 2] -- Definition objects have priority [2,2]
exprObjectPrecedence ObjArr{oaBasis=PatternObj}                      = [3]
exprObjectPrecedence ObjArr{oaBasis=MatchObj}                        =   [4]
exprObjectPrecedence ObjArr{oaBasis=ArgObj}                          =   [5]

-- | Finds the 'objectPrecedence' for all types
buildEPrecedenceMap :: (Show m, MetaDat m) => ExprObjectMap Expr m -> H.HashMap TypeName [Int]
buildEPrecedenceMap = fmap (minimum . map exprObjectPrecedence) . H.fromListWith (++) . map (\oa -> (oaObjPath oa, [oa]))

-- |
-- Prunes an objectMap by precendence. If two objects share the same precendence, only the bigger one(s) will be kept.
-- This is used to ensure that the type of an object can't be changed by other usages, such as a data object by functions using that data
filterBestEPrecedence :: (Show m, MetaDat m) => H.HashMap TypeName [Int] -> ExprObjectMap Expr m -> ExprObjectMap Expr m
filterBestEPrecedence precedenceMap = filter (\oa -> exprObjectPrecedence oa == H.lookupDefault (error "Could not find obj in union") (oaObjPath oa) precedenceMap)



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
buildPrecedenceMap :: (Show m, MetaDat m) => ObjectMap Expr m -> H.HashMap TypeName [Int]
buildPrecedenceMap = fmap (minimum . map objectPrecedence) . H.fromListWith (++) . map (\(obj, annots, arrs) -> (objPath obj, [(obj, annots, arrs)]))

-- |
-- Prunes an objectMap by precendence. If two objects share the same precendence, only the bigger one(s) will be kept.
-- This is used to ensure that the type of an object can't be changed by other usages, such as a data object by functions using that data
filterBestPrecedence :: (Show m, MetaDat m) => H.HashMap TypeName [Int] -> ObjectMap Expr m -> ObjectMap Expr m
filterBestPrecedence precedenceMap = filter (\omi@(obj, _, _) -> objectPrecedence omi == H.lookupDefault (error "Could not find obj in union") (objPath obj) precedenceMap)

-- | Gets an object and all sub-ojects (recursively) from it's arguments
getRecursiveObjs :: ObjectMapItem e m -> ObjectMap e m
getRecursiveObjs (obj@Object{deprecatedObjArgs}, annots, arr) = (obj, annots, arr) : subObjMap
  where
    subObjMap = concatMap (filter notMatchObj . concatMap (getRecursiveObjs . (,[],Nothing)) . maybeToList . snd) (H.elems deprecatedObjArgs)
    notMatchObj (Object{objBasis}, _, _) = objBasis /= MatchObj

-- | This creates 'feUnionAllObjs' and adds it to the 'FEnv'
addUnionObjToEnv :: FEnv -> VEObjectMap -> TEObjectMap -> FEnv
addUnionObjToEnv env1@FEnv{feClassGraph} veobjMap tobjMap = do
  let veobjMapRec = concatMap getRecursiveExprObjs veobjMap
  let tobjMapRec = concatMap getRecursiveExprObjs tobjMap

  -- Finds the best precedence for all each object name
  let vePrecedenceMap = buildEPrecedenceMap veobjMapRec
  let tPrecedenceMap = buildEPrecedenceMap tobjMapRec
  let precedenceMap = H.unionWith min vePrecedenceMap tPrecedenceMap

  -- Filter the objects to only those with the best precedence
  let veobjs' = filterBestEPrecedence precedenceMap veobjMapRec
  let tobjs' = filterBestEPrecedence precedenceMap tobjMapRec

  let veobjMetas = map (getExprMeta . oaObjExpr) veobjs'
  let tobjMetas = map (getMetaType . getExprMeta . oaObjExpr) tobjs'

  -- Builds vars to use for union and union powerset
  let (unionAllObjs, env2) = fresh env1 $ TypeCheckResult [] $ SType topType topType "unionAllObjs"
  let (unionAllObjsPs, env3) = fresh env2 $ TypeCheckResult [] $ SType topType topType "unionAllObjsPs"

  let mkVarMeta p = Meta topType Nothing (VarMetaDat (Just p) Nothing)

  -- Build a variable to store union of tobjs
  let typecheckedAllType = unionAllTypes feClassGraph tobjMetas
  let (typecheckedAllObjs, env4) = fresh env3 $ TypeCheckResult [] $ SType typecheckedAllType topType "typecheckedAll"
  let typecheckedAllObjs' = mkVarMeta typecheckedAllObjs

  -- Builds metas to use for union and union powerset
  let unionAllObjs' = mkVarMeta unionAllObjs
  let unionAllObjsPs' = mkVarMeta unionAllObjsPs

  let constraints = [
        UnionOf 1 H.empty unionAllObjs' (typecheckedAllObjs' : veobjMetas),
        PowersetTo 2 H.empty unionAllObjs' unionAllObjsPs'
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
    tryArrow :: (MetaDat m, Show m) => ObjArr Expr m -> Type
    tryArrow oa = if H.keysSet ptArgs `isSubsetOf` H.keysSet (exprAppliedArgsMap $ oaObjExpr oa)
      then UnionType $ joinUnionType $ map addArg $ S.toList $ S.difference (H.keysSet $ exprAppliedArgsMap $ oaObjExpr oa) (H.keysSet ptArgs)
      else bottomType
    addArg arg = partial{ptArgs=H.insertWith (unionTypes feClassGraph) arg topType ptArgs}
inferArgFromPartial _ _ = bottomType

mkReachesEnv :: FEnv -> TypeVarArgEnv -> TypeCheckResult (ReachesEnv ())
mkReachesEnv env@FEnv{feClassGraph, feUnionAllObjs, feVTypeGraph, feTTypeGraph} vaenv = do
  (SType unionAll _ _) <- descriptor env feUnionAllObjs
  feVTypeGraph' <- forM feVTypeGraph $ \objArrs -> do
    forM objArrs $ \voa -> do
      objUb <- pointUb env (getExprMeta $ oaObjExpr voa)
      soa <- showObjArr env voa
      let soa' = mapMetaObjArr clearMetaDat Nothing soa
      let soa'' = mapOAObjExpr (exprWithMetaType objUb) soa'
      return soa''
  let argTypeGraph = H.mapWithKey (\argName argType -> [ObjArr (Just (GuardExpr (Value (emptyMetaT $ typeVal $ PTypeName argName) argName) Nothing)) ArgObj Nothing [] (Nothing, emptyMetaT (substituteWithVarArgEnv vaenv argType))]) (snd $ splitVarArgEnv vaenv)
  let argClassGraph = classGraphFromObjs (concat $ H.elems argTypeGraph)
  let typeGraph = unionsWith (++) [argTypeGraph, feVTypeGraph', feTTypeGraph]
  return $ ReachesEnv (mergeClassGraphs argClassGraph feClassGraph) unionAll vaenv typeGraph

arrowConstrainUbs :: FEnv -> VConstraint -> Type -> VarMeta -> Type -> VarMeta -> TypeCheckResult (Type, Type)
arrowConstrainUbs env@FEnv{feUnionAllObjs} con (TopType []) srcM dest@UnionType{} destM = do
  unionPnt <- descriptor env feUnionAllObjs
  case unionPnt of
    (SType unionUb@UnionType{} _ _) -> do
      (src', dest') <- arrowConstrainUbs env con unionUb srcM dest destM
      return (src', dest')
    _ -> return (topType, dest)
arrowConstrainUbs _ _ (TopType []) _ dest _ = return (topType, dest)
arrowConstrainUbs _ _ (TopType _) _ _ _ = undefined
arrowConstrainUbs env con src@(TypeVar v _) srcM dest destM = do
  src' <- resolveTypeVar v con
  (_, cdest) <- arrowConstrainUbs env con (getMetaType src') srcM dest destM
  return (src, cdest)
arrowConstrainUbs env@FEnv{feClassGraph} con (UnionType srcPartials) _ dest _ = do
  let srcPartialList = splitUnionType srcPartials
  vaenv <- descriptorConVaenv env con
  reachesEnv <- mkReachesEnv env (fmap stypeAct vaenv)
  srcPartialList' <- mapM (resToTypeCheck . rootReachesPartial reachesEnv) srcPartialList
  let partialMap = H.fromList srcPartialList'
  let (srcPartialList'', destByPartial) = unzip $ H.toList partialMap
  let srcPartials' = joinUnionType srcPartialList''
  let destByGraph = unionAllTypes feClassGraph $ fmap (unionReachesTree feClassGraph) destByPartial
  let dest' = intersectTypes feClassGraph dest destByGraph
  return (compactType feClassGraph $ UnionType srcPartials', dest')
