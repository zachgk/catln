--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Encode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module encodes before type checking to extract all of the
-- types into isolated variables and all of the constraints that
-- relate the variables together. It prepares for typechecking.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module TypeCheck.Encode where

import           Control.Monad
import qualified Data.HashMap.Strict as H
import           Data.Hashable       (Hashable)
import qualified Data.IntMap.Lazy    as IM
import           Prelude             hiding (unzip)

import           Parser.Syntax       (emptyMetaN)
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.TypeGraph (buildTypeEnv)

-- represents how a specified variables corresponds to the known types.
-- It could be a lower bound, upper bound, or exact bound
data TypeBound = BUpper | BLower | BEq
  deriving (Eq)

makeBaseFEnv :: ClassMap -> FEnv
makeBaseFEnv classMap = FEnv{
  fePnts = IM.empty,
  feCons = [],
  feUnionAllObjs = VarMeta 0 emptyMetaN Nothing,
  feUnionTypeObjs = VarMeta 0 emptyMetaN Nothing,
  feVTypeGraph = H.empty,
  feTTypeGraph = H.empty,
  feClassMap = classMap,
  feDefMap = H.empty,
  feTrace = [[]]
  }

fromMeta :: FEnv -> TypeBound -> Maybe VObject -> PreMeta -> String -> TypeCheckResult (VarMeta, FEnv)
fromMeta env bound obj m description  = do
  let tp = getMetaType m
  let (p, env') = case bound of
        BUpper -> fresh env (TypeCheckResult [] $ SType tp bottomType description)
        BLower -> fresh env (TypeCheckResult [] $ SType TopType tp description)
        BEq -> fresh env (TypeCheckResult [] $ SType tp tp description)
  return (VarMeta p m obj, env')

-- TODO: This might reverse the list to return.
mapMWithFEnv :: FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> [a] -> TypeCheckResult ([b], FEnv)
mapMWithFEnv env f = foldM f' ([], env)
  where f' (acc, e) a = do
          (b, e') <- f e a
          return (b:acc, e')

mapMWithFEnvMap :: (Eq k, Hashable k) => FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> H.HashMap k a -> TypeCheckResult (H.HashMap k b, FEnv)
mapMWithFEnvMap env f hmap = do
  (res, env2) <- mapMWithFEnv env f' (H.toList hmap)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      (b, e2) <- f e a
      return ((k, b), e2)

mapMWithFEnvMapWithKey :: (Eq k, Hashable k) => FEnv -> (FEnv -> (k, a) -> TypeCheckResult ((k, b), FEnv)) -> H.HashMap k a -> TypeCheckResult (H.HashMap k b, FEnv)
mapMWithFEnvMapWithKey env f hmap = do
  (res, env2) <- mapMWithFEnv env f' (H.toList hmap)
  return (H.fromList res, env2)
  where
    f' e (k, a) = do
      ((k2, b), e2) <- f e (k, a)
      return ((k2, b), e2)

fromExpr :: VArgMetaMap -> Maybe VObject -> FEnv -> PExpr -> TypeCheckResult (VExpr, FEnv)
fromExpr _ obj env (ICExpr m (CInt i)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant int " ++ show i)
  return (ICExpr m' (CInt i), addConstraints env' [EqualsKnown m' intType])
fromExpr _ obj env (ICExpr m (CFloat f)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant float " ++ show f)
  return (ICExpr m' (CFloat f), addConstraints env' [EqualsKnown m' floatType])
fromExpr _ obj env (ICExpr m (CStr s)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant str " ++ s)
  return (ICExpr m' (CStr s), addConstraints env' [EqualsKnown m' strType])
fromExpr _ obj env1 (IValue m name) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Value " ++ name)
  lookupVal <- fLookup env2 name
  lookupConstraints <- case lookupVal of
    DefVar lookupM      -> return [EqPoints m' lookupM]
    DefKnown lookupType -> return [BoundedByKnown m' lookupType]
  return (IValue m' name, addConstraints env2 lookupConstraints)
fromExpr objArgs obj env1 (IArg m name) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Arg " ++ name)
  let varM = PreTyped (TypeVar $ TVArg name) (getMetaPos m)
  (varM', env3) <- fromMeta env2 BUpper obj varM $ "ArgVar " ++ name
  case H.lookup name objArgs of
    Nothing -> error $ printf "Could not find arg %s with objArgs %s and obj %s" name (show objArgs) (show obj)
    Just lookupArg -> return (IArg varM' name, addConstraints env3 [EqPoints m' lookupArg])
fromExpr objArgs obj env1 (ITupleApply m (baseM, baseExpr) (Just argName) argExpr) = do
  (m', env2) <- fromMeta env1 BUpper obj m $ printf "TupleApply %s(%s = %s) Meta" (show baseExpr) argName (show argExpr)
  (baseM', env3) <- fromMeta env2 BUpper obj baseM $ printf "TupleApply %s(%s = %s) BaseMeta" (show baseExpr) argName (show argExpr)
  (baseExpr', env4) <- fromExpr objArgs obj env3 baseExpr
  (argExpr', env5) <- fromExpr objArgs obj env4 argExpr
  let (convertExprMeta, env6) = fresh env5 (TypeCheckResult [] $ SType TopType bottomType "Tuple converted expr meta")
  let convertExprMeta' = VarMeta convertExprMeta (PreTyped TopType (labelPos "convert" $ getMetaPos $ getExprMeta argExpr)) obj
  let constraints = [ArrowTo (getExprMeta baseExpr') baseM',
                     AddArg (baseM', argName) m',
                     BoundedByObjs BoundAllObjs m',
                     ArrowTo (getExprMeta argExpr') convertExprMeta',
                     PropEq (m', argName) convertExprMeta'
                    ]
  let env7 = addConstraints env6 constraints
  return (ITupleApply m' (baseM', baseExpr') (Just argName) argExpr', env7)
fromExpr objArgs obj env1 (ITupleApply m (baseM, baseExpr) Nothing argExpr) = do
  (m', env2) <- fromMeta env1 BUpper obj m $ printf "TupleApplyInfer %s(%s) Meta" (show baseExpr) (show argExpr)
  (baseM', env3) <- fromMeta env2 BUpper obj baseM $ printf "TupleApplyInfer %s(%s) BaseMeta" (show baseExpr) (show argExpr)
  (baseExpr', env4) <- fromExpr objArgs obj env3 baseExpr
  (argExpr', env5) <- fromExpr objArgs obj env4 argExpr
  let (convertExprMeta, env6) = fresh env5 (TypeCheckResult [] $ SType TopType bottomType "Tuple converted expr meta")
  let convertExprMeta' = VarMeta convertExprMeta (PreTyped TopType (labelPos "convert" $ getMetaPos $ getExprMeta argExpr)) obj
  let constraints = [ArrowTo (getExprMeta baseExpr') baseM',
                     AddInferArg baseM' m',
                     BoundedByObjs BoundAllObjs m',
                     ArrowTo (getExprMeta argExpr') convertExprMeta'
                    ]
  let env7 = addConstraints env6 constraints
  return (ITupleApply m' (baseM', baseExpr') Nothing argExpr', env7)

fromGuard :: VArgMetaMap -> Maybe VObject -> FEnv -> PGuard -> TypeCheckResult (VGuard, FEnv)
fromGuard objArgs obj env1 (IfGuard expr) =  do
  (expr', env2) <- fromExpr objArgs obj env1 expr
  let (bool, env3) = fresh env2 $ TypeCheckResult [] $ SType boolType bottomType "ifGuardBool"
  let bool' = VarMeta bool (PreTyped boolType (labelPos "bool" $ getMetaPos $ getExprMeta expr)) obj
  return (IfGuard expr', addConstraints env3 [ArrowTo (getExprMeta expr') bool'])
fromGuard _ _ env ElseGuard = return (ElseGuard, env)
fromGuard _ _ env NoGuard = return (NoGuard, env)

fromArrow :: VObject -> FEnv -> PArrow -> TypeCheckResult (VArrow, FEnv)
fromArrow obj@(Object _ _ objName objVars _ _) env1 (Arrow m annots aguard maybeExpr) = do
  -- User entered type is not an upper bound, so start with TopType always. The true use of the user entered type is that the expression should have an arrow that has a reachesTree cut that is within the user entered type.
  let jobj = Just obj
  (mUserReturn', env2) <- fromMeta env1 BUpper jobj m (printf "Specified result from %s" (show objName))
  let argMetaMap = formArgMetaMap obj
  (annots', env3) <- mapMWithFEnv env2 (fromExpr argMetaMap jobj) annots
  (aguard', env4) <- fromGuard argMetaMap jobj env3 aguard
  case maybeExpr of
    Just expr -> do
      (m', env5) <- fromMeta env4 BUpper jobj (PreTyped TopType (labelPos "res" $ getMetaPos m)) $ printf "Arrow result from %s" (show objName)
      (vExpr, env6) <- fromExpr argMetaMap jobj env5 expr
      let env7 = case metaTypeVar m of
            Just (TVVar typeVarName) -> case H.lookup typeVarName objVars of
              Just varM -> addConstraints env6 [ArrowTo (getExprMeta vExpr) varM]
              Nothing -> error "unknown type fromArrow"
            Just TVArg{} -> error "Bad TVArg in fromArrow"
            Nothing -> addConstraints env6 [ArrowTo (getExprMeta vExpr) m', ArrowTo (getExprMeta vExpr) mUserReturn']
      let arrow' = Arrow m' annots' aguard' (Just vExpr)
      let env8 = fAddVTypeGraph env7 objName (obj, arrow')
      return (arrow', env8)
    Nothing -> do
      let arrow' = Arrow mUserReturn' annots' aguard' Nothing
      let env5 = fAddVTypeGraph env4 objName (obj, arrow')
      return (arrow', env5)

fromObjectMap :: FEnv -> (VObject, [PArrow]) -> TypeCheckResult ((VObject, [VArrow]), FEnv)
fromObjectMap env1 (obj, arrows) = do
  (arrows', env2) <- mapMWithFEnv env1 (fromArrow obj) arrows
  return ((obj, arrows'), env2)

fromObjVar :: VarMeta -> String -> FEnv -> (TypeVarName, PreMeta) -> TypeCheckResult ((TypeVarName, VarMeta), FEnv)
fromObjVar objM prefix env1 (varName, m) = do
  (m', env2) <- fromMeta env1 BUpper Nothing m (prefix ++ "." ++ varName)
  let env3 = addConstraints env2 [VarEq (objM, varName) m']
  return ((varName, m'), env3)

addObjArg :: VObject -> VarMeta -> String -> H.HashMap TypeVarName VarMeta -> FEnv -> (TypeName, PObjArg) -> TypeCheckResult ((TypeName, VObjArg), FEnv)
addObjArg fakeObj objM prefix varMap env (n, (m, maybeSubObj)) = do
  let prefix' = prefix ++ "." ++ n
  -- requires a fakeObj to pull the type variables from
  let argBound = case maybeSubObj of
        Just{}  -> BUpper
        Nothing -> BEq
  (m', env2) <- fromMeta env argBound (Just fakeObj) m prefix'
  let env3 = addConstraints env2 [PropEq (objM, n) m', BoundedByObjs BoundAllObjs m']
  let env4 = case H.lookup n varMap of
        Just varM -> addConstraints env3 [EqPoints m' varM]
        Nothing   -> env3
  case maybeSubObj of
    Just subObj -> do
      (subObj'@Object{objM=subM}, env5) <- fromObject prefix' True env4 subObj
      return ((n, (m', Just subObj')), addConstraints env5 [ArrowTo subM m'])
    Nothing -> return ((n, (m', Nothing)), env4)

-- Remove constraints on args from the main meta in an Object.
-- Those constraints are still applied by constraints with the args.
-- However, typeVars only work correctly from the arg version and not the main meta
-- Likewise, replace the type vars equal to vars with top type for now
clearMetaArgTypes :: PreMeta -> PreMeta
clearMetaArgTypes (PreTyped (UnionType partials) pos) = PreTyped (UnionType $ joinPartialLeafs $ map clearPartialTypeArgs $ splitPartialLeafs partials) (labelPos "clear" pos)
  where
    clearPartialTypeArgs partial@PartialType{ptArgs} = partial{ptArgs=fmap (const TopType) ptArgs}
clearMetaArgTypes p = p

fromObject :: String -> Bool -> FEnv -> PObject -> TypeCheckResult (VObject, FEnv)
fromObject prefix isObjArg env (Object m basis name vars args doc) = do
  let prefix' = prefix ++ "." ++ name
  (m', env1) <- fromMeta env BUpper Nothing (clearMetaArgTypes m) prefix'
  (vars', env2) <- mapMWithFEnvMapWithKey env1 (fromObjVar m' prefix') vars
  let fakeObjForArgs = Object m' basis name vars' H.empty doc
  (args', env3) <- mapMWithFEnvMapWithKey env2 (addObjArg fakeObjForArgs m' prefix' vars') args
  let obj' = Object m' basis name vars' args' doc
  (objValue, env4) <- fromMeta env3 BUpper (Just obj') (PreTyped (singletonType (PartialType (PTypeName name) H.empty H.empty H.empty PtArgExact)) (labelPos "objValue" $ getMetaPos m)) ("objValue" ++ name)
  let env5 = fInsert env4 name (DefVar objValue)
  let env6 = addConstraints env5 [BoundedByObjs BoundAllObjs m' | isObjArg]
  let env7 = addConstraints env6 [BoundedByKnown m' (singletonType (PartialType (PTypeName name) (fmap (const TopType) vars) H.empty (fmap (const TopType) args) PtArgExact)) | basis == FunctionObj || basis == PatternObj]
  return (obj', env7)

-- Add all of the objects first for various expressions that call other top level functions
fromObjects :: FEnv -> (PObject, [PArrow]) -> TypeCheckResult ((VObject, [PArrow]), FEnv)
fromObjects env (obj, arrows) = do
  (obj', env1) <- fromObject "Object" False env obj
  return ((obj', arrows), env1)

fromPrgm :: FEnv -> (PPrgm, [VObject]) -> TypeCheckResult (VPrgm, FEnv)
fromPrgm env1 ((objMap1, classMap, annots), vobjs) = do
  let objMap2 = zipWith (\(_, arrows) vobj -> (vobj, arrows)) (reverse objMap1) vobjs
  (objMap3, env2) <- mapMWithFEnv env1 fromObjectMap objMap2
  (annots', env3) <- mapMWithFEnv env2 (fromExpr H.empty Nothing) annots
  return ((objMap3, classMap, annots'), env3)

-- Add all of the objects first for various expressions that call other top level functions, from all programs
prepObjPrgm :: FEnv -> PPrgm -> TypeCheckResult ((PPrgm, [VObject]), FEnv)
prepObjPrgm env1 pprgm@(objMap1, _, _) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObjects objMap1
  return ((pprgm, map fst objMap2), env2)

addTypeGraphArrow :: TObject -> FEnv -> TArrow -> TypeCheckResult ((), FEnv)
addTypeGraphArrow obj@Object{objName} env arr = return ((), fAddTTypeGraph env objName (obj, arr))

addTypeGraphObjects :: FEnv -> (TObject, [TArrow]) -> TypeCheckResult ((), FEnv)
addTypeGraphObjects env (obj@Object{objName}, arrows) = do
  let objValue = singletonType (PartialType (PTypeName objName) H.empty H.empty H.empty PtArgExact)
  let env' = fInsert env objName (DefKnown objValue)
  (_, env'') <- mapMWithFEnv env' (addTypeGraphArrow obj) arrows
  return ((), env'')

addTypeGraphPrgm :: FEnv -> TPrgm -> TypeCheckResult ((), FEnv)
addTypeGraphPrgm env (objMap, _, _) = do
  (_, env') <- mapMWithFEnv env addTypeGraphObjects objMap
  return ((), env')

fromPrgms :: FEnv -> [PPrgm] -> [TPrgm] -> TypeCheckResult ([VPrgm], FEnv)
fromPrgms env1 pprgms tprgms = do
  (_, env2) <- mapMWithFEnv env1 addTypeGraphPrgm tprgms
  (pprgmsWithVObjs, env3) <- mapMWithFEnv env2 prepObjPrgm pprgms
  (vprgms, env4) <- mapMWithFEnv env3 fromPrgm pprgmsWithVObjs
  let (vjoinObjMap, _, _) = mergePrgms vprgms
  let (tjoinObjMap, _, _) = mergePrgms tprgms
  let env5 = buildTypeEnv env4 vjoinObjMap tjoinObjMap
  return (vprgms, env5)
