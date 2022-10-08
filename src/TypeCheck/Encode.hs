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

import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.TypeGraph (addUnionObjToEnv)
import           Utils               (fst3)

-- represents how a specified variables corresponds to the known types.
-- It could be a lower bound, upper bound, or exact bound
data TypeBound = BUpper | BLower | BEq
  deriving (Eq)

makeBaseFEnv :: ClassGraph -> FEnv
makeBaseFEnv classGraph = FEnv{
  fePnts = IM.empty,
  feCons = [],
  feUnionAllObjs = Meta TopType Nothing (VarMetaDat 0 Nothing),
  feVTypeGraph = H.empty,
  feTTypeGraph = H.empty,
  feUpdatedDuringEpoch = False,
  feClassGraph = classGraph,
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
  return (mapMetaDat (\_ -> VarMetaDat p obj) m, env')

-- TODO: This might reverse the list to return.
mapMWithFEnv :: FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> [a] -> TypeCheckResult ([b], FEnv)
mapMWithFEnv env f = foldM f' ([], env)
  where f' (acc, e) a = do
          (b, e') <- f e a
          return (b:acc, e')

mapMWithFEnvMaybe :: FEnv -> (FEnv -> a -> TypeCheckResult (b, FEnv)) -> Maybe a -> TypeCheckResult (Maybe b, FEnv)
mapMWithFEnvMaybe env _ Nothing = return (Nothing, env)
mapMWithFEnvMaybe env f (Just a) = do
  (b, env') <- f env a
  return (Just b, env')

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
fromExpr _ obj env (CExpr m (CInt i)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant int " ++ show i)
  return (CExpr m' (CInt i), addConstraints env' [EqualsKnown m' intType])
fromExpr _ obj env (CExpr m (CFloat f)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant float " ++ show f)
  return (CExpr m' (CFloat f), addConstraints env' [EqualsKnown m' floatType])
fromExpr _ obj env (CExpr m (CStr s)) = do
  (m', env') <- fromMeta env BUpper obj m ("Constant str " ++ s)
  return (CExpr m' (CStr s), addConstraints env' [EqualsKnown m' strType])
fromExpr _ obj env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Value " ++ name)
  lookupVal <- fLookup env2 obj name
  lookupConstraints <- case lookupVal of
    DefVar lookupM      -> return [EqPoints m' lookupM]
    DefKnown lookupType -> return [BoundedByKnown m' lookupType]
  return (Value m' name, addConstraints env2 lookupConstraints)
fromExpr argMetaMap obj env1 (Arg m name) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Arg " ++ name)
  let varM = Meta (TypeVar $ TVArg name) (getMetaPos m) emptyMetaDat
  (varM', env3) <- fromMeta env2 BUpper obj varM $ "ArgVar " ++ name
  case suffixLookupInDict name argMetaMap of
    Nothing -> error $ printf "Could not find arg %s with argMetaMap %s and obj %s" name (show argMetaMap) (show obj)
    Just lookupArg -> return (Arg varM' name, addConstraints env3 [EqPoints m' lookupArg])
fromExpr _ obj env1 (HoleExpr m hole) = do
  (m', env2) <- fromMeta env1 BUpper obj m ("Hole " ++ show hole)
  return (HoleExpr m' hole, env2)
fromExpr argMetaMap obj env1 (TupleApply m (baseM, baseExpr) (TupleArgIO argM argName argExpr)) = do
  (m', env2) <- fromMeta env1 BUpper obj m $ printf "TupleApply %s(%s = %s) Meta" (show baseExpr) argName (show argExpr)
  (baseM', env3) <- fromMeta env2 BUpper obj baseM $ printf "TupleApply %s(%s = %s) BaseMeta" (show baseExpr) argName (show argExpr)
  (baseExpr', env4) <- fromExpr argMetaMap obj env3 baseExpr
  (argExpr', env5) <- fromExpr argMetaMap obj env4 argExpr
  (argM', env6) <- fromMeta env5 BUpper obj argM $ printf "TupleApply %s(%s = %s) ArgMeta" (show baseExpr) argName (show argExpr)
  let constraints = [ArrowTo (getExprMeta baseExpr') baseM',
                     AddArg (baseM', argName) m',
                     BoundedByObjs m',
                     ArrowTo (getExprMeta argExpr') argM',
                     PropEq (m', argName) argM'
                    ]
  let env7 = addConstraints env6 constraints
  return (TupleApply m' (baseM', baseExpr') (TupleArgIO argM' argName argExpr'), env7)
fromExpr argMetaMap obj env1 (TupleApply m (baseM, baseExpr) (TupleArgO argM argExpr)) = do
  (m', env2) <- fromMeta env1 BUpper obj m $ printf "TupleApplyInfer %s(%s) Meta" (show baseExpr) (show argExpr)
  (baseM', env3) <- fromMeta env2 BUpper obj baseM $ printf "TupleApplyInfer %s(%s) BaseMeta" (show baseExpr) (show argExpr)
  (baseExpr', env4) <- fromExpr argMetaMap obj env3 baseExpr
  (argExpr', env5) <- fromExpr argMetaMap obj env4 argExpr
  (argM', env6) <- fromMeta env5 BUpper obj argM $ printf "TupleApplyInfer %s(%s) ArgMeta" (show baseExpr) (show argExpr)
  let constraints = [ArrowTo (getExprMeta baseExpr') baseM',
                     AddInferArg baseM' m',
                     BoundedByObjs m',
                     ArrowTo (getExprMeta argExpr') argM'
                    ]
  let env7 = addConstraints env6 constraints
  return (TupleApply m' (baseM', baseExpr') (TupleArgO argM' argExpr'), env7)
fromExpr _ _ _ (TupleApply _ _ TupleArgI{}) = error "Unexpected TupleArgI in encode fromExpr"
fromExpr oArgs obj env1 (VarApply m baseExpr varName varVal) = do
  let baseName = printf "VarApply %s<%s = %s>" (show baseExpr) varName (show varVal) :: String
  (m', env2) <- fromMeta env1 BUpper obj m $ printf "%s Meta" baseName
  (baseExpr', env3) <- fromExpr oArgs obj env2 baseExpr
  (varVal', env4) <- fromMeta env3 BUpper obj varVal $ printf "%s val" baseName
  let constraints = [ArrowTo (getExprMeta baseExpr') m',
                     BoundedByObjs m',
                     VarEq (m', varName) varVal'
                    ]
  let env5 = addConstraints env4 constraints
  return (VarApply m' baseExpr' varName varVal', env5)

fromGuard :: VArgMetaMap -> Maybe VObject -> FEnv -> PGuard -> TypeCheckResult (VGuard, FEnv)
fromGuard argMetaMap obj env1 (IfGuard expr) =  do
  (expr', env2) <- fromExpr argMetaMap obj env1 expr
  let (bool, env3) = fresh env2 $ TypeCheckResult [] $ SType boolType bottomType "ifGuardBool"
  let bool' = Meta boolType (labelPos "bool" $ getMetaPos $ getExprMeta expr) (VarMetaDat bool obj)
  return (IfGuard expr', addConstraints env3 [ArrowTo (getExprMeta expr') bool'])
fromGuard _ _ env ElseGuard = return (ElseGuard, env)
fromGuard _ _ env NoGuard = return (NoGuard, env)

fromArrow :: VObject -> FEnv -> PArrow -> TypeCheckResult (VArrow, FEnv)
fromArrow obj@(Object _ _ objVars _ _ _) env1 (Arrow m aguard maybeExpr) = do
  -- User entered type is not an upper bound, so start with TopType always. The true use of the user entered type is that the expression should have an arrow that has a reachesTree cut that is within the user entered type.
  let jobj = Just obj
  (mUserReturn', env2) <- fromMeta env1 BUpper jobj m (printf "Specified result from %s" (show $ objPath obj))
  let argMetaMap = formArgMetaMap obj
  (aguard', env3) <- fromGuard argMetaMap jobj env2 aguard
  case maybeExpr of
    Just expr -> do
      (m', env4) <- fromMeta env3 BUpper jobj (Meta TopType (labelPos "res" $ getMetaPos m) emptyMetaDat) $ printf "Arrow result from %s" (show $ objPath obj)
      (vExpr, env5) <- fromExpr argMetaMap jobj env4 expr
      let env6 = case metaTypeVar m of
            Just (TVVar typeVarName) -> case suffixLookupInDict typeVarName objVars of
              Just varM -> addConstraints env5 [ArrowTo (getExprMeta vExpr) varM]
              Nothing -> error "unknown type fromArrow"
            Just TVArg{} -> error "Bad TVArg in fromArrow"
            Nothing -> addConstraints env5 [ArrowTo (getExprMeta vExpr) m', ArrowTo (getExprMeta vExpr) mUserReturn']
      let arrow' = Arrow m' aguard' (Just vExpr)
      let env7 = fAddVTypeGraph env6 (objPath obj) (obj, arrow')
      return (arrow', env7)
    Nothing -> do
      let arrow' = Arrow mUserReturn' aguard' Nothing
      let env4 = fAddVTypeGraph env3 (objPath obj) (obj, arrow')
      return (arrow', env4)

fromObjectMap :: FEnv -> (VObject, [PCompAnnot], Maybe PArrow) -> TypeCheckResult ((VObject, [VCompAnnot], Maybe VArrow), FEnv)
fromObjectMap env1 (obj, annots, arrow) = do
  (arrow', env2) <- mapMWithFEnvMaybe env1 (fromArrow obj) arrow
  let argMetaMap = formArgMetaMap obj
  (annots', env3) <- mapMWithFEnv env2 (fromExpr argMetaMap (Just obj)) annots
  return ((obj, annots', arrow'), env3)

fromObjVar :: VarMeta -> String -> FEnv -> (TypeVarName, PreMeta) -> TypeCheckResult ((TypeVarName, VarMeta), FEnv)
fromObjVar oM prefix env1 (varName, m) = do
  (m', env2) <- fromMeta env1 BUpper Nothing m (prefix ++ "." ++ varName)
  let env3 = addConstraints env2 [VarEq (oM, varName) m']
  return ((varName, m'), env3)

addObjArg :: VObject -> VarMeta -> String -> H.HashMap TypeVarName VarMeta -> FEnv -> (TypeName, PObjArg) -> TypeCheckResult ((TypeName, VObjArg), FEnv)
addObjArg fakeObj oM prefix varMap env (n, (m, maybeSubObj)) = do
  let prefix' = prefix ++ "." ++ n
  -- requires a fakeObj to pull the type variables from
  let argBound = case maybeSubObj of
        Just{}  -> BUpper
        Nothing -> BEq
  (m', env2) <- fromMeta env argBound (Just fakeObj) m prefix'
  let env3 = addConstraints env2 [PropEq (oM, n) m', BoundedByObjs m']
  let env4 = case suffixLookupInDict n varMap of
        Just varM -> addConstraints env3 [EqPoints m' varM]
        Nothing   -> env3
  case maybeSubObj of
    Just subObj -> do
      (subObj'@Object{deprecatedObjM=subM}, env5) <- fromObject prefix' True env4 subObj
      return ((n, (m', Just subObj')), addConstraints env5 [ArrowTo subM m'])
    Nothing -> return ((n, (m', Nothing)), env4)

-- Remove constraints on args from the main meta in an Object.
-- Those constraints are still applied by constraints with the args.
-- However, typeVars only work correctly from the arg version and not the main meta
-- Likewise, replace the type vars equal to vars with top type for now
clearMetaArgTypes :: PreMeta -> PreMeta
clearMetaArgTypes (Meta (UnionType partials) pos md) = Meta (UnionType $ joinUnionType $ map clearPartialTypeArgs $ splitUnionType partials) (labelPos "clear" pos) md
  where
    clearPartialTypeArgs partial@PartialType{ptArgs} = partial{ptArgs=fmap (const TopType) ptArgs}
clearMetaArgTypes p = p

fromObject :: String -> Bool -> FEnv -> PObject -> TypeCheckResult (VObject, FEnv)
fromObject prefix isObjArg env (Object m basis vars args doc path) = do
  let prefix' = prefix ++ "." ++ path
  (m', env1) <- fromMeta env BUpper Nothing (clearMetaArgTypes m) prefix'
  (vars', env2) <- mapMWithFEnvMapWithKey env1 (fromObjVar m' prefix') vars
  let fakeObjForArgs = Object m' basis vars' H.empty doc path
  (args', env3) <- mapMWithFEnvMapWithKey env2 (addObjArg fakeObjForArgs m' prefix' vars') args
  let obj' = Object m' basis vars' args' doc path
  (objValue, env4) <- fromMeta env3 BUpper (Just obj') (Meta (singletonType (partialVal (PTypeName path))) (labelPos "objValue" $ getMetaPos m) emptyMetaDat) ("objValue" ++ path)
  let env5 = fInsert env4 path (DefVar objValue)
  let env6 = addConstraints env5 [BoundedByObjs m' | isObjArg]
  let env7 = addConstraints env6 [BoundedByKnown m' (singletonType (PartialType (PTypeName path) (fmap (const TopType) vars) H.empty (fmap (const TopType) args) [] PtArgExact)) | basis == FunctionObj || basis == PatternObj]
  return (obj', env7)

-- Add all of the objects first for various expressions that call other top level functions
fromObjects :: FEnv -> (PObject, [PCompAnnot], Maybe PArrow) -> TypeCheckResult ((VObject, [PCompAnnot], Maybe PArrow), FEnv)
fromObjects env (obj, annots, arrow) = do
  (obj', env1) <- fromObject "Object" False env obj
  return ((obj', annots, arrow), env1)

fromPrgm :: FEnv -> (PPrgm, [VObject]) -> TypeCheckResult (VPrgm, FEnv)
fromPrgm env1 ((objMap1, classGraph, annots), vobjs) = do
  let objMap2 = zipWith (\(_, oans, arrows) vobj -> (vobj, oans, arrows)) (reverse objMap1) vobjs
  (objMap3, env2) <- mapMWithFEnv env1 fromObjectMap objMap2
  (annots', env3) <- mapMWithFEnv env2 (fromExpr H.empty Nothing) annots
  return ((objMap3, classGraph, annots'), env3)

-- Add all of the objects first for various expressions that call other top level functions, from all programs
prepObjPrgm :: FEnv -> PPrgm -> TypeCheckResult ((PPrgm, [VObject]), FEnv)
prepObjPrgm env1 pprgm@(objMap1, _, _) = do
  (objMap2, env2) <- mapMWithFEnv env1 fromObjects objMap1
  return ((pprgm, map fst3 objMap2), env2)

addTypeGraphArrow :: TObject -> FEnv -> TArrow -> TypeCheckResult ((), FEnv)
addTypeGraphArrow obj env arr = return ((), fAddTTypeGraph env (objPath obj) (obj, arr))

addTypeGraphObjects :: FEnv -> TObjectMapItem -> TypeCheckResult ((), FEnv)
addTypeGraphObjects env (obj, _, arrow) = do
  let objValue = singletonType (partialVal (PTypeName (objPath obj)))
  let env' = fInsert env (objPath obj) (DefKnown objValue)
  (_, env'') <- mapMWithFEnvMaybe env' (addTypeGraphArrow obj) arrow
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
  let env5 = addUnionObjToEnv env4 vjoinObjMap tjoinObjMap
  return (vprgms, env5)
