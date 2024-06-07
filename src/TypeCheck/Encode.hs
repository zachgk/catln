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
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TypeCheck.Encode where

import           Control.Monad
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Lazy    as IM
import           Prelude             hiding (unzip)

import           Data.Bifunctor      (Bifunctor (first))
import           Data.Maybe
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import           TypeCheck.TypeUtils (addUnionObjToEnv)
import           Utils

-- represents how a specified variables corresponds to the known types.
-- It could be a bound on actual, required, or both
data TypeBound = BAct | BReq | BActReq
  deriving (Eq)

data EncodeState
  = EncodeOut !(Maybe VObject) -- ^ Used for outputs including in definitions (with object) and global annots (without obj)
  | EncodeIn -- ^ Used for inputs
  deriving (Eq, Ord, Show)

isEncodeOut :: EncodeState -> Bool
isEncodeOut EncodeOut{} = True
isEncodeOut EncodeIn{}  = False

asEncodeIn :: EncodeState -> EncodeState
asEncodeIn (EncodeOut _)  = EncodeIn
asEncodeIn est@EncodeIn{} = est

encodeObj :: EncodeState -> Maybe VObject
encodeObj (EncodeOut obj) = obj
encodeObj EncodeIn{}      = Nothing

makeBaseFEnv :: PPrgm -> FEnv
makeBaseFEnv prgm = FEnv{
  fePnts = IM.empty,
  feConsDats = [],
  feCons = [],
  feUnionAllObjs = Meta topType Nothing (VarMetaDat (Just 0) Nothing),
  feVTypeGraph = H.empty,
  feTTypeGraph = H.empty,
  feUpdatedDuringEpoch = False,
  feTypeEnv = mkTypeEnv prgm,
  feTrace = [[]]
  }

mkVarMetaDat :: EncodeState -> Pnt -> VarMetaDat
mkVarMetaDat (EncodeOut obj) p = VarMetaDat (Just p) obj
mkVarMetaDat EncodeIn p        = VarMetaDat (Just p) Nothing

fromMeta :: FEnv -> TypeBound -> EncodeState -> PreMeta -> String -> TypeCheckResult (VarMeta, FEnv)
fromMeta env bound est m description  = do
  let tp = getMetaType m
  let (p, env') = case bound of
        BAct    -> fresh env (TypeCheckResult [] $ SType tp topType description)
        BReq    -> fresh env (TypeCheckResult [] $ SType topType tp description)
        BActReq -> fresh env (TypeCheckResult [] $ SType tp tp description)
  return (mapMetaDat (\_ -> mkVarMetaDat est p) m, env')

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

fromExpr :: EncodeState -> FEnv -> PExpr -> TypeCheckResult (VExpr, FEnv)
fromExpr est env (CExpr m c) = do
  (m', env') <- fromMeta env BAct est m ("Constant " ++ show c)
  return (CExpr m' c, addConstraints env' [EqualsKnown 3 m' (constantType c)])
fromExpr est@EncodeOut{} env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BAct est m ("Out Val " ++ name)
  return (Value m' name, addConstraints env2 [BoundedByKnown 4 m' (relTypeVal name), BoundedByObjs 4 m' topType])
fromExpr est@EncodeIn{} env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BAct est m ("In Val " ++ name)
  return (Value m' name, addConstraints env2 [BoundedByKnown 5 m' (relTypeVal name)])
fromExpr est env1 (HoleExpr m hole) = do
  (m', env2) <- fromMeta env1 BAct est m ("Hole " ++ show hole)
  return (HoleExpr m' hole, env2)
fromExpr est env1 (AliasExpr base alias) = do
  (base', env2) <- fromExpr est env1 base
  (alias', env3) <- fromExpr est env2 alias
  let env4 = addConstraints env3 [BoundedByKnown 6 (getExprMeta base') (TypeVar (TVArg $ inExprSingleton alias) TVInt)]
  return (AliasExpr base' alias', env4)
fromExpr est env1 (EWhere base cond) = do
  (base', env2) <- fromExpr est env1 base
  let condEst = case est of
        EncodeIn -> EncodeOut Nothing
        _        -> est
  (cond', env3) <- fromExpr condEst env2 cond
  let (bool, env4) = fresh env3 $ TypeCheckResult [] $ SType topType boolType "ifGuardBool"
  let bool' = Meta boolType (labelPos "bool" $ getMetaPos $ getExprMeta cond) (mkVarMetaDat est bool)
  let env5 = addConstraints env4 [ArrowTo 30 (getExprMeta cond') bool']
  return (EWhere base' cond', env5)
fromExpr est env1 (TupleApply m (baseM, baseExpr) arg) = do
  (m', env2) <- fromMeta env1 BAct est m $ printf "Tuple Meta %s(%s)" (show baseExpr) (show arg)
  (baseM', env3) <- fromMeta env2 BAct est baseM $ printf "Tuple BaseMeta %s(%s)" (show baseExpr) (show arg)
  (baseExpr', env4) <- fromExpr est env3 baseExpr
  (arg', env5) <- fromObjArr est env4 arg
  let constraints = case (est, oaObj arg', oaArr arg') of
        (EncodeOut{}, Just obj, (Just _argExpr', arrM')) ->
          -- Output with (x=x)
          [
            ArrowTo 7 (getExprMeta baseExpr') baseM',
                        AddArg 8 (baseM', inExprSingleton obj) m',
                        BoundedByObjs 9 m' topType,
                        PropEq 11 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
        (EncodeOut{}, Nothing, (Just _argExpr', _arrM')) ->
          -- Output with (x) infer
          [
            ArrowTo 12 (getExprMeta baseExpr') baseM',
                        AddInferArg 13 baseM' m',
                        BoundedByObjs 14 m' topType
                        ]
        (EncodeIn{}, Just obj, (Just _argExpr', arrM')) ->
          -- Input with (x=x)
          [
            EqPoints 16 (getExprMeta baseExpr') baseM',
            BoundedByObjs 17 m' topType,
                     AddArg 19 (baseM', inExprSingleton obj) m',
                     PropEq 21 (m', TVArg $ inExprSingleton obj) arrM'
                    ]
        (EncodeIn{}, Just obj, (Nothing, arrM')) ->
          -- Input with (x -> T)
          [
         EqPoints 22 (getExprMeta baseExpr') baseM',
            BoundedByObjs 23 m' topType,
                     AddArg 24 (baseM', inExprSingleton obj) m',
                     PropEq 25 (m', TVArg $ inExprSingleton obj) arrM'
                    ]
        _ -> error $ printf "Invalid fromExpr in %s mode for %s" (show est) (show arg)
  let env6 = addConstraints env5 constraints
  return (TupleApply m' (baseM', baseExpr') arg', endConstraintBlock env6 (if isJust (oaObj arg') then Just arg' else Nothing) (maybe H.empty (fmap (first getExprMeta . head) . exprVarArgs) (oaObj arg')))
fromExpr est env1 (VarApply m baseExpr varName varVal) = do
  let baseName = printf "VarApply %s[%s = %s]" (show baseExpr) (show varName) (show varVal) :: String
  (m', env2) <- fromMeta env1 BAct est m $ printf "%s Meta" baseName
  (baseExpr', env3) <- fromExpr est env2 baseExpr
  (varVal', env4) <- fromMeta env3 BAct est varVal $ printf "%s val" baseName
  let constraints = case est of
        EncodeOut{} -> [
                      ArrowTo 26 (getExprMeta baseExpr') m',
                      PropEq 27 (m', TVVar varName) varVal',
                      BoundedByObjs 28 m' topType
                       ]
        EncodeIn{} -> [
                      EqPoints 29 (getExprMeta baseExpr') m',
                      PropEq 29 (m', TVVar varName) varVal'
                      ]
  let env5 = addConstraints env4 constraints
  return (VarApply m' baseExpr' varName varVal', env5)

fromObjArr :: EncodeState -> FEnv -> PObjArr -> TypeCheckResult (VObjArr, FEnv)
fromObjArr est env1 oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  (oaObj', env2) <- case oaObj of
    Just inExpr -> do
      (inExpr', env2a) <- fromExpr (asEncodeIn est) env1 inExpr
      return (Just inExpr', env2a)
    Nothing -> return (Nothing, env1)
  (oaAnnots', env3) <- mapMWithFEnv (startConstrainBlock env2) (fromExpr est) oaAnnots
  (oaArr', env4) <- case oaArr of
    (arrExpr, arrM) -> do
      (arrM', env3a) <- fromMeta env3 BAct est arrM $ printf "oa arrM %s" (show oa)
      case arrExpr of
        Just argExpr -> do
          (argExpr', env3b) <- fromExpr est env3a argExpr
          return ((Just argExpr', arrM'), env3b)
        Nothing -> return ((Nothing, arrM'), env3a)
  let constraints = case (est, oaObj', oaArr') of
        (EncodeOut{}, Just _obj, (Just argExpr', arrM')) ->
          -- Output with (x=x)
          [
                        BoundedByObjs 10 arrM' topType,
                        ArrowTo 10 (getExprMeta argExpr') arrM'
                        ]
        (EncodeOut{}, Nothing, (Just argExpr', arrM')) ->
          -- Output with (x) infer
          [
                        BoundedByObjs 15 arrM' topType,
                        ArrowTo 15 (getExprMeta argExpr') arrM'
                        ]
        (EncodeIn{}, Just obj, (Just argExpr', arrM')) ->
          -- Input with (x=x)
          [
            BoundedByObjs 18 (getExprMeta argExpr') topType,
            BoundedByKnown 18 (getExprMeta argExpr') (TypeVar (TVArg $ inExprSingleton obj) TVInt),
                     EqPoints 20 (getExprMeta argExpr') arrM'
                    ]
        (EncodeIn{}, Just _obj, (Nothing, _arrM')) ->
          -- Input with (x -> T)
          []
  let env5 = addConstraints env4 constraints
  let oa' = oa{
        oaObj=oaObj',
        oaArr=oaArr',
        oaAnnots=oaAnnots'
        }
  return (oa', env5)

fromObjectMap :: FEnv -> PObjArr -> TypeCheckResult (VObjArr, FEnv)
fromObjectMap env1 oa@ObjArr{oaBasis, oaAnnots, oaObj=Just obj, oaArr=(maybeArrE, arrM)} = do
  let inEst = EncodeIn
  (obj', env2) <- fromExpr inEst (startConstrainBlock env1) obj

  let est = EncodeOut (Just obj')
  (oaAnnots', env3) <- mapMWithFEnv env2 (fromExpr est) oaAnnots
  (mUserReturn', env4) <- fromMeta env3 BAct est arrM (printf "Specified result from %s" (show $ exprPath obj'))
  (oaArr'@(_, arrM'), env5) <- case maybeArrE of
    Just arrE -> do
      (vExpr, env4a) <- fromExpr est env4 arrE
      (arrM', env4b) <- fromMeta env4a BAct est (Meta topType (labelPos "res" $ getMetaPos arrM) emptyMetaDat) $ printf "Arrow result from %s" (show $ exprPath obj')
      return ((Just vExpr, arrM'), addConstraints env4b [ArrowTo 32 (getExprMeta vExpr) arrM', ArrowTo 33 (getExprMeta vExpr) mUserReturn'])
    Nothing -> return ((Nothing, mUserReturn'), env4)
  let oa' = oa{oaObj=Just obj', oaArr=oaArr', oaAnnots=oaAnnots'}
  let env6 = fAddVTypeGraph env5 (oaObjPath oa) oa'
  let env7 = addConstraints env6 [EqPoints 34 (getExprMeta obj') arrM' | oaBasis == TypeObj]
  let env8 = endConstraintBlock env7 (Just oa') (first getExprMeta . head <$> exprVarArgs obj')
  return (oa', env8)
fromObjectMap _ oa = error $ printf "Invalid oa in fromObjectMap %s" (show oa)

fromPrgm :: FEnv -> PPrgm -> TypeCheckResult (VPrgm, FEnv)
fromPrgm env1 (objMap, classGraph, annots) = do
  (objMap', env2) <- mapMWithFEnv env1 fromObjectMap objMap

  (annots', env3) <- mapMWithFEnv (startConstrainBlock env2) (fromExpr (EncodeOut Nothing)) annots
  let env4 = endConstraintBlock env3 Nothing H.empty

  return ((objMap', classGraph, annots'), env4)

addTypeGraphArrow :: FEnv -> TObjArr -> TypeCheckResult ((), FEnv)
addTypeGraphArrow env oa = return ((), fAddTTypeGraph env (oaObjPath oa) oa)

addTypeGraphObjects :: FEnv -> TObjArr -> TypeCheckResult ((), FEnv)
addTypeGraphObjects env oa = do
  (_, env') <- addTypeGraphArrow env oa
  return ((), env')

addTypeGraphPrgm :: FEnv -> TPrgm -> TypeCheckResult ((), FEnv)
addTypeGraphPrgm env (objMap, _, _) = do
  (_, env') <- mapMWithFEnv env addTypeGraphObjects objMap
  return ((), env')

fromPrgms :: FEnv -> [PPrgm] -> [TPrgm] -> TypeCheckResult ([VPrgm], FEnv)
fromPrgms env1 pprgms tprgms = do
  (_, env2) <- mapMWithFEnv env1 addTypeGraphPrgm tprgms
  (vprgms, env3) <- mapMWithFEnv env2 fromPrgm pprgms
  let (tjoinObjMap, _, _) = mergePrgms tprgms
  let env4 = addUnionObjToEnv env3 (concatMap fst3 vprgms) tjoinObjMap
  return (vprgms, env4)
