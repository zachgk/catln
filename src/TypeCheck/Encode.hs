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
-- It could be a lower bound, upper bound, or exact bound
data TypeBound = BUpper | BLower | BEq
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
        BUpper -> fresh env (TypeCheckResult [] $ SType tp topType description)
        BLower -> fresh env (TypeCheckResult [] $ SType topType tp description)
        BEq    -> fresh env (TypeCheckResult [] $ SType tp tp description)
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
  (m', env') <- fromMeta env BUpper est m ("Constant " ++ show c)
  return (CExpr m' c, addConstraints env' [EqualsKnown 3 m' (constantType c)])
fromExpr est@EncodeOut{} env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BUpper est m ("Out Val " ++ name)
  return (Value m' name, addConstraints env2 [BoundedByKnown 4 m' (relTypeVal name), BoundedByObjs 4 m'])
fromExpr est@EncodeIn{} env1 (Value m name) = do
  (m', env2) <- fromMeta env1 BUpper est m ("In Val " ++ name)
  return (Value m' name, addConstraints env2 [BoundedByKnown 5 m' (relTypeVal name)])
fromExpr est env1 (HoleExpr m hole) = do
  (m', env2) <- fromMeta env1 BUpper est m ("Hole " ++ show hole)
  return (HoleExpr m' hole, env2)
fromExpr est env1 (AliasExpr base alias) = do
  (base', env2) <- fromExpr est env1 base
  (alias', env3) <- fromExpr est env2 alias
  let env4 = addConstraints env3 [BoundedByKnown 6 (getExprMeta base') (TypeVar (TVArg $ inExprSingleton alias) TVInt)]
  return (AliasExpr base' alias', env4)
fromExpr est env1 (TupleApply m (baseM, baseExpr) arg@ObjArr{oaObj, oaAnnots, oaArr}) = do
  (m', env2) <- fromMeta env1 BUpper est m $ printf "Tuple Meta %s(%s)" (show baseExpr) (show arg)
  (baseM', env3) <- fromMeta env2 BUpper est baseM $ printf "Tuple BaseMeta %s(%s)" (show baseExpr) (show arg)
  (baseExpr', env4) <- fromExpr est env3 baseExpr
  (oaObj', env5) <- case oaObj of
    Just (GuardExpr inExpr guardExpr) -> do
      (inExpr', env5a) <- fromExpr (asEncodeIn est) env4 inExpr
      (guardExpr', env5b) <- fromGuard est env5a guardExpr
      -- TODO: Add constraint that input expr' refined by guardExpr'
      -- For outputs, should instead verify in decode that it refined (is subtype)
      return (Just $ GuardExpr inExpr' guardExpr', env5b)
    Nothing -> return (Nothing, env4)
  (oaAnnots', env6) <- mapMWithFEnv (startConstrainBlock env5) (fromExpr est) oaAnnots
  (oaArr', env7) <- case oaArr of
    (arrGuardExpr, arrM) -> do
      (arrM', env6a) <- fromMeta env6 BUpper est arrM $ printf "Tuple arrM %s(%s)" (show baseExpr) (show arg)
      case arrGuardExpr of
        Just (GuardExpr argExpr guardExpr) -> do
          (argExpr', env6b) <- fromExpr est env6a argExpr
          (guardExpr', env6c) <- fromGuard est env6b guardExpr
          return ((Just (GuardExpr argExpr' guardExpr'), arrM'), env6c)
        Nothing -> return ((Nothing, arrM'), env6a)
  let constraints = case (est, oaObj', oaArr') of
        (EncodeOut{}, Just (GuardExpr obj Nothing), (Just (GuardExpr argExpr' Nothing), arrM')) ->
          -- Output with (x=x)
          [
            ArrowTo 7 (getExprMeta baseExpr') baseM',
                        AddArg 8 (baseM', inExprSingleton obj) m',
                        BoundedByObjs 9 m',
                        BoundedByObjs 10 arrM',
                        ArrowTo 10 (getExprMeta argExpr') arrM',
                        PropEq 11 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
        (EncodeOut{}, Nothing, (Just (GuardExpr argExpr' Nothing), arrM')) ->
          -- Output with (x) infer
          [
            ArrowTo 12 (getExprMeta baseExpr') baseM',
                        AddInferArg 13 baseM' m',
                        BoundedByObjs 14 m',
                        BoundedByObjs 15 arrM',
                        ArrowTo 15 (getExprMeta argExpr') arrM'
                        ]
        (EncodeIn{}, Just (GuardExpr obj Nothing), (Just (GuardExpr argExpr' Nothing), arrM')) ->
          -- Input with (x=x)
          [
            EqPoints 16 (getExprMeta baseExpr') baseM',
            BoundedByObjs 17 m',
            BoundedByObjs 18 (getExprMeta argExpr'),
                     AddArg 19 (baseM', inExprSingleton obj) m',
                     EqPoints 20 (getExprMeta argExpr') arrM',
                     PropEq 21 (m', TVArg $ inExprSingleton obj) arrM'
                    ]
        (EncodeIn{}, Just (GuardExpr obj Nothing), (Nothing, arrM')) ->
          -- Input with (x -> T)
          [
         EqPoints 22 (getExprMeta baseExpr') baseM',
            BoundedByObjs 23 m',
                     AddArg 24 (baseM', inExprSingleton obj) m',
                     PropEq 25 (m', TVArg $ inExprSingleton obj) arrM'
                    ]
        _ -> error $ printf "Invalid fromExpr in %s mode for %s (obj %s and arr %s)" (show est) (show arg) (show oaObj) (show oaArr)
  let env8 = addConstraints env7 constraints
  let arg' = arg{
        oaObj=oaObj',
        oaArr=oaArr',
        oaAnnots=oaAnnots'
        }
  return (TupleApply m' (baseM', baseExpr') arg', endConstraintBlock env8 (if isJust oaObj' then Just arg' else Nothing) (maybe H.empty (fmap (first getExprMeta . head) . exprVarArgs . rgeExpr) oaObj'))
fromExpr est env1 (VarApply m baseExpr varName varVal) = do
  let baseName = printf "VarApply %s[%s = %s]" (show baseExpr) (show varName) (show varVal) :: String
  (m', env2) <- fromMeta env1 BUpper est m $ printf "%s Meta" baseName
  (baseExpr', env3) <- fromExpr est env2 baseExpr
  (varVal', env4) <- fromMeta env3 BUpper est varVal $ printf "%s val" baseName
  let constraints = case est of
        EncodeOut{} -> [
                      ArrowTo 26 (getExprMeta baseExpr') m',
                      PropEq 27 (m', TVVar varName) varVal',
                      BoundedByObjs 28 m'
                       ]
        EncodeIn{} -> [
                      EqPoints 29 (getExprMeta baseExpr') m',
                      PropEq 29 (m', TVVar varName) varVal'
                      ]
  let env5 = addConstraints env4 constraints
  return (VarApply m' baseExpr' varName varVal', env5)

fromGuard :: EncodeState -> FEnv -> Maybe PExpr -> TypeCheckResult (Maybe VExpr, FEnv)
fromGuard est env1 (Just expr) =  do
  (expr', env2) <- fromExpr est env1 expr
  let (bool, env3) = fresh env2 $ TypeCheckResult [] $ SType topType boolType "ifGuardBool"
  let bool' = Meta boolType (labelPos "bool" $ getMetaPos $ getExprMeta expr) (mkVarMetaDat est bool)
  return (Just expr', addConstraints env3 [ArrowTo 30 (getExprMeta expr') bool'])
fromGuard _ env Nothing = return (Nothing, env)

fromVaenvItem :: EncodeState -> FEnv -> (PExpr, PreMeta) -> TypeCheckResult ((VarMeta, VarMeta), FEnv)
fromVaenvItem est env1 (inE, outM) = do
  (inM', env2) <- fromMeta env1 BUpper est (getExprMeta inE) "argIn"
  (outM', env3) <- fromMeta env2 BUpper est outM "argOut"
  return ((inM', outM'), env3)

fromObjectMap :: FEnv -> PObjArr -> TypeCheckResult (VObjArr, FEnv)
fromObjectMap env1 oa@ObjArr{oaBasis, oaAnnots, oaObj=Just (GuardExpr obj g), oaArr=(maybeArrE, arrM)} = do
  let inEst = EncodeIn
  (obj', env2) <- fromExpr inEst (startConstrainBlock env1) obj

  let est = EncodeOut (Just obj')
  (oaAnnots', env3) <- mapMWithFEnv env2 (fromExpr est) oaAnnots
  (mUserReturn', env4) <- fromMeta env3 BUpper est arrM (printf "Specified result from %s" (show $ exprPath obj'))
  (g', env5) <- fromGuard est env4 g
  (oaArr'@(_, arrM'), env6) <- case maybeArrE of
    Just (GuardExpr arrE _) -> do
      (vExpr, env5a) <- fromExpr est env5 arrE
      (arrM', env5b) <- fromMeta env5a BUpper est (Meta topType (labelPos "res" $ getMetaPos arrM) emptyMetaDat) $ printf "Arrow result from %s" (show $ exprPath obj')
      return ((Just (GuardExpr vExpr Nothing), arrM'), addConstraints env5b [ArrowTo 32 (getExprMeta vExpr) arrM', ArrowTo 33 (getExprMeta vExpr) mUserReturn'])
    Nothing -> return ((Nothing, mUserReturn'), env5)
  let oa' = oa{oaObj=Just (GuardExpr obj' g'), oaArr=oaArr', oaAnnots=oaAnnots'}
  let env7 = fAddVTypeGraph env6 (oaObjPath oa) oa'
  let env8 = addConstraints env7 [EqPoints 34 (getExprMeta obj') arrM' | oaBasis == TypeObj]
  let env9 = endConstraintBlock env8 (Just oa') (first getExprMeta . head <$> exprVarArgs obj')
  return (oa', env9)
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
