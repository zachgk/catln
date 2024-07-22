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
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Lazy    as IM
import           Prelude             hiding (unzip)

import           Control.Monad.State
import           Data.Bifunctor      (Bifunctor (first))
import           Data.Maybe
import           Data.UUID           (nil)
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

showIncodeInOut :: EncodeState -> String
showIncodeInOut EncodeOut{} = "Out"
showIncodeInOut EncodeIn{}  = "In"

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
  feUnionAllObjs = Meta PTopType Nothing nil (VarMetaDat (Just 0) Nothing),
  feVTypeGraph = H.empty,
  feTTypeGraph = H.empty,
  feUpdatedDuringEpoch = False,
  feTypeEnv = mkTypeEnv prgm,
  feTrace = mkTraceConstrain
  }

mkVarMetaDat :: EncodeState -> Pnt -> VarMetaDat
mkVarMetaDat (EncodeOut obj) p = VarMetaDat (Just p) obj
mkVarMetaDat EncodeIn p        = VarMetaDat (Just p) Nothing

fromMeta :: TypeBound -> EncodeState -> PreMeta -> String -> StateT FEnv TypeCheckResult VarMeta
fromMeta bound est m description  = do
  let tp = getMetaType m
  p <- case bound of
        BAct    -> fresh (TypeCheckResult [] $ SType tp PTopType Nothing description)
        BReq    -> fresh (TypeCheckResult [] $ SType PTopType tp Nothing description)
        BActReq -> fresh (TypeCheckResult [] $ SType tp tp Nothing description)
  return (mapMetaDat (\_ -> mkVarMetaDat est p) m)

fromExpr :: EncodeState -> PExpr -> StateT FEnv TypeCheckResult VExpr
fromExpr est (CExpr m c) = do
  m' <- fromMeta BAct est m ("Constant " ++ show c)
  modify $ addConstraints [EqualsKnown 3 m' (constantType c)]
  return $ CExpr m' c
fromExpr est@EncodeOut{} (Value m name) = do
  m' <- fromMeta BAct est m ("Out Val " ++ name)
  modify $ addConstraints [BoundedByKnown 4 m' (relTypeVal name), BoundedByObjs 4 m' PTopType]
  return $ Value m' name
fromExpr est@EncodeIn{} (Value m name) = do
  m' <- fromMeta BAct est m ("In Val " ++ name)
  modify $ addConstraints [BoundedByKnown 5 m' (relTypeVal name)]
  return $ Value m' name
fromExpr est (HoleExpr m hole) = do
  m' <- fromMeta BAct est m ("Hole " ++ show hole)
  return $ HoleExpr m' hole
fromExpr est (AliasExpr base alias) = do
  base' <- fromExpr est base
  alias' <- fromExpr est alias
  modify $ addConstraints [BoundedByKnown 6 (getExprMeta base') (TypeVar (TVArg $ inExprSingleton alias) TVInt)]
  return $ AliasExpr base' alias'
fromExpr est (EWhere m base cond) = do
  m' <- fromMeta BAct est m (printf "Where %s | %s" (show base) (show cond))
  base' <- fromExpr est base
  let condEst = case est of
        EncodeIn -> EncodeOut Nothing
        _        -> est
  cond' <- fromExpr condEst cond
  bool <- fresh $ TypeCheckResult [] $ SType PTopType boolType Nothing "ifGuardBool"
  let bool' = Meta boolType (labelPos "bool" $ getMetaPos $ getExprMeta cond) nil (mkVarMetaDat est bool)
  modify $ addConstraints [ArrowTo 30 (getExprMeta cond') bool', ConWhere 30 (getExprMeta base') (getExprMeta cond') m']
  return $ EWhere m' base' cond'
fromExpr est (TupleApply m (baseM, baseExpr) arg) = do
  m' <- fromMeta BAct est m $ printf "Tuple Meta %s %s(%s)" (showIncodeInOut est) (show baseExpr) (show arg)
  baseM' <- fromMeta BAct est baseM $ printf "Tuple BaseMeta %s %s(%s)" (showIncodeInOut est) (show baseExpr) (show arg)
  baseExpr' <- fromExpr est baseExpr
  arg'' <- case arg of
    EAppArg a -> do
      arg' <- fromObjArr est a
      let constraints = case (est, oaObj arg', oaArr arg') of
            (EncodeOut{}, Just obj, Just (Just _argExpr', arrM')) ->
              -- Output with (x=x)
              [
                            AddArg 8 (baseM', TVArg $ inExprSingleton obj) m',
                            PropEq 11 (m', TVArg $ inExprSingleton obj) arrM'
                            ]
            (EncodeOut{}, Nothing, Just (Just _argExpr', _arrM')) ->
              -- Output with (x) infer
              [
                            AddInferArg 13 baseM' m'
                            ]
            (EncodeIn{}, Just obj, Just (Just _argExpr', arrM')) ->
              -- Input with (x=x)
              [
                        AddArg 19 (baseM', TVArg $ inExprSingleton obj) m',
                        PropEq 21 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
            (EncodeIn{}, Just obj, Just (Nothing, arrM')) ->
              -- Input with (x -> T)
              [
                        AddArg 24 (baseM', TVArg $ inExprSingleton obj) m',
                        PropEq 25 (m', TVArg $ inExprSingleton obj) arrM'
                        ]
            _ -> error $ printf "Invalid fromExpr in %s mode for %s" (show est) (show arg)
      modify $ addConstraints constraints
      modify $ endConstraintBlock (if isJust (oaObj arg') then Just arg' else Nothing) (maybe H.empty (fmap (first getExprMeta . head) . exprVarArgs) (oaObj arg'))
      return $ EAppArg arg'
    EAppVar vn vm -> do
      let baseName = printf "VarApply %s[%s = %s]" (show baseExpr) (show vn) (show vm) :: String
      vm' <- fromMeta BAct est vm $ printf "%s val" baseName
      modify $ addConstraints [AddArg 27 (baseM', TVVar vn) m', PropEq 27 (m', TVVar vn) vm']
      return $ EAppVar vn vm'
    EAppSpread a@HoleExpr{} -> do -- Shortcut for Hole since no need to track neglected args
      a' <- fromExpr est a
      modify $ addConstraints [SetArgMode 11 False baseM' m']
      return $ EAppSpread a'
    EAppSpread a -> error $ printf "Not yet implemented %s" (show a)
  let baseConstraints = case est of
        EncodeOut{} -> [
                ArrowTo 7 (getExprMeta baseExpr') baseM',
                BoundedByObjs 9 m' PTopType
                       ]
        EncodeIn{} -> [
                EqPoints 16 (getExprMeta baseExpr') baseM',
                BoundedByObjs 17 m' PTopType
                      ]
  modify $ addConstraints baseConstraints
  return $ TupleApply m' (baseM', baseExpr') arg''

fromObjArr :: EncodeState -> PObjArr -> StateT FEnv TypeCheckResult VObjArr
fromObjArr est oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- case oaObj of
    Just inExpr -> do
      inExpr' <- fromExpr (asEncodeIn est) inExpr
      return $ Just inExpr'
    Nothing -> return Nothing
  modify startConstrainBlock
  oaAnnots' <- mapM (fromExpr est) oaAnnots
  oaArr' <- case oaArr of
    Nothing -> return Nothing
    Just (arrExpr, arrM) -> do
      arrM' <- fromMeta BAct est arrM $ printf "oa arrM %s" (show oa)
      case arrExpr of
        Just argExpr -> do
          argExpr' <- fromExpr est argExpr
          return $ Just (Just argExpr', arrM')
        Nothing -> return $ Just (Nothing, arrM')
  let constraints = case (est, oaObj', oaArr') of
        (EncodeOut{}, Just _obj, Just (Just argExpr', arrM')) ->
          -- Output with (x=x)
          [
                        BoundedByObjs 10 arrM' PTopType,
                        ArrowTo 10 (getExprMeta argExpr') arrM'
                        ]
        (EncodeOut{}, Nothing, Just (Just argExpr', arrM')) ->
          -- Output with (x) infer
          [
                        BoundedByObjs 15 arrM' PTopType,
                        ArrowTo 15 (getExprMeta argExpr') arrM'
                        ]
        (EncodeIn{}, Just obj, Just (Just argExpr', arrM')) ->
          -- Input with (x=x)
          [
            BoundedByObjs 18 (getExprMeta argExpr') PTopType,
            BoundedByKnown 18 (getExprMeta argExpr') (TypeVar (TVArg $ inExprSingleton obj) TVInt),
                     EqPoints 20 (getExprMeta argExpr') arrM'
                    ]
        (EncodeIn{}, Just _obj, Just (Nothing, _arrM')) ->
          -- Input with (x -> T)
          []
  modify $ addConstraints constraints
  let oa' = oa{
        oaObj=oaObj',
        oaArr=oaArr',
        oaAnnots=oaAnnots'
        }
  return oa'

fromObjectMap :: PObjArr -> StateT FEnv TypeCheckResult VObjArr
fromObjectMap oa@ObjArr{oaAnnots, oaObj=Just obj, oaArr} = do
  let inEst = EncodeIn
  modify startConstrainBlock
  obj' <- fromExpr inEst obj

  let est = EncodeOut (Just obj')
  oaAnnots' <- mapM (fromExpr est) oaAnnots
  oaArr' <- forM oaArr $ \(maybeArrE, arrM) -> do
    mUserReturn' <- fromMeta BAct est arrM (printf "Specified result from %s" (show $ exprPath obj'))
    case maybeArrE of
      Just arrE -> do
        vExpr <- fromExpr est arrE
        arrM' <- fromMeta BAct est (Meta PTopType (labelPos "res" $ getMetaPos arrM) nil emptyMetaDat) $ printf "Arrow result from %s" (show $ exprPath obj')
        modify $ addConstraints [ArrowTo 32 (getExprMeta vExpr) arrM', ArrowTo 33 (getExprMeta vExpr) mUserReturn', NoReturnArg 33 arrM']
        return (Just vExpr, arrM')
      Nothing -> return (Nothing, mUserReturn')
  let oa' = oa{oaObj=Just obj', oaArr=oaArr', oaAnnots=oaAnnots'}
  modify $ fAddVTypeGraph (oaObjPath oa) oa'
  modify $ endConstraintBlock (Just oa') (first getExprMeta . head <$> exprVarArgs obj')
  return oa'
fromObjectMap oa = error $ printf "Invalid oa in fromObjectMap %s" (show oa)

fromPrgm :: PPrgm -> StateT FEnv TypeCheckResult VPrgm
fromPrgm (objMap, classGraph, annots) = do
  objMap' <- mapM fromObjectMap objMap

  modify startConstrainBlock
  annots' <- mapM (fromExpr (EncodeOut Nothing)) annots
  modify $ endConstraintBlock Nothing H.empty

  return (objMap', classGraph, annots')

addTypeGraphArrow :: TObjArr -> StateT FEnv TypeCheckResult ()
addTypeGraphArrow oa = modify (fAddTTypeGraph (oaObjPath oa) oa)

addTypeGraphPrgm :: TPrgm -> StateT FEnv TypeCheckResult ()
addTypeGraphPrgm (objMap, _, _) = mapM_ addTypeGraphArrow objMap

fromPrgms :: [PPrgm] -> [TPrgm] -> StateT FEnv TypeCheckResult [VPrgm]
fromPrgms pprgms tprgms = do
  mapM_ addTypeGraphPrgm tprgms
  vprgms <- mapM fromPrgm pprgms
  let (tjoinObjMap, _, _) = mergePrgms tprgms
  addUnionObjToEnv (concatMap fst3 vprgms) tjoinObjMap
  return vprgms
