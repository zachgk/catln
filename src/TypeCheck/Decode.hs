--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Decode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module decodes after type checking is completed to produce
-- the final typechecked program.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Decode where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Control.Monad       (forM)
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common

toMeta :: FEnv -> VarMeta -> String -> TypeCheckResult (Meta ())
toMeta env@FEnv{feClassGraph} m@(Meta pt pos _) _ = case pointUb env m of
  TypeCheckResult notes ub -> case pt of
    TypeVar{} -> return $ Meta pt pos emptyMetaDat
    _ -> TypeCheckResult notes $ Meta (intersectTypes feClassGraph ub pt) pos emptyMetaDat
  TypeCheckResE notes -> do
    TypeCheckResult notes (Meta bottomType pos emptyMetaDat)

member :: String -> [String ] -> Bool
member x arr = case suffixLookup x arr of
  Just _  -> True
  Nothing -> False

toExpr :: FEnv -> VExpr -> TypeCheckResult TExpr
toExpr env (CExpr m c) = do
  m' <- toMeta env m $ "Constant " ++ show c
  return $ CExpr m' c
toExpr env (Value m name) = do
  m' <- toMeta env m $ "Value_" ++ name
  return $ Value m' name
toExpr env (Arg m name) = do
  m' <- toMeta env m $ "Arg_" ++ name
  return $ Arg m' name
toExpr env (HoleExpr m hole) = do
  m' <- toMeta env m $ "Arg_" ++ show hole
  return $ HoleExpr m' hole
toExpr env (AliasExpr base alias) = do
  base' <- toExpr env base
  alias' <- toExpr env alias
  return $ AliasExpr base' alias'
toExpr env (TupleApply m (baseM, baseExpr) arg) = case arg of
  ObjArr{oaObj=Just (GuardExpr argObj _), oaArr=Just (Just (GuardExpr argExpr _), _)} -> do
    let (argName, argM) = exprPathM argObj
    m' <- toMeta env m "TupleApply_M"
    baseM' <- toMeta env baseM "TupleApply_baseM"
    baseExpr' <- toExpr env baseExpr
    argM' <- toMeta env argM "TupleApply_ArgM"
    argExpr' <- toExpr env argExpr
    let result = TupleApply m' (baseM', baseExpr') (mkIOObjArr argM' argName argExpr')
    case m' of -- check for errors

      -- Don't check if a bottom type is present
      _ | getMetaType m' == bottomType     -> return result
      _ | getMetaType baseM' == bottomType -> return result

      -- tp@(Meta (UnionType uType) _ _) | all (\PartialType{ptArgs=leafArgs} -> not (argName `member` H.keys leafArgs)) (splitUnionType uType) ->
      --                                     TypeCheckResult [TupleMismatch baseM' baseExpr' tp $ H.singleton argName argExpr'] result

      _                                    -> return result
  ObjArr{oaArr=Just (Just (GuardExpr argExpr _), _)} -> do
    let pos = getMetaPos m
    m' <- toMeta env m "TupleApplyInfer_M"
    baseM' <- toMeta env baseM "TupleApplyInfer_baseM"
    baseExpr' <- toExpr env baseExpr
    argExpr' <- toExpr env argExpr
    argName <- case (getMetaType baseM', getMetaType m') of
      (UnionType basePartialLeafs, UnionType partialLeafs) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
        ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
          [argN] -> return argN
          _ -> TypeCheckResE [GenTypeCheckError pos "Failed argument inference due to multiple arg options"]
        (base, result) -> TypeCheckResE [GenTypeCheckError pos $ printf "Failed argument inference due to multiple types with base %s and result %s" (show base) (show result)]
      _ -> TypeCheckResE [GenTypeCheckError pos "Failed argument inference due to non UnionType"]
    return $ TupleApply m' (baseM', baseExpr') (mkIOObjArr (emptyMetaE "ArgName" argExpr') argName argExpr')
  ObjArr{oaObj=Just (GuardExpr argObj _), oaArr=_} -> do
    let (argName, argM) = exprPathM argObj
    m' <- toMeta env m "TupleApplyI_M"
    baseM' <- toMeta env baseM "TupleApplyI_baseM"
    baseExpr' <- toExpr env baseExpr
    argM' <- toMeta env argM "TupleApplyI_ArgM"
    return $ TupleApply m' (baseM', baseExpr') (mkIObjArr argM' argName)
  oa -> error $ printf "Invalid oa %s" (show oa)
toExpr env (VarApply m baseExpr varName varVal) = do
  m' <- toMeta env m "VarApply_M"
  baseExpr' <- toExpr env baseExpr
  varVal' <- toMeta env varVal "VarApply_val"
  let result = VarApply m' baseExpr' varName varVal'
  case m' of -- check for errors

    -- Don't check if a bottom type is present
    _ | getMetaType m' == bottomType -> return result

    _                                -> return result

toGuardExpr :: FEnv -> VGuardExpr -> TypeCheckResult TGuardExpr
toGuardExpr env (GuardExpr e g) = do
  e' <- toExpr env e
  g' <- mapM (toExpr env) g
  return $ GuardExpr e' g'

toObjArr :: FEnv -> VObjArr -> TypeCheckResult TEObjectMapItem
toObjArr env oa@ObjArr{oaObj, oaArr, oaAnnots} = do
  oaObj' <- mapM (toGuardExpr env) oaObj
  oaArr' <- forM oaArr $ \(ge, m) -> do
    ge' <- mapM (toGuardExpr env) ge
    m' <- toMeta env m "arrMeta"
    return (ge', m')
  oaAnnots' <- mapM (toExpr env) oaAnnots
  return oa{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}

toPrgm :: FEnv -> VEPrgm -> TypeCheckResult TEPrgm
toPrgm env (objMap, classGraph, annots) = do
  objMap' <- mapM (toObjArr env) objMap
  annots' <- mapM (toExpr env) annots
  return (objMap', classGraph, annots')

toPrgms :: FEnv -> [VEPrgm] -> TypeCheckResult [TEPrgm]
toPrgms env = mapM (toPrgm env)
