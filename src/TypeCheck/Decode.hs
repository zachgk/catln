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

import           Control.Monad       (forM)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common

toMeta :: FEnv -> VarMeta -> String -> TypeCheckResult (Meta ())
toMeta env@FEnv{feTypeEnv} m@(Meta pt pos _) _ = case pointUb env m of
  TypeCheckResult notes ub -> case pt of
    TypeVar{} -> return $ Meta pt pos emptyMetaDat
    _ -> TypeCheckResult notes $ Meta (intersectTypes feTypeEnv ub pt) pos emptyMetaDat
  TypeCheckResE notes -> do
    TypeCheckResult notes (Meta BottomType pos emptyMetaDat)

member :: String -> [String ] -> Bool
member x arr = case suffixLookup x arr of
  Just _  -> True
  Nothing -> False

toExpr :: FEnv -> VExpr -> TypeCheckResult TExpr
toExpr env (CExpr m c) = do
  m' <- toMeta env m $ "Constant " ++ show c
  return $ CExpr m' c
toExpr env (Value m name) = do
  let name' = case maybeGetSingleton $ getMetaType m of
        Just PartialType{ptName=n} -> n
        _                          -> makeAbsoluteName name -- TODO Maybe consider this an exception
  m' <- toMeta env m $ "Value_" ++ name'
  return $ Value m' name'
toExpr env (HoleExpr m hole) = do
  m' <- toMeta env m $ "Arg_" ++ show hole
  return $ HoleExpr m' hole
toExpr env (AliasExpr base alias) = do
  base' <- toExpr env base
  alias' <- toExpr env alias
  return $ AliasExpr base' alias'
toExpr env (EWhere base cond) = do
  base' <- toExpr env base
  cond' <- toExpr env cond
  return $ EWhere base' cond'
toExpr env (TupleApply m (baseM, baseExpr) arg@ObjArr{oaObj, oaArr, oaAnnots}) = do
  let pos = getMetaPos m
  m' <- toMeta env m "TupleApply_M"
  baseM' <- toMeta env baseM "TupleApply_baseM"
  baseExpr' <- toExpr env baseExpr
  oaObj' <- case oaObj of
    Just joaObj -> Just <$> toExpr env joaObj
    Nothing -> do
      argName <- case (getMetaType baseM', getMetaType m') of
        (UnionType basePartialLeafs, UnionType partialLeafs) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
          ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
            [argN] -> return argN
            _ -> TypeCheckResE [GenTypeCheckError pos "Failed argument inference due to multiple arg options"]
          (base, result) -> TypeCheckResE [GenTypeCheckError pos $ printf "Failed argument inference due to multiple types with base %s and result %s" (show base) (show result)]
        (baseM'', m'') -> TypeCheckResE [GenTypeCheckError pos $ printf "Failed argument inference due to non UnionType in baseMeta %s or meta %s" (show baseM'') (show m'')]
      return $ Just $ Value (mWithType (singletonType $ partialToType argName) $ emptyMetaM "inferArg" m') (pkName argName)
  oaArr' <- forM oaArr $ \(oaArrExpr, oaArrM) -> do
    oaArrExpr' <- mapM (toExpr env) oaArrExpr
    oaArrM' <- toMeta env oaArrM "TupleApply_argM"
    return (oaArrExpr', oaArrM')
  oaAnnots' <- mapM (toExpr env) oaAnnots
  let arg' = arg{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}
  return $ TupleApply m' (baseM', baseExpr') arg'
toExpr env (VarApply m baseExpr varName varVal) = do
  m' <- toMeta env m "VarApply_M"
  baseExpr' <- toExpr env baseExpr
  varVal' <- toMeta env varVal "VarApply_val"
  let result = VarApply m' baseExpr' varName varVal'
  case m' of -- check for errors

    -- Don't check if a bottom type is present
    _ | getMetaType m' == BottomType -> return result

    _                                -> return result

toObjArr :: FEnv -> VObjArr -> TypeCheckResult TObjArr
toObjArr env oa@ObjArr{oaObj, oaArr, oaAnnots} = do
  oaObj' <- mapM (toExpr env) oaObj
  oaArr' <- forM oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (toExpr env) arrE
    arrM' <- toMeta env arrM "arrMeta"
    return (arrE', arrM')
  oaAnnots' <- mapM (toExpr env) oaAnnots
  return oa{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}

toPrgm :: FEnv -> VPrgm -> TypeCheckResult TPrgm
toPrgm env (objMap, classGraph, annots) = do
  objMap' <- mapM (toObjArr env) objMap
  annots' <- mapM (toExpr env) annots
  return (objMap', classGraph, annots')

toPrgms :: FEnv -> [VPrgm] -> TypeCheckResult [TPrgm]
toPrgms env = mapM (toPrgm env)
