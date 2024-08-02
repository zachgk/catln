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

import           Control.Monad.State
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           TypeCheck.Common
import MapMeta (clearMetaDat, MetaType (ArrMeta))

toMeta :: VarMeta -> StateT FEnv TypeCheckResult TypedMeta
toMeta  m@(Meta pt pos mid _) = do
  env@FEnv{feTypeEnv} <- get
  case descriptor env m of
    TypeCheckResult notes SType{stypeAct=ub, stypeTree=rt} -> lift $ TypeCheckResult notes $ Meta (intersectTypes feTypeEnv ub pt) pos mid rt
    TypeCheckResE notes -> lift $ TypeCheckResult notes (Meta BottomType pos mid Nothing)

member :: String -> [String ] -> Bool
member x arr = case suffixLookup x arr of
  Just _  -> True
  Nothing -> False

toExpr :: VExpr -> StateT FEnv TypeCheckResult TExpr
toExpr (CExpr m c) = do
  m' <- toMeta m
  return $ CExpr m' c
toExpr (Value m name) = do
  let name' = case maybeGetSingleton $ getMetaType m of
        Just PartialType{ptName=n} -> n
        _                          -> makeAbsoluteName name -- TODO Maybe consider this an exception
  m' <- toMeta m
  return $ Value m' name'
toExpr (HoleExpr m hole) = do
  m' <- toMeta m
  return $ HoleExpr m' hole
toExpr (AliasExpr base alias) = do
  base' <- toExpr base
  alias' <- toExpr alias
  return $ AliasExpr base' alias'
toExpr (EWhere m base cond) = do
  m' <- toMeta m
  base' <- toExpr base
  cond' <- toExpr cond
  return $ EWhere m' base' cond'
toExpr expr@(TupleApply m (baseM, baseExpr) arg) = do
  let mclear = clearMetaDat ArrMeta m
  m' <- toMeta m
  baseM' <- toMeta baseM
  baseExpr' <- toExpr baseExpr
  arg' <- case arg of
    EAppArg a@ObjArr{oaObj, oaArr, oaAnnots} -> do
      oaObj' <- case oaObj of
        Just joaObj -> Just <$> toExpr joaObj
        Nothing -> do
          mArgName <- case (getMetaType baseM', getMetaType m') of
            (UnionType basePartialLeafs, UnionType partialLeafs) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
              ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
                [argN] -> return $ Just argN
                opts -> lift $ TypeCheckResult [GenTypeCheckError mclear $ printf "Failed argument inference due to multiple arg options %s in %s" (show opts) (show expr)] Nothing
              (base, result) -> lift $ TypeCheckResult [GenTypeCheckError mclear $ printf "Failed argument inference due to multiple types with base %s and result %s in %s" (show base) (show result) (show expr)] Nothing
            (baseM'', m'') -> lift $ TypeCheckResult [GenTypeCheckError mclear $ printf "Failed argument inference due to non UnionType in baseMeta %s or meta %s in %s" (show baseM'') (show m'') (show expr)] Nothing
          return $ case mArgName of
            Just argName -> Just $ Value (mWithType (singletonType $ partialToType argName) $ emptyMetaM "inferArg" m') (pkName argName)
            Nothing -> Nothing -- Failed argument inference, return nothing and error out
      oaArr' <- forM oaArr $ \(oaArrExpr, oaArrM) -> do
        oaArrExpr' <- mapM toExpr oaArrExpr
        oaArrM' <- toMeta oaArrM
        return (oaArrExpr', oaArrM')
      oaAnnots' <- mapM toExpr oaAnnots
      return $ EAppArg a{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}
    EAppSpread a -> EAppSpread <$> toExpr a
  return $ TupleApply m' (baseM', baseExpr') arg'
toExpr (VarApply m baseExpr varName varVal) = do
  m' <- toMeta m
  baseExpr' <- toExpr baseExpr
  varVal' <- toMeta varVal
  let result = VarApply m' baseExpr' varName varVal'
  case m' of -- check for errors

    -- Don't check if a bottom type is present
    _ | getMetaType m' == BottomType -> return result

    _                                -> return result

toObjArr :: VObjArr -> StateT FEnv TypeCheckResult TObjArr
toObjArr oa@ObjArr{oaObj, oaArr, oaAnnots} = do
  oaObj' <- mapM toExpr oaObj
  oaArr' <- forM oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM toExpr arrE
    arrM' <- toMeta arrM
    return (arrE', arrM')
  oaAnnots' <- mapM toExpr oaAnnots
  return oa{oaObj=oaObj', oaArr=oaArr', oaAnnots=oaAnnots'}

toPrgm :: VPrgm -> StateT FEnv TypeCheckResult TPrgm
toPrgm (objMap, classGraph, annots) = do
  objMap' <- mapM toObjArr objMap
  annots' <- mapM toExpr annots
  return (objMap', classGraph, annots')

toPrgms :: [VPrgm] -> StateT FEnv TypeCheckResult [TPrgm]
toPrgms = mapM toPrgm
