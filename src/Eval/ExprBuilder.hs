--------------------------------------------------------------------
-- |
-- Module    :  Eval.ExprBuilder
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to build typed 'Expr' for use in macros and
-- inputs during evaluation.
--------------------------------------------------------------------

module Eval.ExprBuilder where

import qualified Data.HashMap.Strict as H
import           Eval.Common
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf

ioM :: EvalMeta
ioM = Meta ioType Nothing emptyMetaDat

ioArg :: EExpr
ioArg = Value ioM "io"

eApply :: EExpr -> String -> EExpr -> EExpr
eApply baseExpr argName argExpr = TupleApply m (getExprMeta baseExpr, baseExpr) (mkIOObjArr (mWithType argExprType $ emptyMetaE "appArg" argExpr) (partialKey argName) argExpr)
  where
    argExprType = getExprType argExpr
    m = Meta (singletonType $ baseType{ptArgs=H.insert (partialKey argName) argExprType baseArgs}) Nothing emptyMetaDat
    baseType@PartialType{ptArgs=baseArgs} = getExprPartialType baseExpr

eApplyM :: EExpr -> String -> EvalMeta -> EExpr
eApplyM baseExpr argName argM = TupleApply m (getExprMeta baseExpr, baseExpr) (mkIObjArr argM (partialKey argName))
  where
    argExprType = getMetaType argM
    m = Meta (singletonType $ baseType{ptArgs=H.insert (partialKey argName) argExprType baseArgs}) Nothing emptyMetaDat
    baseType@PartialType{ptArgs=baseArgs} = getExprPartialType baseExpr


eVal :: String -> EExpr
eVal name = Value m name
  where m = Meta (typeVal (PTypeName name)) Nothing emptyMetaDat

getExprPartialType :: EExpr -> PartialType
getExprPartialType expr = case getExprType expr of
  UnionType partials -> case splitUnionType partials of
    [partial] -> partial
    _ -> error $ printf "Found non-singleton in getExprPartialType %s" (show expr)
  _ -> error $ printf "Found on-union in getExprPartialType %s" (show expr)
