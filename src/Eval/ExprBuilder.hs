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

-- Wraps a value v in Context(value=v, io=IO)
applyIO :: EExpr -> EExpr
applyIO input = eApply (eApply (eVal "/Context") "value" input) "io" ioArg

ioArg :: EExpr
ioArg = Arg (Meta ioType Nothing emptyMetaDat) "io"

eApply :: EExpr -> String -> EExpr -> EExpr
eApply baseExpr argName argExpr = TupleApply m (getExprMeta baseExpr, baseExpr) (TupleArgIO (emptyMetaE "appArg" argExpr) argName argExpr)
  where
    m = Meta (singletonType $ baseType{ptArgs=H.insert argName (getExprType argExpr) baseArgs}) Nothing emptyMetaDat
    baseType@PartialType{ptArgs=baseArgs} = getExprPartialType baseExpr

eVal :: String -> EExpr
eVal name = Value m name
  where m = Meta (singletonType $ partialVal (PTypeName name)) Nothing emptyMetaDat

getExprPartialType :: EExpr -> PartialType
getExprPartialType expr = case getExprType expr of
  UnionType partials -> case splitUnionType partials of
    [partial] -> partial
    _ -> error $ printf "Found non-singleton in getExprPartialType %s" (show expr)
  _ -> error $ printf "Found on-union in getExprPartialType %s" (show expr)
