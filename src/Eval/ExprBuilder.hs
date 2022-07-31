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
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Printf

-- Wraps a value v in Context(value=v, io=IO)
applyIO :: EExpr -> EExpr
applyIO input = eApply (eApply (eVal "/Context") "value" input) "io" ioArg

ioArg :: EExpr
ioArg = Arg (Typed ioType Nothing) "io"

eApply :: EExpr -> String -> EExpr -> EExpr
eApply baseExpr argName argExpr = TupleApply m (getExprMeta baseExpr, baseExpr) (Just argName) argExpr
  where
    m = Typed (singletonType $ baseType{ptArgs=H.insert argName (getExprType argExpr) baseArgs}) Nothing
    baseType@PartialType{ptArgs=baseArgs} = getExprPartialType baseExpr

eVal :: String -> EExpr
eVal name = Value m name
  where m = Typed (singletonType $ PartialType (PTypeName name) H.empty H.empty H.empty PtArgExact) Nothing

getExprPartialType :: EExpr -> PartialType
getExprPartialType expr = case getExprType expr of
  UnionType partials -> case splitUnionType partials of
    [partial] -> partial
    _ -> error $ printf "Found non-singleton in getExprPartialType %s" (show expr)
  _ -> error $ printf "Found on-union in getExprPartialType %s" (show expr)
