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
import           Data.Maybe
import           Data.UUID           (nil)
import           Eval.Common
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf

ioM :: EvalMeta
ioM = Meta ioType Nothing nil emptyMetaDat

ioArg :: EExpr
ioArg = Value ioM "io"

eApply :: EExpr -> (String, EExpr) -> EExpr
eApply baseExpr (argName, argExpr) = baseExpr `eApplyOAs` [mkIOObjArr (mWithType (getExprType argExpr) $ emptyMetaE argExpr) (partialKey argName) argExpr]

eApplyOAs :: EExpr -> [EObjArr] -> EExpr
eApplyOAs = foldl eApplyOA
  where
    eApplyOA base arg = TupleApply (emptyMetaT mType) (emptyMetaT $ getExprType base, base) (EAppArg arg)
      where
        mType = singletonType $ baseType{ptArgs=H.insert (partialKey $ oaObjPath arg) (getMetaType $ snd $ fromJust $ oaArr arg) baseArgs}
        baseType@PartialType{ptArgs=baseArgs} = getExprPartialType base

eApplyM :: EExpr -> String -> EvalMeta -> EExpr
eApplyM baseExpr argName argM = TupleApply m (getExprMeta baseExpr, baseExpr) (EAppArg $ mkIObjArr argM (partialKey argName))
  where
    argExprType = getMetaType argM
    m = Meta (singletonType $ baseType{ptArgs=H.insert (partialKey argName) argExprType baseArgs}) Nothing nil emptyMetaDat
    baseType@PartialType{ptArgs=baseArgs} = getExprPartialType baseExpr


eVal :: String -> EExpr
eVal name = Value m name
  where m = Meta (typeVal name) Nothing nil emptyMetaDat

getExprPartialType :: EExpr -> PartialType
getExprPartialType expr = case getExprType expr of
  UnionType partials -> case splitUnionType partials of
    [partial] -> partial
    _ -> error $ printf "Found non-singleton in getExprPartialType %s" (show expr)
  _ -> error $ printf "Found on-union in getExprPartialType %s" (show expr)
