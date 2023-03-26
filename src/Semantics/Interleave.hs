--------------------------------------------------------------------
-- |
-- Module    :  Semantics.Interleave
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains code to break apart the semantics to combine into syntax.
--------------------------------------------------------------------

module Semantics.Interleave where
import qualified Data.HashMap.Strict as H
import           Semantics.Prgm

interleaveM :: Meta m -> H.HashMap CodeRangeDat (Meta m)
interleaveM m = case getMetaPos m of
  Just cr -> H.singleton cr m
  Nothing -> H.empty

interleaveExpr :: Expr m -> H.HashMap CodeRangeDat (Meta m)
interleaveExpr (AliasExpr b a) = H.union (interleaveExpr b) (interleaveExpr a)
interleaveExpr (TupleApply m (bm, be) arg) = H.unions [
  interleaveM m,
  interleaveM bm,
  interleaveExpr be,
  interleaveObjArr arg
                                                      ]
interleaveExpr (VarApply m be _ argM) = H.unions [
  interleaveM m,
  interleaveExpr be,
  interleaveM argM
                                                      ]
interleaveExpr e = interleaveM $ getExprMeta  e

interleaveGuardExpr :: GuardExpr Expr m -> H.HashMap CodeRangeDat (Meta m)
interleaveGuardExpr (GuardExpr e g) = H.unions [interleaveExpr e, maybe H.empty interleaveExpr g]

interleaveObjArr :: ObjArr Expr m -> H.HashMap CodeRangeDat (Meta m)
interleaveObjArr ObjArr{oaObj, oaAnnots, oaM, oaArr} = H.unions [
  maybe H.empty interleaveGuardExpr oaObj,
  H.unions $ map interleaveExpr oaAnnots,
  interleaveM oaM,
  maybe H.empty interleaveGuardExpr oaArr
  ]

interleavePrgm :: ExprPrgm Expr m -> H.HashMap CodeRangeDat (Meta m)
interleavePrgm (objMap, _, annots) = H.unions (map interleaveObjArr objMap ++ map interleaveExpr annots)
