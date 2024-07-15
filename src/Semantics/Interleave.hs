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
import           Data.Bifunctor      (bimap)
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
  interleaveEApp arg
                                                      ]
  where
    interleaveEApp (EAppArg a)    = interleaveObjArr a
    interleaveEApp (EAppSpread a) = interleaveExpr a
interleaveExpr (VarApply m be _ argM) = H.unions [
  interleaveM m,
  interleaveExpr be,
  interleaveM argM
                                                      ]
interleaveExpr e = interleaveM $ getExprMeta  e

interleaveObjArr :: ObjArr Expr m -> H.HashMap CodeRangeDat (Meta m)
interleaveObjArr ObjArr{oaObj, oaAnnots, oaArr} = H.unions [
  maybe H.empty interleaveExpr oaObj,
  H.unions $ map interleaveExpr oaAnnots,
  maybe H.empty (uncurry H.union . bimap (maybe H.empty interleaveExpr) interleaveM) oaArr
  ]

interleavePrgm :: Prgm Expr m -> H.HashMap CodeRangeDat (Meta m)
interleavePrgm (objMap, _, annots) = H.unions (map interleaveObjArr objMap ++ map interleaveExpr annots)
