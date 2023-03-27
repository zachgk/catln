--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.MapRawMeta
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module extends "MapMeta" to rawPrgms.
--------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Syntax.Ct.MapRawMeta where
import           Data.Bifunctor (Bifunctor (second))
import           MapMeta
import           Syntax.Ct.Prgm

instance MapMeta RawExpr where
  mapMeta f loc (RawCExpr m c) = RawCExpr (f (ExprMeta loc ExprMetaConstant) m) c
  mapMeta f loc (RawValue m n) = RawValue (f (ExprMeta loc ExprMetaVal) m) n
  mapMeta f loc (RawHoleExpr m h) = RawHoleExpr (f (ExprMeta loc ExprMetaHole) m) h
  mapMeta f loc (RawTheExpr e) = RawTheExpr (mapMeta f loc e)
  mapMeta f loc (RawAliasExpr b a) = RawAliasExpr (mapMeta f loc b) (mapMeta f loc a)
  mapMeta f loc (RawTupleApply m (bm, be) args) = RawTupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMeta f loc be) (map (mapMetaObjArr f (Just loc)) args)
  mapMeta f loc (RawVarsApply m be vars) = RawVarsApply (f (ExprMeta loc ExprMetaApplyVar) m) (mapMeta f loc be) (map (second (f (ExprMeta loc ExprMetaApplyVarVal))) vars)
  mapMeta f loc (RawContextApply m (bm, be) args) = RawContextApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMeta f loc be) (map (second (f (ExprMeta loc ExprMetaTupleArg))) args)
  mapMeta f loc (RawParen e) = RawParen (mapMeta f loc e)
  mapMeta f loc (RawMethod b m) = RawMethod (mapMeta f loc b) (mapMeta f loc m)
  mapMeta f loc (RawList m lst) = RawList (f (ExprMeta loc ExprMetaTupleArg) m) (map (mapMeta f loc) lst)


mapMetaRawStatement :: (MapMeta e) => MetaFun a b -> RawStatement e a -> RawStatement e b
mapMetaRawStatement f (RawDeclStatement objArr) = RawDeclStatement (mapMetaObjArr f Nothing objArr)
mapMetaRawStatement f (MultiTypeDefStatement (MultiTypeDef clss objs) path) = MultiTypeDefStatement (MultiTypeDef clss (map (mapMeta f InputMeta) objs)) path
mapMetaRawStatement f (TypeDefStatement obj) = TypeDefStatement (mapMeta f InputMeta obj)
mapMetaRawStatement f (RawClassDefStatement (typeExpr, className) path) = RawClassDefStatement (mapMeta f InputMeta typeExpr, className) path
mapMetaRawStatement _ (RawClassDeclStatement d p) = RawClassDeclStatement d p
mapMetaRawStatement f (RawExprStatement e) = RawExprStatement (mapMeta f OutputMeta e)
mapMetaRawStatement f (RawAnnot e) = RawAnnot (mapMeta f AnnotMeta e)
mapMetaRawStatement _ (RawModule m p) = RawModule m p

mapMetaRawStatementTree :: (MapMeta e) => MetaFun a b -> RawStatementTree e a -> RawStatementTree e b
mapMetaRawStatementTree f (RawStatementTree s tree) = RawStatementTree (mapMetaRawStatement f s) (map (mapMetaRawStatementTree f) tree)

mapMetaRawPrgm :: MetaFun a b -> RawPrgm a -> RawPrgm b
mapMetaRawPrgm f (imports, statementTrees) = (imports, map (mapMetaRawStatementTree f) statementTrees)