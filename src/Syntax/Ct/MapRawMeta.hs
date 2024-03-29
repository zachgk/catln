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
import           Data.Bifunctor (Bifunctor (bimap, second))
import           Data.Maybe     (fromMaybe)
import           MapMeta
import           Syntax.Ct.Prgm

instance MapMeta RawExpr where
  mapMeta f loc (RawCExpr m c) = RawCExpr (f (ExprMeta loc ExprMetaConstant) m) c
  mapMeta f loc (RawValue m n) = RawValue (f (ExprMeta loc ExprMetaVal) m) n
  mapMeta f loc (RawHoleExpr m h) = RawHoleExpr (f (ExprMeta loc ExprMetaHole) m) h
  mapMeta f loc (RawMacroValue m n) = RawMacroValue (f (ExprMeta loc ExprMetaMacroVal) m) n
  mapMeta f loc (RawTheExpr e) = RawTheExpr (mapMeta f loc e)
  mapMeta f loc (RawSpread e) = RawSpread (mapMeta f loc e)
  mapMeta f loc (RawAliasExpr b a) = RawAliasExpr (mapMeta f loc b) (mapMeta f loc a)
  mapMeta f loc (RawTupleApply m (bm, be) args) = RawTupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMeta f loc be) (map (mapMetaRawObjArr f (Just loc)) args)
  mapMeta f loc (RawVarsApply m be vars) = RawVarsApply (f (ExprMeta loc ExprMetaApplyVar) m) (mapMeta f loc be) (map mapVar vars)
    where
      mapVar (varKey, varVal) = (mapMeta f loc varKey, f (ExprMeta loc ExprMetaApplyVarVal) varVal)
  mapMeta f loc (RawContextApply m (bm, be) args) = RawContextApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMeta f loc be) (map (second (f (ExprMeta loc ExprMetaTupleArg))) args)
  mapMeta f loc (RawParen e) = RawParen (mapMeta f loc e)
  mapMeta f loc (RawMethod b m) = RawMethod (mapMeta f loc b) (mapMeta f loc m)
  mapMeta f loc (RawList m lst) = RawList (f (ExprMeta loc ExprMetaTupleArg) m) (map (mapMeta f loc) lst)
  mapMeta f loc (RawTypeProp m b (TypePropProj p v)) = RawTypeProp (f (ExprMeta loc ExprMetaTypeProp) m) (mapMeta f loc b) (TypePropProj p (mapMeta f loc v))
  mapMeta f loc (RawTypeProp m b (TypePropRel p v)) = RawTypeProp (f (ExprMeta loc ExprMetaTypeProp) m) (mapMeta f loc b) (TypePropRel p (mapMeta f loc v))

mapMetaRawObjArr :: (MapMeta e) => MetaFun a b -> Maybe MetaLocation -> RawObjArr e a -> RawObjArr e b
mapMetaRawObjArr f mloc roa@RawObjArr{roaObj, roaAnnots, roaArr, roaDef} = roa{
  roaObj = fmap (mapMetaGuardExpr f (fromMaybe InputMeta mloc)) roaObj,
  roaAnnots = map (mapMeta f (fromMaybe AnnotMeta mloc)) roaAnnots,
  roaArr = fmap (bimap (fmap (mapMetaGuardExpr f (fromMaybe OutputMeta mloc))) (f ArrMeta)) roaArr,
  roaDef = fmap (mapMeta f (fromMaybe InputMeta mloc)) roaDef
                                                             }

mapMetaRawStatement :: (MapMeta e) => MetaFun a b -> RawStatement e a -> RawStatement e b
mapMetaRawStatement f (RawDeclStatement objArr) = RawDeclStatement (mapMetaRawObjArr f Nothing objArr)
mapMetaRawStatement f (MultiTypeDefStatement (MultiTypeDef clss objs extends)) = MultiTypeDefStatement (MultiTypeDef (mapMeta f InputMeta clss) (map (mapMetaGuardExpr f InputMeta) objs) extends)
mapMetaRawStatement f (TypeDefStatement obj) = TypeDefStatement (mapMeta f InputMeta obj)
mapMetaRawStatement f (RawClassDefStatement (typeExpr, className)) = RawClassDefStatement (mapMeta f InputMeta typeExpr, className)
mapMetaRawStatement f (RawClassDeclStatement clss) = RawClassDeclStatement (mapMeta f InputMeta clss)
mapMetaRawStatement f (RawExprStatement e) = RawExprStatement (mapMeta f OutputMeta e)
mapMetaRawStatement f (RawAnnot e) = RawAnnot (mapMeta f AnnotMeta e)
mapMetaRawStatement f (RawApplyStatement (RawApply terms)) = RawApplyStatement $ RawApply $ map mapTerm terms
  where
    mapTerm (RATermDeep e)  = RATermDeep $ mapMeta f ApplyMeta e
    mapTerm (RATermChild e) = RATermChild $ mapMeta f ApplyMeta e
mapMetaRawStatement _ (RawModule m) = RawModule m

mapMetaRawStatementTree :: (MapMeta e) => MetaFun a b -> RawStatementTree e a -> RawStatementTree e b
mapMetaRawStatementTree f (RawStatementTree s tree) = RawStatementTree (mapMetaRawStatement f s) (map (mapMetaRawStatementTree f) tree)

mapMetaRawPrgm :: MetaFun a b -> RawPrgm a -> RawPrgm b
mapMetaRawPrgm f (imports, statementTrees) = (imports, map (mapMetaRawStatementTree f) statementTrees)
