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
import           Control.Monad
import           Control.Monad.Identity (Identity (runIdentity))
import           Data.Maybe             (fromMaybe)
import           MapMeta
import           Syntax.Ct.Prgm

instance MapMeta RawExpr where
  mapMetaM f loc (RawCExpr m c) = do
    m' <- f (ExprMeta loc ExprMetaConstant) m
    return $ RawCExpr m' c
  mapMetaM f loc (RawValue m n) = do
    m' <- f (ExprMeta loc ExprMetaVal) m
    return $ RawValue m' n
  mapMetaM f loc (RawHoleExpr m h) = do
    m' <- f (ExprMeta loc ExprMetaHole) m
    return $ RawHoleExpr m' h
  mapMetaM f loc (RawMacroValue m n) = do
    m' <- f (ExprMeta loc ExprMetaMacroVal) m
    return $ RawMacroValue m' n
  mapMetaM f loc (RawFmtStrExpr m n s) = do
    m' <- f (ExprMeta loc ExprMetaMacroVal) m
    return $ RawFmtStrExpr m' n s
  mapMetaM f loc (RawApplyExpr m n) = do
    m' <- f (ExprMeta loc ExprMetaMacroVal) m
    n' <- mapRawApplyM f n
    return $ RawApplyExpr m' n'
  mapMetaM f loc (RawTheExpr e) = do
    e' <- mapMetaM f loc e
    return $ RawTheExpr e'
  mapMetaM f loc (RawAliasExpr b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ RawAliasExpr b' a'
  mapMetaM f loc (RawWhere m b a) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ RawWhere m' b' a'
  mapMetaM f loc (RawTupleApply m (bm, be) args) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
    be' <- mapMetaM f loc be
    args' <- mapM aux args
    return $ RawTupleApply m' (bm', be') args'
    where
      aux (b, a) = do
        a' <- mapMetaRawObjArrM f (Just loc) a
        return (b, a')
  mapMetaM f loc (RawVarsApply m be vars) = do
    m' <- f (ExprMeta loc ExprMetaApplyVar) m
    be' <- mapMetaM f loc be
    vars' <- mapM (mapMetaRawObjArrM f (Just loc)) vars
    return $ RawVarsApply m' be' vars'
  mapMetaM f loc (RawContextApply m (bm, be) args) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
    be' <- mapMetaM f loc be
    args' <- mapM (mapMetaRawObjArrM f (Just loc)) args
    return $ RawContextApply m' (bm', be') args'
  mapMetaM f loc (RawParen e) = RawParen <$> mapMetaM f loc e
  mapMetaM f loc (RawMethod m b mt) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    b' <- mapMetaM f loc b
    mt' <- mapMetaM f loc mt
    return $ RawMethod m' b' mt'
  mapMetaM f loc (RawList m lst) = do
    m' <- f (ExprMeta loc ExprMetaTupleArg) m
    lst' <- mapM (mapMetaM f loc) lst
    return $ RawList m' lst'
  mapMetaM f loc (RawTypeProp m b (TypePropProj p v)) = do
    m' <- f (ExprMeta loc ExprMetaTypeProp) m
    b' <- mapMetaM f loc b
    v' <- mapMetaM f loc v
    return $ RawTypeProp m' b' (TypePropProj p v')
  mapMetaM f loc (RawTypeProp m b (TypePropRel p v)) = do
    m' <- f (ExprMeta loc ExprMetaTypeProp) m
    b' <- mapMetaM f loc b
    v' <- mapMetaM f loc v
    return $ RawTypeProp m' b' (TypePropRel p v')

mapRawApplyM :: (Monad n, MapMeta e) => MetaFun n a b -> RawApply e a -> n (RawApply e b)
mapRawApplyM f (RawApply terms) = RawApply <$> mapM mapTerm terms
  where
    mapTerm (RATermDeep e)  = RATermDeep <$> mapMetaM f ApplyMeta e
    mapTerm (RATermChild e) = RATermChild <$> mapMetaM f ApplyMeta e

mapMetaRawObjArrM :: (Monad n, MapMeta e) => MetaFun n a b -> Maybe MetaLocation -> RawObjArr e a -> n (RawObjArr e b)
mapMetaRawObjArrM f mloc roa@RawObjArr{roaObj, roaAnnots, roaArr, roaDef} = do
  roaObj' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) roaObj
  roaAnnots' <- mapM (mapMetaM f (fromMaybe AnnotMeta mloc)) roaAnnots
  roaArr' <- forM roaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrE
    arrM' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrM
    return (arrE', arrM')
  roaDef' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) roaDef
  return roa{roaObj=roaObj', roaAnnots=roaAnnots', roaArr=roaArr', roaDef=roaDef'}

mapMetaRawStatementM :: (Monad n, MapMeta e) => MetaFun n a b -> RawStatement e a -> n (RawStatement e b)
mapMetaRawStatementM f (RawDeclStatement objArr) = RawDeclStatement <$> mapMetaRawObjArrM f Nothing objArr
mapMetaRawStatementM f (RawBindStatement objArr) = RawBindStatement  <$> mapMetaRawObjArrM f Nothing objArr
mapMetaRawStatementM f (RawAnnot e) = RawAnnot <$> mapMetaM f AnnotMeta e

mapMetaRawStatementTreeM :: (Monad n, MapMeta e) => MetaFun n a b -> RawStatementTree e a -> n (RawStatementTree e b)
mapMetaRawStatementTreeM f (RawStatementTree s tree) = do
  s' <- mapMetaRawStatementM f s
  tree' <- mapM (mapMetaRawStatementTreeM f) tree
  return $ RawStatementTree s' tree'

mapMetaRawPrgmM :: Monad n => MetaFun n a b -> RawPrgm a -> n (RawPrgm b)
mapMetaRawPrgmM f (RawPrgm imports statementTrees) = do
  statementTrees' <- mapM (mapMetaRawStatementTreeM f) statementTrees
  return (RawPrgm imports statementTrees')

mapMetaRawPrgm :: MetaFun Identity a b -> RawPrgm a -> RawPrgm b
mapMetaRawPrgm f p = runIdentity $ mapMetaRawPrgmM f p
