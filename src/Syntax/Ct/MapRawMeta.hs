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
  mapMetaM f loc (RawCExpr m c) = return $ RawCExpr (f (ExprMeta loc ExprMetaConstant) m) c
  mapMetaM f loc (RawValue m n) = return $ RawValue (f (ExprMeta loc ExprMetaVal) m) n
  mapMetaM f loc (RawHoleExpr m h) = return $ RawHoleExpr (f (ExprMeta loc ExprMetaHole) m) h
  mapMetaM f loc (RawMacroValue m n) = return $ RawMacroValue (f (ExprMeta loc ExprMetaMacroVal) m) n
  mapMetaM f loc (RawApplyExpr m n) = do
    n' <- mapRawApplyM f n
    return $ RawApplyExpr (f (ExprMeta loc ExprMetaMacroVal) m) n'
  mapMetaM f loc (RawTheExpr e) = do
    e' <- mapMetaM f loc e
    return $ RawTheExpr e'
  mapMetaM f loc (RawAliasExpr b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ RawAliasExpr b' a'
  mapMetaM f loc (RawWhere b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ RawWhere b' a'
  mapMetaM f loc (RawTupleApply m (bm, be) args) = do
    be' <- mapMetaM f loc be
    args' <- mapM aux args
    return $ RawTupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, be') args'
    where
      aux (b, a) = do
        a' <- mapMetaRawObjArrM f (Just loc) a
        return (b, a')
  mapMetaM f loc (RawVarsApply m be vars) = do
    be' <- mapMetaM f loc be
    vars' <- mapM (mapMetaRawObjArrM f (Just loc)) vars
    return $ RawVarsApply (f (ExprMeta loc ExprMetaApplyVar) m) be' vars'
  mapMetaM f loc (RawContextApply m (bm, be) args) = do
    be' <- mapMetaM f loc be
    args' <- mapM (mapMetaRawObjArrM f (Just loc)) args
    return $ RawContextApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, be') args'
  mapMetaM f loc (RawParen e) = RawParen <$> mapMetaM f loc e
  mapMetaM f loc (RawMethod b m) = do
    b' <- mapMetaM f loc b
    m' <- mapMetaM f loc m
    return $ RawMethod b' m'
  mapMetaM f loc (RawList m lst) = do
    lst' <- mapM (mapMetaM f loc) lst
    return $ RawList (f (ExprMeta loc ExprMetaTupleArg) m) lst'
  mapMetaM f loc (RawTypeProp m b (TypePropProj p v)) = do
    b' <- mapMetaM f loc b
    v' <- mapMetaM f loc v
    return $ RawTypeProp (f (ExprMeta loc ExprMetaTypeProp) m) b' (TypePropProj p v')
  mapMetaM f loc (RawTypeProp m b (TypePropRel p v)) = do
    b' <- mapMetaM f loc b
    v' <- mapMetaM f loc v
    return $ RawTypeProp (f (ExprMeta loc ExprMetaTypeProp) m) b' (TypePropRel p v')

mapRawApplyM :: (Monad n, MapMeta e) => MetaFun a b -> RawApply e a -> n (RawApply e b)
mapRawApplyM f (RawApply terms) = RawApply <$> mapM mapTerm terms
  where
    mapTerm (RATermDeep e)  = RATermDeep <$> mapMetaM f ApplyMeta e
    mapTerm (RATermChild e) = RATermChild <$> mapMetaM f ApplyMeta e

mapMetaRawObjArrM :: (Monad n, MapMeta e) => MetaFun a b -> Maybe MetaLocation -> RawObjArr e a -> n (RawObjArr e b)
mapMetaRawObjArrM f mloc roa@RawObjArr{roaObj, roaAnnots, roaArr, roaDef} = do
  roaObj' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) roaObj
  roaAnnots' <- mapM (mapMetaM f (fromMaybe AnnotMeta mloc)) roaAnnots
  roaArr' <- forM roaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrE
    arrM' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrM
    return (arrE', arrM')
  roaDef' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) roaDef
  return roa{roaObj=roaObj', roaAnnots=roaAnnots', roaArr=roaArr', roaDef=roaDef'}

mapMetaRawStatementM :: (Monad n, MapMeta e) => MetaFun a b -> RawStatement e a -> n (RawStatement e b)
mapMetaRawStatementM f (RawDeclStatement objArr) = RawDeclStatement <$> mapMetaRawObjArrM f Nothing objArr
mapMetaRawStatementM f (RawBindStatement objArr) = RawBindStatement  <$> mapMetaRawObjArrM f Nothing objArr
mapMetaRawStatementM f (RawAnnot e) = RawAnnot <$> mapMetaM f AnnotMeta e

mapMetaRawStatementTreeM :: (Monad n, MapMeta e) => MetaFun a b -> RawStatementTree e a -> n (RawStatementTree e b)
mapMetaRawStatementTreeM f (RawStatementTree s tree) = do
  s' <- mapMetaRawStatementM f s
  tree' <- mapM (mapMetaRawStatementTreeM f) tree
  return $ RawStatementTree s' tree'

mapMetaRawPrgmM :: Monad n => MetaFun a b -> RawPrgm a -> n (RawPrgm b)
mapMetaRawPrgmM f (imports, statementTrees) = do
  statementTrees' <- mapM (mapMetaRawStatementTreeM f) statementTrees
  return (imports, statementTrees')

mapMetaRawPrgm :: MetaFun a b -> RawPrgm a -> RawPrgm b
mapMetaRawPrgm f p = runIdentity $ mapMetaRawPrgmM f p
