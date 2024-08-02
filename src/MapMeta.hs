--------------------------------------------------------------------
-- |
-- Module    :  MapMeta
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- MapMeta is a utility to update the metadata across a program.
--------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module MapMeta where

import           Control.Monad
import           Control.Monad.Identity
import qualified Data.HashMap.Strict    as H
import           Data.Maybe             (fromMaybe)
import           Semantics.Prgm

data MetaLocation
  = OutputMeta
  | InputMeta
  | AnnotMeta
  | ApplyMeta
  | GuardMeta
  | ImportMeta
  deriving (Eq, Ord, Show)

data MetaType
  = ExprMeta MetaLocation ExprMetaType
  | ArrMeta
  deriving (Eq, Ord, Show)

data ExprMetaType
  = ExprMetaConstant
  | ExprMetaVal
  | ExprMetaHole
  | ExprMetaMacroVal
  | ExprMetaApplyArg
  | ExprMetaApplyArgBase
  | ExprMetaTupleArg
  | ExprMetaApplyVar
  | ExprMetaApplyVarVal
  | ExprMetaWhere
  | ExprMetaTypeProp
  deriving (Eq, Ord, Show)

type MetaFun a b = MetaType -> Meta a -> Meta b

class MapMeta m where
  mapMetaM :: Monad n => MetaFun a b -> MetaLocation -> m a -> n (m b)


clearMetaDat :: MetaFun a ()
clearMetaDat _ (Meta p l _) = Meta p l ()

interleaveMeta :: H.HashMap CodeRangeDat a -> MetaFun () (Maybe a)
interleaveMeta dat _ (Meta t p _) = Meta t p (p >>= (`H.lookup` dat))

zipMetaFun :: MetaFun a b -> MetaFun a c -> MetaFun a (b, c)
zipMetaFun f1 f2 tp m@(Meta t p _) = Meta t p (db, dc)
  where
    (Meta _ _ db) = f1 tp m
    (Meta _ _ dc) = f2 tp m

--

mapMetaAppliedExprM :: (Monad n, MetaDat m, Show m) => MetaFun m m -> MetaLocation -> Expr m -> n (Expr m)
mapMetaAppliedExprM f loc (CExpr m c) = return $ CExpr (f (ExprMeta loc ExprMetaConstant) m) c
mapMetaAppliedExprM f loc (Value m n) = return $ Value (f (ExprMeta loc ExprMetaVal) m) n
mapMetaAppliedExprM f loc (HoleExpr m h) = return $ HoleExpr (f (ExprMeta loc ExprMetaHole) m) h
mapMetaAppliedExprM f loc (AliasExpr b a) = do
  b' <- mapMetaAppliedExprM f loc b
  a' <- mapMetaAppliedExprM f loc a
  return $ AliasExpr b' a'
mapMetaAppliedExprM f loc (EWhere m b a) = do
  b' <- mapMetaAppliedExprM f loc b
  a' <- mapMetaAppliedExprM f loc a
  return $ EWhere (f (ExprMeta loc ExprMetaWhere) m) b' a'
mapMetaAppliedExprM f loc (TupleApply m (bm, be) arg) = do
  be' <- mapMetaAppliedExprM f loc be
  arg' <- mapArg arg
  return $ TupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, be') arg'
    where
      mapArg (EAppArg a)    = do
        obj' <- case oaObj a of
          Just o -> do
            o' <- mapMetaM f loc o
            return $ Just o'
          Nothing -> return Nothing
        let a' = a{oaObj=obj'}
        return $ EAppArg a'
      mapArg (EAppSpread a) = do
        a' <- mapMetaAppliedExprM f loc a
        return $ EAppSpread a'
mapMetaAppliedExprM f loc (VarApply m be varName varVal) = do
  be' <- mapMetaAppliedExprM f loc be
  return $ VarApply (f (ExprMeta loc ExprMetaApplyVar) m) be' varName (f (ExprMeta loc ExprMetaApplyVarVal) varVal)

instance MapMeta Expr where
  mapMetaM f loc (CExpr m c) = return $ CExpr (f (ExprMeta loc ExprMetaConstant) m) c
  mapMetaM f loc (Value m n) = return $ Value (f (ExprMeta loc ExprMetaVal) m) n
  mapMetaM f loc (HoleExpr m h) = return $ HoleExpr (f (ExprMeta loc ExprMetaHole) m) h
  mapMetaM f loc (AliasExpr b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ AliasExpr b' a'
  mapMetaM f loc (EWhere m b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ EWhere (f (ExprMeta loc ExprMetaWhere) m) b' a'
  mapMetaM f loc (TupleApply m (bm, be) arg) = do
    be' <- mapMetaM f loc be
    arg' <- mapArg f arg
    return $ TupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, be') arg'
    where
      mapArg :: (Monad n, MapMeta e) => MetaFun a b -> EApp e a -> n (EApp e b)
      mapArg f' (EAppArg a)    = do
        a' <- mapMetaObjArrM f' (Just loc) a
        return $ EAppArg a'
      mapArg f' (EAppSpread a) = do
        a' <- mapMetaM f' loc a
        return $ EAppSpread a'
  mapMetaM f loc (VarApply m be varName varVal) = do
    be' <- mapMetaM f loc be
    return $ VarApply (f (ExprMeta loc ExprMetaApplyVar) m) be' varName (f (ExprMeta loc ExprMetaApplyVarVal) varVal)

mapMetaObjArrM :: (Monad n, MapMeta e) => MetaFun a b -> Maybe MetaLocation -> ObjArr e a -> n (ObjArr e b)
mapMetaObjArrM f mloc oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) oaObj
  oaAnnots' <- mapM (mapMetaM f (fromMaybe AnnotMeta mloc)) oaAnnots
  oaArr' <- forM  oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrE
    let arrM' = f ArrMeta arrM
    return (arrE', arrM')
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

mapMetaPrgmM :: (Monad n, MapMeta e) => MetaFun a b -> Prgm e a -> n (Prgm e b)
mapMetaPrgmM f (objMap, classGraph, annots) = do
  objMap' <- mapM (mapMetaObjArrM f Nothing) objMap
  annots' <- mapM (mapMetaM f AnnotMeta) annots
  return (objMap', classGraph, annots')

mapMeta :: (MapMeta m) => MetaFun a b -> MetaLocation -> m a -> m b
mapMeta f loc a = runIdentity $ mapMetaM f loc a

mapMetaAppliedExpr :: (MetaDat m, Show m) => MetaFun m m -> MetaLocation -> Expr m -> Expr m
mapMetaAppliedExpr f loc e = runIdentity $ mapMetaAppliedExprM f loc e

mapMetaObjArr :: (MapMeta e) => MetaFun a b -> Maybe MetaLocation -> ObjArr e a -> ObjArr e b
mapMetaObjArr f loc oa = runIdentity $ mapMetaObjArrM f loc oa

mapMetaPrgm :: (MapMeta e) => MetaFun a b -> Prgm e a -> Prgm e b
mapMetaPrgm f p = runIdentity $ mapMetaPrgmM f p
