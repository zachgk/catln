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
import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.HashMap.Strict        as H
import           Data.Maybe                 (fromMaybe)
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
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

type MetaFun n a b = MetaType -> Meta a -> n (Meta b)

class MapMeta m where
  mapMetaM :: Monad n => MetaFun n a b -> MetaLocation -> m a -> n (m b)


clearMetaDat :: (Monad n) => MetaFun n a ()
clearMetaDat _ (Meta p l mid _) = return $ Meta p l mid ()

interleaveMeta :: (Monad n) => H.HashMap UUID.UUID a -> MetaFun n () (Maybe a)
interleaveMeta dat _ (Meta t p mid _) = return $ Meta t p mid (H.lookup mid dat)

interleavePrgm :: Prgm Expr m -> H.HashMap UUID.UUID (Meta m)
interleavePrgm prgm = H.fromList $ execWriter $ mapMetaPrgmM f prgm
  where
    f _ m = do
        tell [(getMetaID m, m)]
        return m

addMetaID :: MetaFun IO m m
addMetaID _ m = if UUID.null $ getMetaID m
  then do
    id' <- UUID.nextRandom
    return m{getMetaID=id'}
  else return m

zipMetaFun :: (Monad n) => MetaFun n a b -> MetaFun n a c -> MetaFun n a (b, c)
zipMetaFun f1 f2 tp m@(Meta t p mid _) = do
  (Meta _ _ _ db) <- f1 tp m
  (Meta _ _ _ dc) <- f2 tp m
  return $ Meta t p mid (db, dc)

mapMetaAppliedExprM :: (Monad n, MetaDat m, Show m) => MetaFun n m m -> MetaLocation -> Expr m -> n (Expr m)
mapMetaAppliedExprM f loc (CExpr m c) = do
  m' <- f (ExprMeta loc ExprMetaConstant) m
  return $ CExpr m' c
mapMetaAppliedExprM f loc (Value m n) = do
  m' <- f (ExprMeta loc ExprMetaVal) m
  return $ Value m' n
mapMetaAppliedExprM f loc (HoleExpr m h) = do
  m' <- f (ExprMeta loc ExprMetaHole) m
  return $ HoleExpr m' h
mapMetaAppliedExprM f loc (AliasExpr b a) = do
  b' <- mapMetaAppliedExprM f loc b
  a' <- mapMetaAppliedExprM f loc a
  return $ AliasExpr b' a'
mapMetaAppliedExprM f loc (EWhere m b a) = do
  m' <- f (ExprMeta loc ExprMetaWhere) m
  b' <- mapMetaAppliedExprM f loc b
  a' <- mapMetaAppliedExprM f loc a
  return $ EWhere m' b' a'
mapMetaAppliedExprM f loc (TupleApply m (bm, be) arg) = do
  m' <- f (ExprMeta loc ExprMetaApplyArg) m
  bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
  be' <- mapMetaAppliedExprM f loc be
  arg' <- mapArg arg
  return $ TupleApply m' (bm', be') arg'
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
  m' <- f (ExprMeta loc ExprMetaApplyVar) m
  be' <- mapMetaAppliedExprM f loc be
  varVal' <- f (ExprMeta loc ExprMetaApplyVarVal) varVal
  return $ VarApply m' be' varName varVal'

instance MapMeta Expr where
  mapMetaM f loc (CExpr m c) = do
    m' <- f (ExprMeta loc ExprMetaConstant) m
    return $ CExpr m' c
  mapMetaM f loc (Value m n) = do
    m' <- f (ExprMeta loc ExprMetaVal) m
    return $ Value m' n
  mapMetaM f loc (HoleExpr m h) = do
    m' <- f (ExprMeta loc ExprMetaHole) m
    return $ HoleExpr m' h
  mapMetaM f loc (AliasExpr b a) = do
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ AliasExpr b' a'
  mapMetaM f loc (EWhere m b a) = do
    m' <- f (ExprMeta loc ExprMetaWhere) m
    b' <- mapMetaM f loc b
    a' <- mapMetaM f loc a
    return $ EWhere m' b' a'
  mapMetaM f loc (TupleApply m (bm, be) arg) = do
    m' <- f (ExprMeta loc ExprMetaApplyArg) m
    bm' <- f (ExprMeta loc ExprMetaApplyArgBase) bm
    be' <- mapMetaM f loc be
    arg' <- mapArg f arg
    return $ TupleApply m' (bm', be') arg'
    where
      mapArg :: (Monad n, MapMeta e) => MetaFun n a b -> EApp e a -> n (EApp e b)
      mapArg f' (EAppArg a)    = do
        a' <- mapMetaObjArrM f' (Just loc) a
        return $ EAppArg a'
      mapArg f' (EAppSpread a) = do
        a' <- mapMetaM f' loc a
        return $ EAppSpread a'
  mapMetaM f loc (VarApply m be varName varVal) = do
    m' <- f (ExprMeta loc ExprMetaApplyVar) m
    be' <- mapMetaM f loc be
    varVal' <- f (ExprMeta loc ExprMetaApplyVarVal) varVal
    return $ VarApply m' be' varName varVal'

mapMetaObjArrM :: (Monad n, MapMeta e) => MetaFun n a b -> Maybe MetaLocation -> ObjArr e a -> n (ObjArr e b)
mapMetaObjArrM f mloc oa@ObjArr{oaObj, oaAnnots, oaArr} = do
  oaObj' <- mapM (mapMetaM f (fromMaybe InputMeta mloc)) oaObj
  oaAnnots' <- mapM (mapMetaM f (fromMaybe AnnotMeta mloc)) oaAnnots
  oaArr' <- forM  oaArr $ \(arrE, arrM) -> do
    arrE' <- mapM (mapMetaM f (fromMaybe OutputMeta mloc)) arrE
    arrM' <- f ArrMeta arrM
    return (arrE', arrM')
  return oa{oaObj=oaObj', oaAnnots=oaAnnots', oaArr=oaArr'}

mapMetaPrgmM :: (Monad n, MapMeta e) => MetaFun n a b -> Prgm e a -> n (Prgm e b)
mapMetaPrgmM f (objMap, classGraph, annots) = do
  objMap' <- mapM (mapMetaObjArrM f Nothing) objMap
  annots' <- mapM (mapMetaM f AnnotMeta) annots
  return (objMap', classGraph, annots')

mapMeta :: (MapMeta m) => MetaFun Identity a b -> MetaLocation -> m a -> m b
mapMeta f loc a = runIdentity $ mapMetaM f loc a

mapMetaAppliedExpr :: (MetaDat m, Show m) => MetaFun Identity m m -> MetaLocation -> Expr m -> Expr m
mapMetaAppliedExpr f loc e = runIdentity $ mapMetaAppliedExprM f loc e

mapMetaObjArr :: (MapMeta e) => MetaFun Identity a b -> Maybe MetaLocation -> ObjArr e a -> ObjArr e b
mapMetaObjArr f loc oa = runIdentity $ mapMetaObjArrM f loc oa

mapMetaPrgm :: (MapMeta e) => MetaFun Identity a b -> Prgm e a -> Prgm e b
mapMetaPrgm f p = runIdentity $ mapMetaPrgmM f p
