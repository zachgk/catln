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

import           Data.Bifunctor      (Bifunctor (bimap))
import qualified Data.HashMap.Strict as H
import           Data.Maybe          (fromMaybe)
import           Semantics
import           Semantics.Prgm

data MetaLocation
  = OutputMeta
  | InputMeta
  | AnnotMeta
  | ApplyMeta
  | GuardMeta
  deriving (Eq, Ord, Show)

data MetaType
  = ExprMeta MetaLocation ExprMetaType
  | ObjMeta
  | ObjArgMeta
  | ObjVarMeta
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
  | ExprMetaTypeProp
  deriving (Eq, Ord, Show)

type MetaFun a b = MetaType -> Meta a -> Meta b

class MapMeta m where
  mapMeta :: MetaFun a b -> MetaLocation -> m a -> m b


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

mapMetaAppliedExpr :: (MetaDat m, Show m) => MetaFun m m -> MetaLocation -> Expr m -> Expr m
mapMetaAppliedExpr f loc (CExpr m c) = CExpr (f (ExprMeta loc ExprMetaConstant) m) c
mapMetaAppliedExpr f loc (Value m n) = Value (f (ExprMeta loc ExprMetaVal) m) n
mapMetaAppliedExpr f loc (HoleExpr m h) = HoleExpr (f (ExprMeta loc ExprMetaHole) m) h
mapMetaAppliedExpr f loc (AliasExpr b a) = AliasExpr (mapMetaAppliedExpr f loc b) (mapMetaAppliedExpr f loc a)
mapMetaAppliedExpr f loc (TupleApply m (bm, be) arg) = TupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMetaAppliedExpr f loc be) (mapOAObjExpr (mapMeta f loc) arg)
mapMetaAppliedExpr f loc (VarApply m be varName varVal) = VarApply (f (ExprMeta loc ExprMetaApplyVar) m) (mapMetaAppliedExpr f loc be) varName (f (ExprMeta loc ExprMetaApplyVarVal) varVal)

instance MapMeta Expr where
  mapMeta f loc (CExpr m c) = CExpr (f (ExprMeta loc ExprMetaConstant) m) c
  mapMeta f loc (Value m n) = Value (f (ExprMeta loc ExprMetaVal) m) n
  mapMeta f loc (HoleExpr m h) = HoleExpr (f (ExprMeta loc ExprMetaHole) m) h
  mapMeta f loc (AliasExpr b a) = AliasExpr (mapMeta f loc b) (mapMeta f loc a)
  mapMeta f loc (TupleApply m (bm, be) arg) = TupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMeta f loc be) (mapMetaObjArr f (Just loc) arg)
  mapMeta f loc (VarApply m be varName varVal) = VarApply (f (ExprMeta loc ExprMetaApplyVar) m) (mapMeta f loc be) varName (f (ExprMeta loc ExprMetaApplyVarVal) varVal)

mapMetaGuardExpr :: (MapMeta e) => MetaFun a b -> MetaLocation -> GuardExpr e a -> GuardExpr e b
mapMetaGuardExpr f loc (GuardExpr expr guard) = GuardExpr (mapMeta f loc expr) (fmap (mapMeta f GuardMeta) guard)

mapMetaObjArr :: (MapMeta e) => MetaFun a b -> Maybe MetaLocation -> ObjArr e a -> ObjArr e b
mapMetaObjArr f mloc oa@ObjArr{oaObj, oaAnnots, oaArr} = oa{
  oaObj = fmap (mapMetaGuardExpr f (fromMaybe InputMeta mloc)) oaObj,
  oaAnnots = map (mapMeta f (fromMaybe AnnotMeta mloc)) oaAnnots,
  oaArr = bimap (fmap (mapMetaGuardExpr f (fromMaybe OutputMeta mloc))) (f ArrMeta) oaArr
                                                             }

mapMetaPrgm :: (MapMeta e) => MetaFun a b -> Prgm e a -> Prgm e b
mapMetaPrgm f (objMap, classGraph, annots) = (map (mapMetaObjArr f Nothing) objMap, classGraph, map (mapMeta f AnnotMeta) annots)
