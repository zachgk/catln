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

import           Semantics.Prgm

data MetaLocation
  = OutputMeta
  | InputMeta
  | AnnotMeta
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
  | ExprMetaArg
  | ExprMetaHole
  | ExprMetaApplyArg
  | ExprMetaApplyArgBase
  | ExprMetaTupleArg
  | ExprMetaApplyVar
  | ExprMetaApplyVarVal
  deriving (Eq, Ord, Show)

type MetaFun a b = MetaType -> Meta a -> Meta b

class MapMeta m where
  mapMeta :: MetaFun a b -> MetaLocation -> m a -> m b


clearMetaDat :: MetaFun a ()
clearMetaDat _ (Meta p l _) = Meta p l ()

--

mapMetaAppliedExpr :: MetaFun m m -> MetaLocation -> Expr m -> Expr m
mapMetaAppliedExpr f loc (CExpr m c) = CExpr (f (ExprMeta loc ExprMetaConstant) m) c
mapMetaAppliedExpr f loc (Value m n) = Value (f (ExprMeta loc ExprMetaVal) m) n
mapMetaAppliedExpr f loc (Arg m n) = Arg (f (ExprMeta loc ExprMetaArg) m) n
mapMetaAppliedExpr f loc (HoleExpr m h) = HoleExpr (f (ExprMeta loc ExprMetaHole) m) h
mapMetaAppliedExpr f loc (AliasExpr b a) = AliasExpr (mapMetaAppliedExpr f loc b) (mapMetaAppliedExpr f loc a)
mapMetaAppliedExpr f loc (TupleApply m (bm, be) arg) = TupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMetaAppliedExpr f loc be) (mapMetaAppliedExprTupleArg f arg)
  where
    mapMetaAppliedExprTupleArg :: MetaFun m m -> TupleArg Expr m -> TupleArg Expr m
    mapMetaAppliedExprTupleArg f2 (TupleArgI argM argName) = TupleArgI (f2 (ExprMeta loc ExprMetaTupleArg) argM) argName
    mapMetaAppliedExprTupleArg f2 (TupleArgO argM argVal) = TupleArgO (f2 (ExprMeta loc ExprMetaTupleArg) argM) argVal
    mapMetaAppliedExprTupleArg f2 (TupleArgIO argM argName argVal) = TupleArgIO (f2 (ExprMeta loc ExprMetaTupleArg) argM) argName argVal
mapMetaAppliedExpr f loc (VarApply m be varName varVal) = VarApply (f (ExprMeta loc ExprMetaApplyVar) m) (mapMetaAppliedExpr f loc be) varName (f (ExprMeta loc ExprMetaApplyVarVal) varVal)

mapMetaTupleArg :: (MapMeta e) => MetaFun a b -> MetaLocation -> TupleArg e a -> TupleArg e b
mapMetaTupleArg f loc (TupleArgI m argName) = TupleArgI (f (ExprMeta loc ExprMetaTupleArg) m) argName
mapMetaTupleArg f loc (TupleArgO m argVal) = TupleArgO (f (ExprMeta loc ExprMetaTupleArg) m) (mapMeta f loc argVal)
mapMetaTupleArg f loc (TupleArgIO m argName argVal) = TupleArgIO (f (ExprMeta loc ExprMetaTupleArg) m) argName (mapMeta f loc argVal)

instance MapMeta Expr where
  mapMeta f loc (CExpr m c) = CExpr (f (ExprMeta loc ExprMetaConstant) m) c
  mapMeta f loc (Value m n) = Value (f (ExprMeta loc ExprMetaVal) m) n
  mapMeta f loc (Arg m n) = Arg (f (ExprMeta loc ExprMetaArg) m) n
  mapMeta f loc (HoleExpr m h) = HoleExpr (f (ExprMeta loc ExprMetaHole) m) h
  mapMeta f loc (AliasExpr b a) = AliasExpr (mapMeta f loc b) (mapMeta f loc a)
  mapMeta f loc (TupleApply m (bm, be) arg) = TupleApply (f (ExprMeta loc ExprMetaApplyArg) m) (f (ExprMeta loc ExprMetaApplyArgBase) bm, mapMeta f loc be) (mapMetaTupleArg f loc arg)
  mapMeta f loc (VarApply m be varName varVal) = VarApply (f (ExprMeta loc ExprMetaApplyVar) m) (mapMeta f loc be) varName (f (ExprMeta loc ExprMetaApplyVarVal) varVal)

mapMetaObjArg :: (MapMeta e) => MetaFun a b -> ObjArg e a -> ObjArg e b
mapMetaObjArg f (m, maybeObj) = (f ObjArgMeta m, fmap (mapMeta f InputMeta) maybeObj)

instance (MapMeta e) => MapMeta (Object e) where
  mapMeta f _ (Object m basis vars args doc e path) = Object (f ObjMeta m) basis (fmap (f ObjVarMeta) vars) (fmap (mapMetaObjArg f) args) doc (mapMeta f InputMeta e) path

mapMetaArrow :: (MapMeta e) => MetaFun a b -> Arrow e a -> Arrow e b
mapMetaArrow f (Arrow m guard maybeExpr) = Arrow (f ArrMeta m) (fmap (mapMeta f GuardMeta) guard) (fmap (mapMeta f OutputMeta) maybeExpr)

mapMetaObjectMap :: (MapMeta e) => MetaFun a b -> ObjectMap e a -> ObjectMap e b
mapMetaObjectMap f = map aux
  where aux (obj, annots, arrow) = (mapMeta f InputMeta obj, fmap (mapMeta f AnnotMeta) annots, fmap (mapMetaArrow f) arrow)

mapMetaPrgm :: (MapMeta e) => MetaFun a b -> Prgm e a -> Prgm e b
mapMetaPrgm f (objMap, classGraph, annots) = (mapMetaObjectMap f objMap, classGraph, map (mapMeta f AnnotMeta) annots)

mapMetaExprObj :: (MapMeta e) => MetaFun a b -> ExprObject e a -> ExprObject e b
mapMetaExprObj f (ExprObject basis doc expr) = ExprObject basis doc (mapMeta f InputMeta expr)

mapMetaExprObjectMap :: (MapMeta e) => MetaFun a b -> ExprObjectMap e a -> ExprObjectMap e b
mapMetaExprObjectMap f = map aux
  where aux (obj, annots, arrow) = (mapMetaExprObj f obj, fmap (mapMeta f AnnotMeta) annots, fmap (mapMetaArrow f) arrow)

mapMetaExprPrgm :: (MapMeta e) => MetaFun a b -> ExprPrgm e a -> ExprPrgm e b
mapMetaExprPrgm f (objMap, classGraph, annots) = (mapMetaExprObjectMap f objMap, classGraph, map (mapMeta f AnnotMeta) annots)
