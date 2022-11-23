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

data MetaType
  = ExprMeta ExprMetaType
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
  mapMeta :: MetaFun a b -> m a -> m b


--

mapMetaAppliedExpr :: MetaFun m m -> Expr m -> Expr m
mapMetaAppliedExpr f (CExpr m c) = CExpr (f (ExprMeta ExprMetaConstant) m) c
mapMetaAppliedExpr f (Value m n) = Value (f (ExprMeta ExprMetaVal) m) n
mapMetaAppliedExpr f (Arg m n) = Arg (f (ExprMeta ExprMetaArg) m) n
mapMetaAppliedExpr f (HoleExpr m h) = HoleExpr (f (ExprMeta ExprMetaHole) m) h
mapMetaAppliedExpr f (TupleApply m (bm, be) arg) = TupleApply (f (ExprMeta ExprMetaApplyArg) m) (f (ExprMeta ExprMetaApplyArgBase) bm, mapMetaAppliedExpr f be) (mapMetaAppliedExprTupleArg f arg)
  where
    mapMetaAppliedExprTupleArg :: MetaFun m m -> TupleArg Expr m -> TupleArg Expr m
    mapMetaAppliedExprTupleArg f2 (TupleArgI argM argName) = TupleArgI (f2 (ExprMeta ExprMetaTupleArg) argM) argName
    mapMetaAppliedExprTupleArg f2 (TupleArgO argM argVal) = TupleArgO (f2 (ExprMeta ExprMetaTupleArg) argM) argVal
    mapMetaAppliedExprTupleArg f2 (TupleArgIO argM argName argVal) = TupleArgIO (f2 (ExprMeta ExprMetaTupleArg) argM) argName argVal
mapMetaAppliedExpr f (VarApply m be varName varVal) = VarApply (f (ExprMeta ExprMetaApplyVar) m) (mapMetaAppliedExpr f be) varName (f (ExprMeta ExprMetaApplyVarVal) varVal)

mapMetaTupleArg :: (MapMeta e) => MetaFun a b -> TupleArg e a -> TupleArg e b
mapMetaTupleArg f (TupleArgI m argName) = TupleArgI (f (ExprMeta ExprMetaTupleArg) m) argName
mapMetaTupleArg f (TupleArgO m argVal) = TupleArgO (f (ExprMeta ExprMetaTupleArg) m) (mapMeta f argVal)
mapMetaTupleArg f (TupleArgIO m argName argVal) = TupleArgIO (f (ExprMeta ExprMetaTupleArg) m) argName (mapMeta f argVal)

instance MapMeta Expr where
  mapMeta f (CExpr m c) = CExpr (f (ExprMeta ExprMetaConstant) m) c
  mapMeta f (Value m n) = Value (f (ExprMeta ExprMetaVal) m) n
  mapMeta f (Arg m n) = Arg (f (ExprMeta ExprMetaArg) m) n
  mapMeta f (HoleExpr m h) = HoleExpr (f (ExprMeta ExprMetaHole) m) h
  mapMeta f (TupleApply m (bm, be) arg) = TupleApply (f (ExprMeta ExprMetaApplyArg) m) (f (ExprMeta ExprMetaApplyArgBase) bm, mapMeta f be) (mapMetaTupleArg f arg)
  mapMeta f (VarApply m be varName varVal) = VarApply (f (ExprMeta ExprMetaApplyVar) m) (mapMeta f be) varName (f (ExprMeta ExprMetaApplyVarVal) varVal)

mapMetaGuard :: (MapMeta e) => MetaFun a b -> Guard (e a) -> Guard (e b)
mapMetaGuard f (IfGuard expr) = IfGuard (mapMeta f expr)
mapMetaGuard _ ElseGuard      = ElseGuard
mapMetaGuard _ NoGuard        = NoGuard

mapMetaObjArg :: MetaFun a b -> ObjArg a -> ObjArg b
mapMetaObjArg f (m, maybeObj) = (f ObjArgMeta m, fmap (mapMeta f) maybeObj)

instance MapMeta Object where
  mapMeta f (Object m basis vars args doc path) = Object (f ObjMeta m) basis (fmap (f ObjVarMeta) vars) (fmap (mapMetaObjArg f) args) doc path

mapMetaArrow :: (MapMeta e) => MetaFun a b -> Arrow e a -> Arrow e b
mapMetaArrow f (Arrow m guard maybeExpr) = Arrow (f ArrMeta m) (mapMetaGuard f guard) (fmap (mapMeta f) maybeExpr)

mapMetaObjectMap :: (MapMeta e) => MetaFun a b -> ObjectMap e a -> ObjectMap e b
mapMetaObjectMap f = map aux
  where aux (obj, annots, arrow) = (mapMeta f obj, fmap (mapMeta f) annots, fmap (mapMetaArrow f) arrow)

mapMetaPrgm :: (MapMeta e) => MetaFun a b -> Prgm e a -> Prgm e b
mapMetaPrgm f (objMap, classGraph, annots) = (mapMetaObjectMap f objMap, classGraph, map (mapMeta f) annots)
