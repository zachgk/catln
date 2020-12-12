--------------------------------------------------------------------
-- |
-- Module    :  MapMeta
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module MapMeta where

import           Syntax.Prgm

data MetaType
  = ExprMeta
  | ObjMeta
  | ObjArgMeta
  | ObjVarMeta
  | ArrMeta
  deriving (Eq, Ord, Show)

class MapMeta m where
  mapMeta :: (MetaType -> a -> b) -> m a -> m b


--

instance MapMeta IExpr where
  mapMeta f (ICExpr m c) = ICExpr (f ExprMeta m) c
  mapMeta f (IValue m n) = IValue (f ExprMeta m) n
  mapMeta f (IArg m n) = IArg (f ExprMeta m) n
  mapMeta f (ITupleApply m (bm, be) argName argVal) = ITupleApply (f ExprMeta m) (f ExprMeta bm, mapMeta f be) argName (mapMeta f argVal)

instance MapMeta Expr where
  mapMeta f (CExpr m c) = CExpr (f ExprMeta m) c
  mapMeta f (Value m n) = Value (f ExprMeta m) n
  mapMeta f (Arg m n) = Arg (f ExprMeta m) n
  mapMeta f (TupleApply m (bm, be) argName argVal) = TupleApply (f ExprMeta m) (f ExprMeta bm, mapMeta f be) argName (mapMeta f argVal)

mapMetaGuard :: (MapMeta e) => (MetaType -> a -> b) -> Guard (e a) -> Guard (e b)
mapMetaGuard f (IfGuard expr) = IfGuard (mapMeta f expr)
mapMetaGuard _ ElseGuard = ElseGuard
mapMetaGuard _ NoGuard = NoGuard

mapMetaObjArg :: (MetaType -> a -> b) -> ObjArg a -> ObjArg b
mapMetaObjArg f (m, maybeObj) = (f ObjArgMeta m, fmap (mapMeta f) maybeObj)

instance MapMeta Object where
  mapMeta f (Object m basis name vars args) = Object (f ObjMeta m) basis name (fmap (f ObjVarMeta) vars) (fmap (mapMetaObjArg f) args)

mapMetaArrow :: (MapMeta e) => (MetaType -> a -> b) -> Arrow (e a) a -> Arrow (e b) b
mapMetaArrow f (Arrow m annots guard maybeExpr) = Arrow (f ArrMeta m) (map (mapMeta f) annots) (mapMetaGuard f guard) (fmap (mapMeta f) maybeExpr)

mapMetaObjectMap :: (MapMeta e) => (MetaType -> a -> b) -> ObjectMap (e a) a -> ObjectMap (e b) b
mapMetaObjectMap f = map aux
  where aux (obj, arrows) = (mapMeta f obj, map (mapMetaArrow f) arrows)

mapMetaPrgm :: (MapMeta e) => (MetaType -> a -> b) -> Prgm (e a) a -> Prgm (e b) b
mapMetaPrgm f (objMap, classMap, annots) = (mapMetaObjectMap f objMap, classMap, map (mapMeta f) annots)
