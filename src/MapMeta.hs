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

class MapMeta m where
  mapMeta :: (a -> b) -> m a -> m b

--

instance MapMeta IExpr where
  mapMeta f (ICExpr m c) = ICExpr (f m) c
  mapMeta f (IValue m n) = IValue (f m) n
  mapMeta f (IArg m n) = IArg (f m) n
  mapMeta f (ITupleApply m (bm, be) argName argVal) = ITupleApply (f m) (f bm, mapMeta f be) argName (mapMeta f argVal)

instance MapMeta Expr where
  mapMeta f (CExpr m c) = CExpr (f m) c
  mapMeta f (Value m n) = Value (f m) n
  mapMeta f (Arg m n) = Arg (f m) n
  mapMeta f (TupleApply m (bm, be) argName argVal) = TupleApply (f m) (f bm, mapMeta f be) argName (mapMeta f argVal)

mapMetaCompAnnot :: (MapMeta e) => (a -> b) -> CompAnnot (e a) -> CompAnnot (e b)
mapMetaCompAnnot f (CompAnnot name args) = CompAnnot name (fmap (mapMeta f) args)

mapMetaGuard :: (MapMeta e) => (a -> b) -> Guard (e a) -> Guard (e b)
mapMetaGuard f (IfGuard expr) = IfGuard (mapMeta f expr)
mapMetaGuard _ ElseGuard = ElseGuard
mapMetaGuard _ NoGuard = NoGuard

mapMetaObjArg :: (a -> b) -> ObjArg a -> ObjArg b
mapMetaObjArg f (m, maybeObj) = (f m, fmap (mapMeta f) maybeObj)

instance MapMeta Object where
  mapMeta f (Object m basis name vars args) = Object (f m) basis name (fmap f vars) (fmap (mapMetaObjArg f) args)

mapMetaArrow :: (MapMeta e) => (a -> b) -> Arrow (e a) a -> Arrow (e b) b
mapMetaArrow f (Arrow m annots guard maybeExpr) = Arrow (f m) (map (mapMetaCompAnnot f) annots) (mapMetaGuard f guard) (fmap (mapMeta f) maybeExpr)

mapMetaObjectMap :: (MapMeta e) => (a -> b) -> ObjectMap (e a) a -> ObjectMap (e b) b
mapMetaObjectMap f = map aux
  where aux (obj, arrows) = (mapMeta f obj, map (mapMetaArrow f) arrows)

mapMetaPrgm :: (MapMeta e) => (a -> b) -> Prgm (e a) a -> Prgm (e b) b
mapMetaPrgm f (objMap, classMap) = (mapMetaObjectMap f objMap, classMap)
