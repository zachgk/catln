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

import           Data.Hashable
import qualified Data.HashMap.Strict as H

import           Syntax.Prgm

class MapMeta m where
  mapMeta :: (a -> b) -> m a -> m b

--

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

instance MapMeta Arrow where
  mapMeta f (Arrow m annots guard maybeExpr) = Arrow (f m) (map (mapMetaCompAnnot f) annots) (mapMetaGuard f guard) (fmap (mapMeta f) maybeExpr)

mapMetaObjectMap :: (Eq b, Hashable b) => (a -> b) -> ObjectMap a -> ObjectMap b
mapMetaObjectMap f mp = H.fromList $ map aux $ H.toList mp
  where aux (obj, arrows) = (mapMeta f obj, map (mapMeta f) arrows)

mapMetaPrgm :: (Eq b, Hashable b) => (a -> b) -> Prgm a -> Prgm b
mapMetaPrgm f (objMap, classMap) = (mapMetaObjectMap f objMap, classMap)
