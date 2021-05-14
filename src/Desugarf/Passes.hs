--------------------------------------------------------------------
-- |
-- Module    :  Desugarf.Passes
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains various passes run over the program after
-- it has finished desugaring. They apply various corrections to
-- the program left over from the parsing.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Desugarf.Passes where

import qualified Data.HashMap.Strict as H

import           Syntax.Types
import           Syntax
import           Parser.Syntax
import           MapMeta
import Text.Printf
import Syntax.Prgm

-- replaces uses of PTypeName with PClassName where it actually contains a class
-- e.g. PTypeName Boolean ==> PClassName Boolean
-- uses the mapMeta for objMap and annots, but must map the classMap manually
-- the fullPrgmClassToTypes includes the imports and is used for when the class def is inside an import
typeNameToClass :: DesPrgm -> DesPrgm -> DesPrgm
typeNameToClass (_, (_, fullPrgmClassToTypes), _) (objMap, classMap@(typeToClass, classToTypes), annots) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'), annots)
  where
    classToTypes' = fmap (\(s, vs, ts) -> (s, fmap mapType vs, fmap mapType ts)) classToTypes
    aux _ (PreTyped t p) = PreTyped (mapType t) p

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (UnionType partials) = unionTypes classMap $ map mapPartial $ splitPartialLeafs partials
      where
        mapPartial (PartialType (PTypeName name) partialVars partialProps partialArgs partialArgMode) = singletonType (PartialType name' (fmap mapType partialVars) (fmap mapType partialProps) (fmap mapType partialArgs) partialArgMode)
          where name' = if H.member name fullPrgmClassToTypes
                  -- is a class, replace with class type
                  then PClassName name

                  -- is data, use data after recursively cleaning classes
                  else PTypeName name
        mapPartial partial@PartialType{ptName=PClassName{}, ptVars, ptProps, ptArgs} = singletonType $ partial{
          ptVars = fmap mapType ptVars,
          ptProps = fmap mapType ptProps,
          ptArgs = fmap mapType ptArgs
                                                                                                              }

expandDataReferences :: DesPrgm -> DesPrgm -> DesPrgm
expandDataReferences (fullPrgmObjMap, _, _) (objMap, classMap@(typeToClass, classToTypes), annots) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'), annots)
  where
    classToTypes' = fmap (\(s, vs, ts) -> (s, fmap mapType vs, fmap mapType ts)) classToTypes
    objExpansions = H.fromList $ concatMap (\(obj@Object{objBasis, objName}, _) -> ([(objName, obj) | objBasis == TypeObj])) fullPrgmObjMap
    aux metaType inM@(PreTyped t p) = case metaType of
      ExprMeta -> inM
      ObjMeta -> inM
      ObjArgMeta -> PreTyped (mapType t) p
      ObjVarMeta -> PreTyped (mapType t) p
      ArrMeta -> PreTyped (mapType t) p

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (UnionType partials) = unionTypes classMap $ map mapPartial $ splitPartialLeafs partials
      where
        mapPartial PartialType{ptName=PTypeName name} = case H.lookup name objExpansions of
          Just Object{objM} -> getMetaType objM
          Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show objMap)
        mapPartial partial@PartialType{ptName=PClassName{}} = singletonType partial
