--------------------------------------------------------------------
-- |
-- Module    :  Desugarf.Passes
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
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
typeNameToClass :: DesPrgm -> DesPrgm
typeNameToClass (objMap, classMap@(typeToClass, classToTypes), annots) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'), annots)
  where
    classToTypes' = fmap (\(s, vs, ts) -> (s, fmap mapType vs, fmap mapType ts)) classToTypes
    aux _ (PreTyped t p) = PreTyped (mapType t) p

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (SumType partials) = unionTypes classMap $ map mapPartial $ splitPartialLeafs partials
      where
        mapPartial (PartialType (PTypeName name) partialVars partialProps partialArgs partialArgMode) = singletonType (PartialType name' (fmap mapType partialVars) (fmap mapType partialProps) (fmap mapType partialArgs) partialArgMode)
          where name' = case H.lookup name classToTypes' of
                  -- is a class, replace with class type
                  Just _ -> PClassName name

                  -- is data, use data after recursively cleaning classes
                  Nothing -> PTypeName name
        mapPartial partial@PartialType{ptName=PClassName{}, ptVars, ptProps, ptArgs} = singletonType $ partial{
          ptVars = fmap mapType ptVars,
          ptProps = fmap mapType ptProps,
          ptArgs = fmap mapType ptArgs
                                                                                                              }

expandDataReferences :: DesPrgm -> DesPrgm
expandDataReferences (objMap, classMap@(typeToClass, classToTypes), annots) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'), annots)
  where
    classToTypes' = fmap (\(s, vs, ts) -> (s, fmap mapType vs, fmap mapType ts)) classToTypes
    objExpansions = H.fromList $ concatMap (\(obj@(Object _ basis name _ _), _) -> ([(name, obj) | basis == TypeObj])) objMap
    aux metaType inM@(PreTyped t p) = case metaType of
      ExprMeta -> inM
      ObjMeta -> inM
      ObjArgMeta -> PreTyped (mapType t) p
      ObjVarMeta -> PreTyped (mapType t) p
      ArrMeta -> PreTyped (mapType t) p

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (SumType partials) = unionTypes classMap $ map mapPartial $ splitPartialLeafs partials
      where
        mapPartial PartialType{ptName=PTypeName name} = case H.lookup name objExpansions of
          Just (Object objM _ _ _ _) -> getMetaType objM
          Nothing -> error $ printf "Data not found in expandDataReferences for %s" name
        mapPartial partial@PartialType{ptName=PClassName{}} = singletonType partial
