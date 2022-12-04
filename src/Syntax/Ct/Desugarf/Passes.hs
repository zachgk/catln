--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Ct.Desugarf.Passes
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

module Syntax.Ct.Desugarf.Passes where

import qualified Data.HashMap.Strict     as H

import           Data.Graph
import           Data.List               (nub)
import           MapMeta
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Text.Printf
import           Utils

-- replaces uses of PRelativeName with PTypeName or PClassName when it can be determined
-- e.g. PRelativeName Boolean ==> PClassName /Data/Boolean
-- uses the mapMeta for objMap and annots, but must map the classGraph manually
-- the fullPrgmClassToTypes includes the imports and is used for when the def is inside an import
resolveRelativeNames :: FinalDesPrgm -> FinalDesPrgm -> FinalDesPrgm
resolveRelativeNames (fullPrgmObjMap, fullPrgmClassGraph, _) (objMap, classGraph@(ClassGraph cg), annots) = mapMetaPrgm aux (objMap, classGraph', annots)
  where
    classGraph' = ClassGraph $ graphFromEdges $ mapFst3 mapCGNode $ graphToNodes cg
    mapCGNode (CGClass (s, vs, ts, doc, p)) = CGClass (s, fmap (mapType True) vs, fmap (mapType True) ts, doc, p)
    mapCGNode CGType = CGType
    classNames = listClassNames fullPrgmClassGraph
    objNames = nub $ map (objPath . fst3) fullPrgmObjMap
    aux _ (Meta t p md) = Meta (mapType False t) p md

    -- |
    -- requireResolveRelative -> type -> updated type
    -- It is required to resolve for the classGraph, but expressions can be left unresolved until type inference
    mapType :: Bool -> Type -> Type
    mapType _ TopType = TopType
    mapType _ tp@(TypeVar TVVar{}) = tp
    mapType _ (TypeVar TVArg{}) = error "Invalid arg type"
    mapType reqResolve (UnionType partials) = unionAllTypes classGraph $ map (singletonType . mapPartial) $ splitUnionType partials
      where
        mapPartial :: PartialType -> PartialType
        mapPartial (PartialType (PRelativeName name) partialVars partialArgs partialPreds partialArgMode) = PartialType name' (fmap (mapType reqResolve) partialVars) (fmap (mapType reqResolve) partialArgs) (map mapPartial partialPreds) partialArgMode
          where
            typeOptions = relativeNameFilter name objNames
            classOptions = relativeNameFilter name classNames
            name' = case (reqResolve, classOptions, typeOptions) of
                  -- is a class, replace with class type
                  (_, [className], []) -> PClassName className

                  -- is data, use data after recursively cleaning classes
                  (_, [], [typeName]) -> PTypeName typeName

                  -- This case occurs when a class is used in a multiTypeDef
                  -- TODO Instead, the incorrect type definition should be removed before this pass is run
                  (_, [className], [typeName]) | className == typeName -> PClassName className

                  (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s in type %s.\n\n\tType Options: %s\n\n\tClass Options: %s" name  (show $ UnionType partials) (show objNames) (show classNames)
                  -- (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s in type %s" name  (show $ UnionType partials)

                  (False, _, _) -> PRelativeName name
                  (True, foundTypeNames, foundClassNames) -> error $ printf "Could not resolve required name: %s \n\t Found possible typeNames: %s \n\t Found possible classNames: %s" name (show foundTypeNames) (show foundClassNames)
        mapPartial partial@PartialType{ptVars, ptArgs, ptPreds} = partial{
          ptVars = fmap (mapType reqResolve) ptVars,
          ptArgs = fmap (mapType reqResolve) ptArgs,
          ptPreds = fmap mapPartial ptPreds
                                                                                                                      }

expandDataReferences :: FinalDesPrgm -> FinalDesPrgm -> FinalDesPrgm
expandDataReferences (fullPrgmObjMap, _, _) (objMap, classGraph@(ClassGraph cg), annots) = mapMetaPrgm aux (objMap, classGraph', annots)
  where
    classGraph' = ClassGraph $ graphFromEdges $ mapFst3 mapCGNode $ graphToNodes cg
    mapCGNode (CGClass (s, vs, ts, doc, p)) = CGClass (s, fmap mapType vs, fmap mapType ts, doc, p)
    mapCGNode CGType = CGType
    objExpansions = H.fromList $ concatMap (\(obj@Object{objBasis}, _, _) -> ([(objPath obj, obj) | objBasis == TypeObj])) fullPrgmObjMap
    aux metaType inM@(Meta t p md) = case metaType of
      ExprMeta _ -> inM
      ObjMeta    -> inM
      ObjArgMeta -> Meta (mapType t) p md
      ObjVarMeta -> Meta (mapType t) p md
      ArrMeta    -> Meta (mapType t) p md

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (UnionType partials) = unionAllTypes classGraph $ map mapPartial $ splitUnionType partials
      where
        mapPartial PartialType{ptName=PTypeName name} = case suffixLookup name (H.keys objExpansions) of
          Just fname -> case H.lookup fname objExpansions of
            Just obj -> getMetaType $ objM obj
            Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show $ H.keys objExpansions)
          Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show $ H.keys objExpansions)
        mapPartial partial@PartialType{ptName=PClassName{}} = singletonType partial
        mapPartial partial@PartialType{ptName=PRelativeName{}} = singletonType partial