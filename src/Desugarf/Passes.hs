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

import           Data.List           (nub)
import           MapMeta
import           Parser.Syntax
import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           Text.Printf

member :: String -> [String ] -> Bool
member x arr = case suffixLookup x arr of
  Just _  -> True
  Nothing -> False

-- replaces uses of PRelativeName with PTypeName or PClassName when it can be determined
-- e.g. PRelativeName Boolean ==> PClassName /Data/Boolean
-- uses the mapMeta for objMap and annots, but must map the classMap manually
-- the fullPrgmClassToTypes includes the imports and is used for when the def is inside an import
resolveRelativeNames :: DesPrgm -> DesPrgm -> DesPrgm
resolveRelativeNames (fullPrgmObjMap, (_, fullPrgmClassToTypes), _) (objMap, classMap@(typeToClass, classToTypes), annots) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'), annots)
  where
    classToTypes' = fmap (\(s, vs, ts, doc, path) -> (s, fmap (mapType True) vs, fmap (mapType True) ts, doc, path)) classToTypes
    objNames = nub $ map (objPath . fst) fullPrgmObjMap
    aux _ (PreTyped t p) = PreTyped (mapType False t) p

    -- |
    -- requireResolveRelative -> type -> updated type
    -- It is required to resolve for the classmap, but expressions can be left unresolved until type inference
    mapType :: Bool -> Type -> Type
    mapType _ TopType = TopType
    mapType _ tp@(TypeVar TVVar{}) = tp
    mapType _ (TypeVar TVArg{}) = error "Invalid arg type"
    mapType reqResolve (UnionType partials) = unionAllTypes classMap $ map mapPartial $ splitUnionType partials
      where
        mapPartial (PartialType (PRelativeName name) partialVars partialProps partialArgs partialArgMode) = singletonType (PartialType name' (fmap (mapType reqResolve) partialVars) (fmap (mapType reqResolve) partialProps) (fmap (mapType reqResolve) partialArgs) partialArgMode)
          where name' = case (reqResolve, relativeNameFilter name $ H.keys fullPrgmClassToTypes, relativeNameFilter name objNames) of
                  -- is a class, replace with class type
                  (_, [className], []) -> PClassName className

                  -- is data, use data after recursively cleaning classes
                  (_, [], [typeName]) -> PTypeName typeName

                  -- This case occurs when a class is used in a multiTypeDef
                  -- TODO Instead, the incorrect type definition should be removed before this pass is run
                  (_, [className], [typeName]) | className == typeName -> PClassName className

                  (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s in type %s" name  (show $ UnionType partials)

                  (False, _, _) -> PRelativeName name
                  (True, typeNames, classNames) -> error $ printf "Could not resolve required name: %s \n\t Found possible typeNames: %s \n\t Found possible classNames: %s" name (show typeNames) (show classNames)
        mapPartial partial@PartialType{ptVars, ptProps, ptArgs} = singletonType $ partial{
          ptVars = fmap (mapType reqResolve) ptVars,
          ptProps = fmap (mapType reqResolve) ptProps,
          ptArgs = fmap (mapType reqResolve) ptArgs
                                                                                                              }

expandDataReferences :: DesPrgm -> DesPrgm -> DesPrgm
expandDataReferences (fullPrgmObjMap, _, _) (objMap, classMap@(typeToClass, classToTypes), annots) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'), annots)
  where
    classToTypes' = fmap (\(s, vs, ts, doc, path) -> (s, fmap mapType vs, fmap mapType ts, doc, path)) classToTypes
    objExpansions = H.fromList $ concatMap (\(obj@Object{objBasis, objPath}, _) -> ([(objPath, obj) | objBasis == TypeObj])) fullPrgmObjMap
    aux metaType inM@(PreTyped t p) = case metaType of
      ExprMeta   -> inM
      ObjMeta    -> inM
      ObjArgMeta -> PreTyped (mapType t) p
      ObjVarMeta -> PreTyped (mapType t) p
      ArrMeta    -> PreTyped (mapType t) p

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (UnionType partials) = unionAllTypes classMap $ map mapPartial $ splitUnionType partials
      where
        mapPartial PartialType{ptName=PTypeName name} = case suffixLookup name (H.keys objExpansions) of
          Just fname -> case H.lookup fname objExpansions of
            Just Object{objM} -> getMetaType objM
            Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show $ H.keys objExpansions)
          Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show $ H.keys objExpansions)
        mapPartial partial@PartialType{ptName=PClassName{}} = singletonType partial
        mapPartial partial@PartialType{ptName=PRelativeName{}} = singletonType partial
