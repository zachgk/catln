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

-- Removes objects that match classes
-- When a class is used in the RHS of a multiTypeDef, it will create an object temporarily
-- This removes that temporary object and the CGType classGraph entry for it
removeClassInstanceObjects :: DesPrgm -> DesPrgm -> DesPrgm
removeClassInstanceObjects (_, fullPrgmClassGraph, _) (objMap, ClassGraph cg, annots) = (objMap', classGraph', annots)
  where
    classNames = listClassNames fullPrgmClassGraph
    notMatchesClassName n = null $ relativeNameFilter n classNames

    classGraph' = ClassGraph $ graphFromEdges $ filter classEntryMatches $ graphToNodes cg
    objMap' = filter (notMatchesClassName . oaObjPath) objMap

    classEntryMatches (CGType, PRelativeName n, _) = notMatchesClassName n
    classEntryMatches _                            = True


-- replaces uses of PRelativeName with PTypeName or PClassName when it can be determined
-- e.g. PRelativeName Boolean ==> PClassName /Data/Boolean
-- uses the mapMeta for objMap and annots, but must map the classGraph manually
-- the fullPrgmClassToTypes includes the imports and is used for when the def is inside an import
resolveRelativeNames :: DesPrgm -> DesPrgm -> DesPrgm
resolveRelativeNames (fullPrgmObjMap, fullPrgmClassGraph, _) (objMap, classGraph@(ClassGraph cg), annots) = mapMetaExprPrgm resolveMeta (objMap, classGraph', annots)
  where
    classGraph' = ClassGraph $ graphFromEdges $ map mapClassEntry $ graphToNodes cg
    mapClassEntry (node, tp, subTypes) = (mapCGNode node, resolveName True tp, map (resolveName True) subTypes)
    mapCGNode (CGClass (s, clss, ts, doc)) = CGClass (s, mapPartial True clss, fmap (mapType True) ts, doc)
    mapCGNode CGType = CGType
    classNames = nub $ listClassNames fullPrgmClassGraph
    objNames = nub $ map oaObjPath (concatMap getRecursiveExprObjs fullPrgmObjMap)

    resolveMeta _ (Meta t p md) = Meta (mapType False t) p md

    -- |
    -- requireResolveRelative -> type -> updated type
    -- It is required to resolve for the classGraph, but expressions can be left unresolved until type inference
    mapType :: Bool -> Type -> Type
    mapType _ (TopType ps) = TopType ps
    mapType _ tp@(TypeVar TVVar{}) = tp
    mapType _ (TypeVar TVArg{}) = error "Invalid arg type"
    mapType reqResolve (UnionType partials) = unionAllTypes classGraph $ map (singletonType . mapPartial reqResolve) $ splitUnionType partials

    mapPartial :: Bool -> PartialType -> PartialType
    mapPartial reqResolve partial@PartialType{ptName, ptVars, ptArgs, ptPreds} = partial {
      ptName = resolveName reqResolve ptName,
      ptVars = fmap (mapType reqResolve) ptVars,
      ptArgs = fmap (mapType reqResolve) ptArgs,
      ptPreds = fmap (mapPartial reqResolve) ptPreds
                                                                                                                  }

    -- |
    -- Attempts to convert 'PRelativeName' to either a type or a class
    -- If reqResolve is true, the conversions must succeed otherwise it can be left optional
    resolveName :: Bool -> PartialName -> PartialName
    resolveName reqResolve (PRelativeName name)= case (reqResolve, relativeNameFilter name classNames, relativeNameFilter name objNames) of
                  -- is a class, replace with class type
                  (_, [className], []) -> PClassName className

                  -- is data, use data after recursively cleaning classes
                  (_, [], [typeName]) -> PTypeName typeName

                  -- This case occurs when a class is used in a multiTypeDef
                  (_, [className], [typeName]) | className == typeName -> error $ printf "Found duplicate name %s" (show className)

                  -- (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s in type %s.\n\n\tType Options: %s\n\n\tClass Options: %s" name  (show partial) (show objNames) (show classNames)
                  (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s\n\tAvailable types: %s" name (show objNames)

                  (False, _, _) -> PRelativeName name
                  (True, foundTypeNames, foundClassNames) -> error $ printf "Could not resolve required name: %s \n\t Found possible typeNames: %s \n\t Found possible classNames: %s" name (show foundTypeNames) (show foundClassNames)
    resolveName _ name = name

expandDataReferences :: DesPrgm -> DesPrgm -> DesPrgm
expandDataReferences (fullPrgmObjMap, _, _) (objMap, classGraph@(ClassGraph cg), annots) = mapMetaExprPrgm aux (objMap, classGraph', annots)
  where
    classGraph' = ClassGraph $ graphFromEdges $ mapFst3 mapCGNode $ graphToNodes cg
    mapCGNode (CGClass (s, clss, ts, doc)) = CGClass (s, clss, fmap mapType ts, doc)
    mapCGNode CGType = CGType
    -- objExpansions = H.fromList $ concatMap (\(obj@ExprObject{eobjBasis}, _, _) -> ([(eobjPath obj, obj) | eobjBasis == TypeObj])) fullPrgmObjMap
    objExpansions = H.fromList $ concatMap (\oa@ObjArr{oaBasis} -> ([(oaObjPath oa, oa) | oaBasis == TypeObj])) fullPrgmObjMap
    aux metaType inM@(Meta t p md) = case metaType of
      ObjMeta               -> inM
      ExprMeta OutputMeta _ -> inM
      ExprMeta _ _          -> Meta (mapType t) p md
      ObjArgMeta            -> Meta (mapType t) p md
      ObjVarMeta            -> Meta (mapType t) p md
      ArrMeta               -> Meta (mapType t) p md

    mapType (TopType ps) = TopType ps
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (UnionType partials) = unionAllTypes classGraph $ map mapPartial $ splitUnionType partials

    mapPartial PartialType{ptName=PTypeName name} = case suffixLookup name (H.keys objExpansions) of
      Just fname -> case H.lookup fname objExpansions of
        Just obj -> getMetaType $ getExprMeta $ oaObjExpr obj
        Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show $ H.keys objExpansions)
      Nothing -> error $ printf "Data not found in expandDataReferences for %s with objExpansions %s" name (show $ H.keys objExpansions)
    mapPartial partial@PartialType{ptName=PClassName{}} = singletonType partial
    mapPartial partial@PartialType{ptName=PRelativeName{}} = singletonType partial
