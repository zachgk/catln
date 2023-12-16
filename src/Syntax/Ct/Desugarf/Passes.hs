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

import           Data.Graph
import qualified Data.HashMap.Strict     as H
import           Data.List               (nub)
import           Data.Maybe
import           MapMeta
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Parser.Syntax
import           Text.Printf
import           Utils                   (graphToNodes)

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
resolveRelativeNames fullPrgm@(fullPrgmObjMap, fullPrgmClassGraph, _) (objMap, ClassGraph cg, annots) = mapMetaPrgm resolveMeta (objMap, classGraph', annots)
  where
    classGraph' = ClassGraph $ graphFromEdges $ map mapClassEntry $ graphToNodes cg
    mapClassEntry (node, tp, subTypes) = (mapCGNode node, resolveName True tp, map (resolveName True) subTypes)
    mapCGNode (CGClass (s, clss, ts, doc)) = CGClass (s, fromJust $ maybeGetClassSingleton $ mapPartial True clss, fmap (mapType True) ts, doc)
    mapCGNode CGType = CGType
    classNames = nub $ listClassNames fullPrgmClassGraph
    objNames = nub $ map oaObjPath (concatMap getRecursiveObjs fullPrgmObjMap)

    resolveMeta _ (Meta t p md) = Meta (mapType False t) p md

    -- |
    -- requireResolveRelative -> type -> updated type
    -- It is required to resolve for the classGraph, but expressions can be left unresolved until type inference
    mapType :: Bool -> Type -> Type
    mapType reqResolve t@(TopType [PredRel n m]) = case resolveName reqResolve (PRelativeName n) of
      PTypeName n'       -> setArgMode H.empty m $ singletonType $ partialVal $ PTypeName n'
      PClassName n'      -> setArgMode H.empty m $ classPartial $ partialVal $ PTypeName n'
      PRelativeName{} -> t
    mapType _ (TopType ps) = TopType ps
    mapType _ tp@(TypeVar TVVar{} _) = tp
    mapType _ (TypeVar TVArg{} _) = error "Invalid arg type"
    mapType reqResolve (UnionType partials) = unionAllTypes (mkTypeEnv fullPrgm) $ map (mapPartial reqResolve) $ splitUnionType partials

    mapPartial :: Bool -> PartialType -> Type
    mapPartial reqResolve partial@PartialType{ptName, ptVars, ptArgs, ptPreds} = case name' of
      PTypeName{}       -> singletonType partial'
      PClassName{}      -> classPartial partial'
      (PRelativeName n) -> spreadType H.empty $ relTypeVal n
      where
        name' = resolveName reqResolve ptName
        partial' = partial {
          ptName = PTypeName $ fromPartialName name',
          ptVars = fmap (mapType reqResolve) ptVars,
          ptArgs = fmap (mapType reqResolve) ptArgs,
          ptPreds = fmap (mapTypePred (fromJust . maybeGetSingleton . mapPartial reqResolve)) ptPreds
                                                                                                                      }

    -- |
    -- Attempts to convert 'PRelativeName' to either a type or a class
    -- If reqResolve is true, the conversions must succeed otherwise it can be left optional
    resolveName :: Bool -> PartialName -> PartialName
    resolveName reqResolve (PRelativeName name)= case (reqResolve, relativeNameFilter name classNames, relativeNameFilter name objNames) of
                  -- is a class, replace with class type
                  (_, [className], []) -> PClassName $ makeAbsoluteName className

                  -- is data, use data after recursively cleaning classes
                  (_, [], [typeName]) -> PTypeName $ makeAbsoluteName typeName

                  -- This case occurs when a class is used in a multiTypeDef
                  (_, [className], [typeName]) | className == typeName -> error $ printf "Found duplicate name %s" (show className)

                  -- Used to check (line below) to ensure types are always known, but may not be true for Arg Values
                  -- (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s in type %s.\n\n\tType Options: %s\n\n\tClass Options: %s" name  (show partial) (show objNames) (show classNames)
                  -- (_, [], []) -> error $ printf "There is no possible types or classes that correspond to name %s\n\tAvailable types: %s" name (show objNames)

                  (False, _, _) -> PRelativeName name
                  (True, foundTypeNames, foundClassNames) -> error $ printf "Could not resolve required name: %s \n\t Found possible typeNames: %s \n\t Found possible classNames: %s" name (show foundTypeNames) (show foundClassNames)
    resolveName _ name = name
