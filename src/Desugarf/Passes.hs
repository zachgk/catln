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

module Desugarf.Passes where

import qualified Data.HashMap.Strict as H

import           Syntax.Types
import           Syntax
import           Parser.Syntax
import           MapMeta

-- replaces uses of PTypeName with PClassName where it actually contains a class
-- e.g. PTypeName Boolean ==> PClassName Boolean
typeNameToClass :: DesPrgm -> DesPrgm
typeNameToClass (objMap, classMap@(typeToClass, classToTypes)) = mapMetaPrgm aux (objMap, (typeToClass, classToTypes'))
  where
    classToTypes' = fmap (\(s, vs, ts) -> (s, fmap mapType vs, fmap mapType ts)) classToTypes
    aux (PreTyped t) = PreTyped $ mapType t

    mapType TopType = TopType
    mapType tp@(TypeVar TVVar{}) = tp
    mapType (TypeVar TVArg{}) = error "Invalid arg type"
    mapType (SumType partials) = unionTypes classMap $ map mapPartial $ splitPartialLeafs partials
      where
        mapPartial (PTypeName name, partialVars, partialArgs) = singletonType (name', fmap mapType partialVars, fmap mapType partialArgs)
          where name' = case H.lookup name classToTypes' of
                  -- is a class, replace with class type
                  Just _ -> PClassName name

                  -- is data, use data after recursively cleaning classes
                  Nothing -> PTypeName name
        mapPartial (PClassName name, partialVars, partialArgs) = singletonType (PClassName name, fmap mapType partialVars, fmap mapType partialArgs)
