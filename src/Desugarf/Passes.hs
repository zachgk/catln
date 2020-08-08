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
import           Data.Maybe

import           Syntax.Types
import           Syntax
import           Parser.Syntax
import           MapMeta

-- replaces uses of class with a SumType of it's objects
-- e.g. Boolean ==> Sum (True | False)
classToObjSum :: DesPrgm -> DesPrgm
classToObjSum prgm@(_, (_, classToTypes)) = mapMetaPrgm aux prgm
  where
    aux (PreTyped t) = PreTyped $ mapType H.empty t

    -- Replace all classes with their sum and replace type vars based on the var map
    mapType _ TopType = TopType
    mapType varMap tp@(TypeVar (TVVar t)) = fromMaybe tp $ H.lookup t varMap
    mapType _ (TypeVar TVArg{}) = error "Invalid arg type"
    mapType varMap (SumType partials) = unionTypes $ map mapPartial $ splitPartialLeafs partials
      where
        mapPartial (name, partialVars, partialArgs) = case H.lookup name classToTypes of
          -- is a class, replace with class type
          Just (_, classVars, classTypes) -> unionTypes $ map (mapType (H.union classVars varMap)) classTypes

          -- is data, use data after recursively cleaning classes
          Nothing -> SumType $ joinPartialLeafs [(name, fmap (mapType varMap) partialVars, fmap (mapType varMap) partialArgs)]
