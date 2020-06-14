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
import qualified Data.HashSet          as S

import           Syntax.Types
import           Syntax
import           Parser.Syntax
import           MapMeta
import           Text.Printf

-- replaces uses of class with a SumType of it's objects
-- e.g. Boolean ==> Sum (True | False)
classToObjSum :: DesPrgm -> DesPrgm
classToObjSum prgm@(_, (_, classToTypes)) = mapMetaPrgm aux prgm
  where
    aux (PreTyped t) = PreTyped $ mapType t
    mapType TopType = TopType
    mapType t@TypeVar{} = t
    mapType (SumType partials) = SumType partials'
      where
        partials' = H.fromList $ concatMap mapPartial $ H.toList partials
        mapPartial (name, opts) = case H.lookup name classToTypes of
          Just (_, SumType classPartials) -> H.toList classPartials
          Just cls -> error $ printf "bad classToObjSum partial: %s(%s) \n\tClass: %s" name (show opts) (show cls)
          Nothing -> [(name, mapOpts opts)]
        mapOpts = S.map mapOpt
        mapOpt (vars, args) = (fmap mapType vars, fmap mapType args)
