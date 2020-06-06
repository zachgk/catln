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

classToObjSum :: DesPrgm -> DesPrgm
classToObjSum prgm@(_, (_, classToTypes)) = mapMetaPrgm aux prgm
  where
    aux (PreTyped t) = PreTyped $ mapType t
    mapType TopType = TopType
    mapType (SumType partials) = SumType partials'
      where
        partials' = H.fromList $ concatMap mapPartial $ H.toList partials
        mapOpt (vars, args) = (fmap mapType vars, fmap mapType args)
        mapOpts = S.map mapOpt
        mapPartial (name, opts) = case H.lookup name classToTypes of
          Just (_, types) -> map (, mapOpts opts) $ S.toList types
          Nothing -> [(name, mapOpts opts)]
