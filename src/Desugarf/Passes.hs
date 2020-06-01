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
    aux t@(PreTyped RawTopType) = t
    aux (PreTyped (RawSumType partials)) = PreTyped $ RawSumType partials'
      where
        partials' = H.fromList $ concatMap mapPartial $ H.toList partials
        -- TODO mapPartial where args can be classes
        mapPartial partial@(name, opts) = case H.lookup name classToTypes of
          Just (_, types) -> map (,opts) $ S.toList types
          Nothing -> [partial]
