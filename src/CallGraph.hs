--------------------------------------------------------------------
-- |
-- Module    :  CallGraph
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module CallGraph where

import Data.Graph

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Syntax.Types
import           Syntax

type CallGraph = (Graph, Vertex -> ((), Name, [Name]), Name -> Maybe Vertex)

tupleNamesInExpr :: Expr m -> S.HashSet Name
tupleNamesInExpr CExpr{} = S.empty
tupleNamesInExpr (Value _ name) = S.singleton name
tupleNamesInExpr (TupleApply _ _ args) = S.unions $ H.elems (fmap tupleNamesInExpr args)
