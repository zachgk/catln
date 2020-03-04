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
import           Syntax

type CallGraph = (Graph, Vertex -> ((), Name, [Name]), Name -> Maybe Vertex)

tupleNamesInExpr :: Expr m -> S.HashSet Name
tupleNamesInExpr CExpr{} = S.empty
tupleNamesInExpr (Tuple _ name args) = S.unions $ S.singleton name:H.elems (fmap tupleNamesInExpr args)
