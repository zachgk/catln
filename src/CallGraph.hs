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
import           Syntax.Prgm
import           Syntax.Types
import Parser.Syntax

type CallGraph = (Graph, Vertex -> ((), Name, [Name]), Name -> Maybe Vertex)

tupleNamesInExpr :: PSExpr -> S.HashSet Name
tupleNamesInExpr PSCExpr{} = S.empty
tupleNamesInExpr (PSValue _ name) = S.singleton name
tupleNamesInExpr (PSTupleApply _ _ args) = S.unions $ H.elems (fmap tupleNamesInExpr args)

buildCallGraph :: [PSemiDecl] -> CallGraph
buildCallGraph decls = graphFromEdges $ map fromDecl decls
  where
    fromDecl (PSemiDecl (DeclLHS _ (Pattern (Object _ _ name _) _)) _ Nothing) = ((), name, [])
    fromDecl (PSemiDecl (DeclLHS _ (Pattern (Object _ _ name _) _)) _ (Just expr)) = ((), name, S.toList $ tupleNamesInExpr expr)
