--------------------------------------------------------------------
-- |
-- Module    :  Utils
-- Copyright :  (c) Zach Kimberg 2021
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Utils where

import Data.Graph

type GraphNodes node key = (node, key, [key])
type GraphData node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

graphToNodes :: GraphData node key -> [GraphNodes node key]
graphToNodes (g, nodeFromVertex, _) = map nodeFromVertex $ vertices g
