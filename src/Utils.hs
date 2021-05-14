--------------------------------------------------------------------
-- |
-- Module    :  Utils
-- Copyright :  (c) Zach Kimberg 2021
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains minor utility functions.
--------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Utils where

import Data.Graph

type GraphNodes node key = (node, key, [key])
type GraphData node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thr3 :: (a, b, c) -> c
thr3 (_, _, c) = c

mapFst3 :: (Functor f) => (a -> a') -> f (a, b, c) -> f (a', b, c)
mapFst3 fn = fmap (\(a, b, c) -> (fn a, b, c))

mapSnd3 :: (Functor f) => (b -> b') -> f (a, b, c) -> f (a, b', c)
mapSnd3 fn = fmap (\(a, b, c) -> (a, fn b, c))

mapMFst3 :: (Traversable t, Monad m) => (a -> m a') -> t (a, b, c) -> m (t (a', b, c))
mapMFst3 f = mapM aux
  where aux (a, b, c) = do
          a' <- f a
          return (a', b, c)

graphToNodes :: GraphData node key -> [GraphNodes node key]
graphToNodes (g, nodeFromVertex, _) = map nodeFromVertex $ vertices g

fmapGraph :: (Ord key) => (node1 -> node2) -> GraphData node1 key -> GraphData node2 key
fmapGraph f = graphFromEdges . mapFst3 f . graphToNodes
