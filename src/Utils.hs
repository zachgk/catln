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
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Utils where

import Data.Graph

type GraphNodes node key = (node, key, [key])
type GraphData node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

mapFst3 :: (Functor f) => (a -> a') -> f (a, b, c) -> f (a', b, c)
mapFst3 f = fmap (\(a, b, c) -> (f a, b, c))

mapMFst3 :: (Traversable t, Monad m) => (a -> m a') -> t (a, b, c) -> m (t (a', b, c))
mapMFst3 f = mapM aux
  where aux (a, b, c) = do
          a' <- f a
          return (a', b, c)

graphToNodes :: GraphData node key -> [GraphNodes node key]
graphToNodes (g, nodeFromVertex, _) = map nodeFromVertex $ vertices g
