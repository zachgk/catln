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

module Utils where

import           Control.Monad       (foldM)
import           Data.Graph
import           Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.Maybe          (fromJust)
import           Data.String.Builder (Builder, literal)

type GraphNodes node key = (node, key, [key])
type GraphData node key = (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thr3 :: (a, b, c) -> c
thr3 (_, _, c) = c

applyFst3 :: (a -> a') -> (a, b, c) -> (a', b, c)
applyFst3 f (a, b, c)= (f a, b, c)

mapFst3 :: (Functor f) => (a -> a') -> f (a, b, c) -> f (a', b, c)
mapFst3 fn = fmap (\(a, b, c) -> (fn a, b, c))

mapSnd3 :: (Functor f) => (b -> b') -> f (a, b, c) -> f (a, b', c)
mapSnd3 fn = fmap (\(a, b, c) -> (a, fn b, c))

mapMFst3 :: (Traversable t, Monad m) => (a -> m a') -> t (a, b, c) -> m (t (a', b, c))
mapMFst3 f = mapM aux
  where aux (a, b, c) = do
          a' <- f a
          return (a', b, c)

graphEmpty :: GraphData node key -> Bool
graphEmpty (g, _, _) = null $ vertices g

graphToNodes :: GraphData node key -> [GraphNodes node key]
graphToNodes (g, nodeFromVertex, _) = map nodeFromVertex $ vertices g

fmapGraph :: (Ord key) => (node1 -> node2) -> GraphData node1 key -> GraphData node2 key
fmapGraph f = graphFromEdges . mapFst3 f . graphToNodes

graphLookup :: (Ord key) => key -> GraphData node key -> Maybe node
graphLookup k (_, nodeFromVertex, vertexFromKey) = fst3 . nodeFromVertex <$> vertexFromKey k

-- TODO Change function to ([(n1, [n2])] -> [n2]) to recognize each input node has it's own direct imports
mapGraphWithDeps :: (Monad m, Hashable k, Ord k) => ([n1] -> [n2] -> m [n2]) -> GraphData n1 k -> m (GraphData n2 k)
mapGraphWithDeps f g@(graph, fromNode, fromName) = do
  let connComps = stronglyConnCompR $ graphToNodes g
  computed <- foldM addSCC H.empty connComps
  return $ graphFromEdges $ H.elems computed
  where
    -- addSCC :: (Monad m, Hashable k, Ord k) => H.HashMap k (n2, k, [k]) -> SCC (n1, k, [k]) -> m (H.HashMap k (n2, k, [k]))
    addSCC finished new = do
      let newList = flattenSCC new
      let newNames = S.fromList $ map snd3 newList
      let transitiveDepNames = S.fromList $ map (snd3 . fromNode) $ concatMap (reachable graph . fromJust . fromName) newNames
      -- let directDepNames = S.fromList $ concatMap thr3 newList
      -- TODO Improve import/export system (specifically core re-exporting it's files) to support using directDependencies
      let finishedDepNames = S.difference transitiveDepNames newNames
      let finished' = map (\dn -> H.lookupDefault (error "Failed to find dep in mapGraphWithDeps") dn finished) (S.toList finishedDepNames)
      new' <- f (map fst3 newList) (map fst3 finished')
      let newMap' = H.fromList $ zipWith (\(_, k, ds) n' -> (k, (n', k, ds))) newList new'
      return $ H.unionWith (error "overlap in mapGraphWithDeps") newMap' finished

unionsWith :: (Foldable f, Ord k, Hashable k) => (a->a->a) -> f (H.HashMap k a) -> H.HashMap k a
unionsWith f = foldl (H.unionWith f) H.empty

isSubsetOf :: (Eq a, Hashable a) => S.HashSet a -> S.HashSet a -> Bool
x `isSubsetOf` y = all (`S.member` y) x

isSubmapOf :: (Eq k, Eq v, Hashable k) => H.HashMap k v -> H.HashMap k v -> Bool
as `isSubmapOf` bs = and $ H.mapWithKey aux as
  where aux ak av = case H.lookup ak bs of
          Just bv -> av == bv
          Nothing -> True

-- normal type, type to powerset
powerset :: [x] -> [[x]]
powerset []     = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

uniq :: (Eq x, Hashable x) => [x] -> [x]
uniq = S.toList . S.fromList

withIndent :: Int -> String -> Builder
withIndent indent s = literal (replicate (4*indent) ' ' ++ s ++ "\n")
