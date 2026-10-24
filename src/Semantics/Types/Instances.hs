-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Instances
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The 'Semigroup' and 'Monoid' instances of 'ClassGraph' and 'TypeEnv'. Merging two class graphs
-- unions the type variables of a class declared in both, which needs the type algebra, so the
-- instances live above it as orphans rather than beside their types in "Semantics.Types.Data".
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Semantics.Types.Instances where

import           Data.Graph           (graphFromEdges, vertices)
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Semantics.Types.Data
import           Semantics.Types.Ops
import           Text.Printf


instance Semigroup ClassGraph where
  (ClassGraph classGraphA) <> (ClassGraph classGraphB) = ClassGraph $ mapToGraph $ H.unionWith mergeClasses (graphToMap classGraphA) (graphToMap classGraphB)
    where
      graphToMap (g, nodeFromVertex, _) = H.fromList $ map ((\classData@(_, className, _) -> (className, classData)) . nodeFromVertex) $ vertices g
      mapToGraph = graphFromEdges . H.elems
      -- Members are DEDUPLICATED by value ('nub'). The same base class re-merges once per
      -- importing page across an import closure, so concatenating would repeat every member once
      -- per page — and the class expansion walks the class DAG branching once PER member entry,
      -- so the repetition would make it exponential in the depth of the class hierarchy.
      mergeClasses (CGClass sealedA classA setA docA, className, subClassNamesA) (CGClass sealedB classB setB docB, _, subClassNamesB) = if sealedA == sealedB
            then (CGClass sealedA (mergeClassPartials classA classB) (L.nub (setA ++ setB)) (mergeDoc docA docB), className, L.nub (subClassNamesA ++ subClassNamesB))
            else error "Added to sealed class definition"
      mergeClasses node@(CGClass{}, _, _) (CGType{}, _, _) = node
      mergeClasses (CGType{}, _, _) node@(CGClass{}, _, _) = node
      mergeClasses (CGType, name, []) (CGType, _, []) = (CGType, name, [])
      mergeClasses cg1 cg2 = error $ printf "Unexpected input to mergeClassGraphs: \n\t%s \n\t%s" (show cg1) (show cg2)

      mergeClassPartials clss@PartialType{ptVars=varsA} PartialType{ptVars=varsB} = clss{ptVars = H.unionWith (unionTypes mergeTypeEnv) varsA varsB}
      -- Shared across all class merges of this (<>): building the sat cache is O(class graph).
      mergeTypeEnv = emptyTypeEnv'{teClassGraph=ClassGraph classGraphA, teSatCache=mkSatCache (ClassGraph classGraphA) S.empty}

instance Monoid ClassGraph where
  mempty = emptyClassGraph

instance (Semigroup tg) => Semigroup (TypeEnv tg) where
  -- A merged env is a DIFFERENT value: it never carries a query-cache token.
  -- It reads the items of a side that is one of a series ('ReuseScope') as that side holds them, so
  -- its reads of them are noted as that side's are. What it was merged with is no item of the
  -- series, and only the merge that names it ('Semantics.withLocalArrowsOf', 'teLocalKey') lets an
  -- answer read in it be handed to another env.
  (TypeEnv cg1 tg1 n1 b1 d1 dc1 pe1 ap1 is1 _ _ ln1 rf1 r1 _ _) <> (TypeEnv cg2 tg2 n2 b2 d2 dc2 pe2 ap2 is2 _ _ ln2 rf2 r2 _ _) = TypeEnv cg' (tg1 <> tg2) n' (H.unionWith (++) b1 b2) (d1 || d2) (dc1 || dc2) (pe1 || pe2) (ap1 || ap2) (is1 || is2) Nothing Nothing (S.union ln1 ln2) (min rf1 rf2) reuse Nothing (mkSatCache cg' n')
    where
      cg' = cg1 <> cg2
      n' = S.union n1 n2
      reuse = case (r1, r2) of
        (Nothing, r)              -> r
        (r, Nothing)              -> r
        (Just a, Just b) | a == b -> Just a
        _                         -> Nothing
