--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Partition
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module splits a program page into per-function-SCC fragments
-- so type inference can solve each fragment separately, promoting
-- solved fragments to typechecked dependencies of the later ones.
-- The groups come from the name-reference call graph over object-map
-- item keys: an item references another when any of its ObjArrs
-- mentions the other's name — in an expression ('Value' names of the
-- pattern, body, nested sub-objects, and annotations) or in a spelled
-- meta type (a result annotation @-> Cons@ or a declared argument
-- spelling reaches mid-solve reads through the type graph's
-- name-matching fallbacks, so those names are dependencies even when
-- no expression mentions them).
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck.Partition where

import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Graph           (flattenSCC, stronglyConnCompR)
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.List            (nub, partition, sortOn)
import           Data.Maybe

import           MapMeta              (mapMetaObjArrM)
import qualified MapMeta
import           Semantics.Prgm
import           Semantics.Types
import           Utils                (snd3)

-- | Names referenced anywhere inside a spelled type: positive and negative
-- leaf names, predicate class\/rel\/expr partial names, and (recursively) the
-- types in every partial's var\/arg slots. Argument names and class names that
-- are not object-map keys resolve to nothing downstream, so over-collection is
-- harmless; a missed name is a missed dependency edge.
typeRefNames :: Type -> [Name]
typeRefNames TypeVar{} = []
typeRefNames (UnionType mtop pos _) = concatMap partialNames (splitUnionType pos) ++ topNames
  where
    topNames = case mtop of
      Nothing -> []
      Just (preds, negLeafs, _) -> predsNames preds ++ concatMap partialNames (splitUnionType negLeafs)
    partialNames PartialType{ptName, ptVars, ptArgs, ptPreds} =
      ptName : concatMap typeRefNames (H.elems ptVars)
            ++ concatMap typeRefNames (H.elems ptArgs)
            ++ predsNames ptPreds
    predsNames (PredsOne p)  = predNames p
    predsNames (PredsAnd ps) = concatMap predsNames ps
    predsNames (PredsNot ps) = predsNames ps
    predNames (PredExpr p)  = partialNames p
    predNames (PredClass p) = partialNames p
    predNames (PredRel p)   = partialNames p

-- | 'Value' names referenced by an expression, descending into applied
-- arguments' sub-objects and bodies.
exprRefNames :: Expr m -> [Name]
exprRefNames CExpr{} = []
exprRefNames (Value _ n) = [n]
exprRefNames HoleExpr{} = []
exprRefNames (AliasExpr b a) = exprRefNames b ++ exprRefNames a
exprRefNames (EWhere _ b c) = exprRefNames b ++ exprRefNames c
exprRefNames (TupleApply _ (_, be) arg) = exprRefNames be ++ argNames arg
  where
    argNames (EAppArg aoa)  = objArrExprRefNames aoa
    argNames (EAppVar _ _)  = []
    argNames (EAppSpread e) = exprRefNames e

-- | Expression-side names of an ObjArr: pattern, annotations, and body.
objArrExprRefNames :: ObjArr Expr m -> [Name]
objArrExprRefNames oa = concatMap exprRefNames $
  maybeToList (oaObj oa) ++ oaAnnots oa ++ [e | Just (Just e, _) <- [oaArr oa]]

-- | All names an ObjArr references: expression names plus the names in every
-- spelled meta type (collected through 'mapMetaObjArrM' so no meta position is
-- missed).
objArrRefNames :: (MetaDat m, Show m) => ObjArr Expr m -> [Name]
objArrRefNames oa = objArrExprRefNames oa ++ metaNames
  where
    metaNames = execWriter $ mapMetaObjArrM collect Nothing oa
    collect :: MapMeta.MetaType -> Meta m -> Writer [Name] (Meta m)
    collect _ m = do
      tell $ typeRefNames $ getMetaType m
      return m

-- | Dependency-first solve groups of object-map item keys.
--
-- The first group is the DATA PRELUDE: every item whose ObjArrs are all
-- arrow-less (data\/type objects). Solving them first finalizes the data and
-- pattern entries of the object universe before any function group reads it.
-- The remaining groups are the SCCs of the name-reference graph between
-- function items, in dependency-first order ('stronglyConnCompR' returns
-- dependencies before dependents). Nodes and edges are sorted by name so the
-- grouping is deterministic.
itemSccGroups :: (MetaDat m, Show m) => ObjectMap Expr m -> [[Name]]
itemSccGroups (ObjectMap omap) = [dataKeys | not (null dataKeys)] ++ sccGroups
  where
    keys = sortOn nameStr $ H.keys omap
    isDataItem item = all (isNothing . oaArr) (flatObjectMapItem item)
    (dataKeys, funcKeys) = partition (\k -> isDataItem (H.lookupDefault (error "itemSccGroups: missing key") k omap)) keys
    funcKeySet = S.fromList funcKeys
    itemEdges k = sortOn nameStr $ filter (/= k) $ filter (`S.member` funcKeySet) $ nub $ concatMap resolveRef refs
      where
        refs = nub $ concatMap objArrRefNames $ flatObjectMapItem $ H.lookupDefault (error "itemSccGroups: missing key") k omap
        -- A relative reference resolves to EVERY matching item (the
        -- conservative choice for an ambiguous name).
        resolveRef r = relativeNameFilter r keys
    sccGroups = map (sortOn nameStr . map snd3 . flattenSCC) $ stronglyConnCompR [((), k, itemEdges k) | k <- funcKeys]

-- | Restrict each source program to the items of one group, dropping
-- statement annotations (they get their own fragments). The result keeps the
-- source-program list shape so encode\/decode results stay aligned with the
-- original programs; a program with no items in the group keeps an empty
-- object map.
fragmentPrgms :: [[Name]] -> [Prgm Expr m] -> [[Prgm Expr m]]
fragmentPrgms groups pprgms = map fragment groups
  where
    fragment group = map (restrict (S.fromList group)) pprgms
    restrict keySet p@Prgm{prgmObjMap=ObjectMap omap} =
      p{prgmObjMap = ObjectMap $ H.filterWithKey (\k _ -> S.member k keySet) omap, prgmAnnots = []}

-- | One fragment per source program that has statement annotations: the
-- annotations with an empty object map (they are consumers only — nothing
-- reads their points). Solved last, after every function group.
annotFragmentPrgms :: [Prgm Expr m] -> [[Prgm Expr m]]
annotFragmentPrgms pprgms =
  [ [ if j == i then p{prgmObjMap = ObjectMap H.empty} else emptied p
    | (j, p) <- zip [(0 :: Int)..] pprgms ]
  | (i, pi') <- zip [(0 :: Int)..] pprgms, not (null (prgmAnnots pi')) ]
  where
    emptied p = p{prgmObjMap = ObjectMap H.empty, prgmAnnots = []}
