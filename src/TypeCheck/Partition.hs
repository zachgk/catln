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
import qualified Data.List            as L
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
    predNames PredConstEq{} = []

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

-- | The definition a sub function is nested in: the item whose name its own extends by a dot
-- ('Syntax.Ct.Desugarf.Nested.nestedSubName' — @/Data/List/sorted.insert@ is nested in
-- @/Data/List/sorted@). The innermost when several do, as a sub function of a sub function
-- extends both.
nestedParentOf :: H.HashMap Name Name -> Name -> Maybe Name
nestedParentOf names k = listToMaybe [ item | i <- reverse (L.elemIndices '.' (nameStr k)), let p = mkName (take i (nameStr k)), Just item <- [H.lookup p names] ]

-- | Each name the items of a map define, to the item it is kept under ('nestedParentOf'): an item's
-- own key, and the function each definition in a context defines ('oaDefinedName'). Every
-- definition in a context is kept under the context's key (@/main{io -> IO}@ in @/ContextIn@), so
-- by keys alone the sub functions of one (@/main.$47d595@, a bind's continuation) had no parent,
-- and were never typed by their callers.
definedNameItems :: (MetaDat m, Show m) => ObjectMap Expr m -> H.HashMap Name Name
definedNameItems (ObjectMap omap) = H.fromList $
  [ (oaDefinedName oa, k) | (k, item) <- H.toList omap, oa <- flatObjectMapItem item, oaDefinedName oa /= k ]
  ++ [ (k, k) | k <- H.keys omap ]

-- | Dependency-first solve groups of object-map item keys.
--
-- The first group is the DATA PRELUDE: every item whose ObjArrs are all
-- arrow-less (data\/type objects). Solving them first finalizes the data and
-- pattern entries of the object universe before any function group reads it.
-- The remaining groups are the SCCs of the name-reference graph between
-- function items, in dependency-first order ('stronglyConnCompR' returns
-- dependencies before dependents). Nodes and edges are sorted by name so the
-- grouping is deterministic.
--
-- A sub function is solved with the definition it is nested in, not before it as a dependency: its
-- callers are exactly the ones in that definition ('nestedParentOf'), so what they pass and what
-- they need of it are part of its typing ('TypeCheck.Encode.nestedCallerConstraints'). The parent
-- already refers to it, so an edge back to the parent puts both in one group.
itemSccGroups :: (MetaDat m, Show m) => ObjectMap Expr m -> [[Name]]
itemSccGroups (ObjectMap omap) = [dataKeys | not (null dataKeys)] ++ sccGroups
  where
    keys = sortOn nameStr $ H.keys omap
    isDataItem item = all (isNothing . oaArr) (flatObjectMapItem item)
    (dataKeys, funcKeys) = partition (\k -> isDataItem (H.lookupDefault (error "itemSccGroups: missing key") k omap)) keys
    funcKeySet = S.fromList funcKeys
    funcNames = definedNameItems (ObjectMap (H.filterWithKey (\k _ -> S.member k funcKeySet) omap))
    itemEdges k = sortOn nameStr $ filter (/= k) $ filter (`S.member` funcKeySet) $ nub $ maybeToList (nestedParentOf funcNames k) ++ concatMap resolveRef refs
      where
        refs = nub $ concatMap objArrRefNames $ flatObjectMapItem $ H.lookupDefault (error "itemSccGroups: missing key") k omap
        -- A relative reference resolves to EVERY matching item (the
        -- conservative choice for an ambiguous name).
        resolveRef r = relativeNameFilter r keys
    sccGroups = map (sortOn nameStr . map snd3 . flattenSCC) $ stronglyConnCompR [((), k, itemEdges k) | k <- funcKeys]

-- | The keys of all arrow-less (data\/type) items. Class expansions materialize member
-- objects from the static class graph regardless of any name reference, so EVERY solve
-- fragment needs the data items in its dependency closure — they are cheap (no bodies) and
-- their absence sends a class-typed solve through the un-memoized expansion fallbacks.
dataItemKeys :: ObjectMap Expr m -> [Name]
dataItemKeys (ObjectMap omap) = H.keys $ H.filter (all (isNothing . oaArr) . flatObjectMapItem) omap

-- | Per-item UNRESOLVED references: every name each item's ObjArrs mention. Polymorphic in
-- the meta so it serves both the page side (pre-typed) and the dependency side (typechecked
-- — whose resolved metas can carry universe-sized types, so dedupe with a set, never 'nub').
itemRawRefs :: (MetaDat m, Show m) => ObjectMap Expr m -> H.HashMap Name [Name]
itemRawRefs (ObjectMap omap) = fmap (hashNub . concatMap objArrRefNames . flatObjectMapItem) omap

-- | 'itemRawRefs' for TYPECHECKED items: expression names plus the names in the arrow
-- RESULT types only. A typechecked item's every meta holds a fully resolved type — walking
-- them all re-walks near-universe-sized unions hundreds of times per item, and the graph
-- only follows an item outward through its calls (expression names) and its results (the
-- reaches recursion enters the next arrows through the dest type's names).
itemRawRefsShallow :: (MetaDat m, Show m) => ObjectMap Expr m -> H.HashMap Name [Name]
itemRawRefsShallow (ObjectMap omap) = fmap (hashNub . concatMap oaRefs . flatObjectMapItem) omap
  where
    oaRefs oa = objArrExprRefNames oa ++ concatMap (typeRefNames . getMetaType) [m | Just (_, m) <- [oaArr oa]]

hashNub :: [Name] -> [Name]
hashNub = S.toList . S.fromList

-- | Resolve raw per-item references into edges against a key set: a relative reference
-- resolves to EVERY matching key (the conservative choice for an ambiguous name), through a
-- suffix index so each reference resolves in one lookup instead of a scan over all keys
-- (a typechecked item can carry thousands of distinct names).
resolveRefEdges :: [Name] -> H.HashMap Name [Name] -> H.HashMap Name [Name]
resolveRefEdges keys = H.mapWithKey resolveItem
  where
    -- 'relativeNameMatches rn k' holds iff ('/':rn) is a suffix of ('/':k) at a path
    -- boundary (or rn == k): index every key under each of its path-boundary suffixes.
    suffixIndex :: H.HashMap String [Name]
    suffixIndex = H.fromListWith (++) $ concatMap keyEntries keys
    keyEntries k = (nameStr k, [k]) : [(suf, [k]) | suf <- boundarySuffixes ('/' : nameStr k)]
    boundarySuffixes s = [rest | ('/':rest) <- tails' s, not (null rest)]
    tails' []         = []
    tails' s@(_:rest) = s : tails' rest
    resolveItem k refs = filter (/= k) $ hashNub $ concatMap resolveRef refs
    resolveRef r
      | nameStr r `elem` ["", "/"] = []
      | otherwise = H.lookupDefault [] (nameStr r) suffixIndex

-- | Transitive closure of the edge map from the seed keys, EXCLUDING the seeds themselves.
closureFrom :: H.HashMap Name [Name] -> [Name] -> S.HashSet Name
closureFrom edges seeds = S.difference (go S.empty seeds) (S.fromList seeds)
  where
    go visited [] = visited
    go visited (k:rest)
      | S.member k visited = go visited rest
      | otherwise = go (S.insert k visited) (H.lookupDefault [] k edges ++ rest)

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
