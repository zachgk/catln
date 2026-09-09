--------------------------------------------------------------------
-- |
-- Module    :  Semantics.TypeGraph
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module handles the type graph during type checking.
-- It uses base type(s) and arrows that can convert them
-- and checks if a conversion path is possible
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Semantics.TypeGraph where
import           Control.Monad
import           Data.Aeson          (ToJSON)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.String.Builder (build)
import           GHC.Generics        (Generic)
import           Semantics.Types
import           Text.Printf
import           Utils               (withIndent)

data ReachesTree
  = ReachesPartialTree !(H.HashMap PartialType ReachesTree) -- ReachesPartialTree are required (all must be handled to handle a partial)
  | ReachesTypeTree !(H.HashMap Type (String, ReachesTree)) -- ReachesTypeTree are optional (can choose to follow only one)
  | ReachesLeaf
  deriving (Eq, Ord, Generic, Hashable, ToJSON)

data ReachesEnv tg = ReachesEnv {
  rTypeEnv :: !(TypeEnv tg),
  rVaenv   :: !TypeVarArgEnv,
  rVisited :: !(S.HashSet PartialType)
                             }

instance Show ReachesTree where
  show = build . aux 0
    where
      aux indent (ReachesPartialTree children) = do
        forM_ (H.toList children) $ \(key, subTree) -> do
          withIndent indent $ show key
          aux (indent + 1) subTree
      aux indent (ReachesTypeTree children) = do
        forM_ (H.toList children) $ \(key, (reason, subTree)) -> do
          withIndent indent (printf "%s by %s" (show key) reason)
          aux (indent + 1) subTree
      aux _ ReachesLeaf = return ()

unionReachesTree :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> Type
-- A raw applied form whose settled data this whole union does not already contain (a recursive
-- result like @sorted(raw)@ next to only its @Nil@ base case) is presented BY its declared result
-- too ('presentRawResult': @sorted(raw) ∪ ∀ConsList@), so a requirement built from the union admits
-- the @Cons@ the term reaches. A raw form whose settled result IS already present anywhere in the
-- union (a wrapper @pureConsInt(raw)@ beside its @Cons@ result, or @restToString(raw)@ beside its
-- @String@) is left exact, preserving raw-vs-settled dispatch keying. The coverage is the WHOLE
-- union, so a settled sibling provided by another reach child is seen.
unionReachesTree typeEnv vaenv (ReachesPartialTree children) =
  presentRawResult typeEnv vaenv u
  where
    -- Flattened single-compaction union: each child contributes its source key and its
    -- subtree's union as separate operands ('unionAllTypesWithEnvOnce' merges all positive
    -- operands raw and compacts once), instead of one compaction per child plus one per
    -- fold step. The TypeVar cases keep replacing the key with the pointer, as before.
    u = unionAllTypesWithEnvOnce typeEnv vaenv $ concatMap unionChild $ H.toList children
    unionChild (key, val) = case unionReachesTree typeEnv vaenv val of
      (TypeVar (TVArg argName) tl i) | makeAbsoluteName (ptName key) == makeAbsoluteName (pkName argName) -> [TypeVar (TVArg $ makeAbsolutePk argName) tl i]
      (TypeVar (TVVar varName) tl i) -> [TypeVar (TVVar $ makeAbsolutePk varName) tl i]
      val' -> [singletonType key, val']
unionReachesTree typeEnv vaenv (ReachesTypeTree children) = unionAllTypesWithEnvOnce typeEnv vaenv (H.keys children ++ map (unionReachesTree typeEnv vaenv . snd) (H.elems children))
unionReachesTree _ _ ReachesLeaf = BottomType

joinReachesTrees :: ReachesTree -> ReachesTree -> ReachesTree
joinReachesTrees (ReachesPartialTree a) (ReachesPartialTree b) = ReachesPartialTree $ H.unionWith joinReachesTrees a b
joinReachesTrees ReachesLeaf ReachesLeaf = ReachesLeaf
joinReachesTrees (ReachesPartialTree t) v | H.null t = v
joinReachesTrees v (ReachesPartialTree t) | H.null t = v
joinReachesTrees a b = error $ printf "joinReachesTrees for mixed tree and leaf not yet defined: \n\t%s\n\t%s" (show a) (show b)

joinAllReachesTrees :: Foldable f => f ReachesTree -> ReachesTree
joinAllReachesTrees = foldr1 joinReachesTrees

reachesHasCutSubtypeOf :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf typeEnv vaenv (ReachesPartialTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypeOfWithEnv typeEnv vaenv (singletonType key) superType || reachesHasCutSubtypeOf typeEnv vaenv val superType
reachesHasCutSubtypeOf typeEnv vaenv (ReachesTypeTree children) superType = any childIsSubtype $ H.toList children
  where childIsSubtype (key, (_, val)) = isSubtypeOfWithEnv typeEnv vaenv key superType || reachesHasCutSubtypeOf typeEnv vaenv val superType
reachesHasCutSubtypeOf _ _ ReachesLeaf _ = False

-- | Cuts a reaches tree to a required destination. At a source partial's choice node
-- ('ReachesTypeTree'), a child arrow is GUARANTEED when its claimed type satisfies the
-- requirement, or its subtree can be cut to within it ('reachesHasCutSubtypeOf' — the arrows
-- BEYOND it reach the requirement, which is how a claim that must rewrite further is judged:
-- by the paths leading out of it, not by exempting it from the check). When the guaranteed
-- children jointly COVER the source partial — every value the source admits is accepted by
-- some guaranteed arrow — only they are kept: the requirement forces the arrow choice, and
-- no input is stranded by ignoring the others. Without joint coverage the node is left whole:
-- the remaining arrows are value dispatch (each accepting inputs the guaranteed ones do not),
-- which the compiler is not free to choose among. Required nodes ('ReachesPartialTree') keep
-- every child — their keys are the source terms themselves, which eval needs intact.
cutReachesTree :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> ReachesTree -> ReachesTree
cutReachesTree typeEnv vaenv req (ReachesPartialTree children) = ReachesPartialTree $ H.mapWithKey cutChild children
  where
    cutChild srcPartial (ReachesTypeTree arms) = ReachesTypeTree $ fmap recurseArm arms'
      where
        guaranteed key (_, subTree) = isSubtypeOfWithEnv typeEnv vaenv key req || reachesHasCutSubtypeOf typeEnv vaenv subTree req
        kept = H.filterWithKey guaranteed arms
        -- The portion of the source each kept arm accepts, re-queried from the graph (the tree
        -- keeps its lean committed shape; see 'typeGraphQueryWithReason').
        keptCoverage = unionAllTypesWithEnvOnce typeEnv vaenv
          [ coveredSrc
          | (reason, coveredSrc, dest) <- typeGraphQueryWithReason typeEnv vaenv srcPartial
          , Just (keptReason, _) <- [H.lookup dest kept]
          , reason == keptReason ]
        covers = isSubtypeOfWithEnv typeEnv vaenv (singletonType srcPartial) keptCoverage
        arms' = if not (H.null kept) && covers then kept else arms
        recurseArm (reason, subTree) = (reason, cutReachesTree typeEnv vaenv req subTree)
    cutChild _ subTree = cutReachesTree typeEnv vaenv req subTree
cutReachesTree typeEnv vaenv req (ReachesTypeTree children) = ReachesTypeTree $ fmap (fmap (cutReachesTree typeEnv vaenv req)) children
cutReachesTree _ _ _ ReachesLeaf = ReachesLeaf

-- | A SETTLED leaf is a data value: one whose name has a data definition. Everything else —
-- a function-named applied term, including one whose name is still relative (an exact graph
-- lookup would miss it) — is a term the graph may still rewrite. Only settled leaves are
-- what a requirement constrains; the rewritable ones settle at their own points.
isSettledLeaf :: (TypeGraph tg) => TypeEnv tg -> PartialType -> Bool
isSettledLeaf typeEnv l = not (null (typeGraphData typeEnv (ptName l)))


reachesPartials :: (TypeGraph tg) => ReachesEnv tg -> [PartialType] -> ReachesTree
reachesPartials typeEnv partials = ReachesPartialTree $ H.fromList $ zip partials (map (reachesPartial typeEnv) partials)
  where
    reachesPartial :: (TypeGraph tg) => ReachesEnv tg -> PartialType -> ReachesTree
    reachesPartial ReachesEnv{rVisited} p | S.member p rVisited = ReachesLeaf
    reachesPartial ReachesEnv{rVaenv} PartialType{ptName=argName} | TVArg (partialKey argName) `H.member` rVaenv = ReachesTypeTree $ H.singleton (TypeVar (TVArg $ partialKey argName) TVInt PTopType) ("arg", ReachesLeaf)
    reachesPartial env@ReachesEnv{rTypeEnv, rVisited, rVaenv} partial = do

      let ttypes = typeGraphQueryWithReason rTypeEnv rVaenv partial

      let env' = env{rVisited=S.insert partial rVisited}
      if null ttypes
        then ReachesLeaf
        else ReachesTypeTree $ H.fromList [(dest, (reason, reaches env' dest)) | (reason, _, dest) <- ttypes]

-- | Union a type's leaves with their declared result (@leaf ∪ result@), so a form is recognised
-- by what its arrows produce as well as by what it is. A recursive call in an argument position
-- (@insert(lst= sorted(raw))@) matches no arrow object structurally (@sorted(raw) ∩ Nil = ∅@);
-- unioning @sorted@'s declared result — 'PTopType' until inferred, else e.g. @ConsList@ — lets
-- both @insert@ arms apply, so the reach stays a genuine upper bound. Intersecting a class-top
-- result against a concrete member is transitive (@∀Functor ∩ Cons = Cons@ via
-- 'expandClassPartialWithNamesFull'), so the arms the produced value would reach are followed.
--
-- Every leaf is asked, not only the function-named ones. What a name is declared as says nothing
-- about whether arrows leave it — a data type may be the source of one — and a leaf with no
-- arrows answers 'PTopType' and is skipped by the guard below, so asking costs nothing where
-- there is nothing to add. Coverage is judged against the ACCUMULATING union itself (which always
-- contains the whole original union): a form whose declared result is ALREADY present adds
-- nothing, so a leaf standing beside what it produces is kept exact. One level only: the query
-- inspects direct arguments.
presentRawResult :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type
presentRawResult typeEnv vaenv t@(UnionType Nothing leafs _) =
  foldr addResult t (splitUnionType leafs)
  where
    -- A 'PTopType' result (a genuinely undeclared function) carries no settled data to admit and
    -- would only widen the union to Top, so it is not added; only a concrete or class-top result is.
    addResult l acc =
      let r = typeGraphDeclaredResult typeEnv vaenv l
      in if isBottomType r || isTop r || isSubtypeOfWithEnv typeEnv vaenv r acc
           then acc
           else unionTypesWithEnv typeEnv vaenv r acc
    isTop PTopType = True
    isTop _        = False
presentRawResult _ _ t = t

reaches :: (TypeGraph tg) => ReachesEnv tg -> Type -> ReachesTree
reaches _     (UnionType (Just _) _ _) = ReachesLeaf
reaches _     (TypeVar v _ _) = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType Nothing src consts) =
  reachesPartials env $ splitUnionType src ++ map constantPartialType consts

reachesTo :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
reachesTo typeEnv vaenv srcType = reachesHasCutSubtypeOf typeEnv vaenv reached
  where
    reachesEnv = ReachesEnv typeEnv vaenv S.empty
    reached = reaches reachesEnv srcType
