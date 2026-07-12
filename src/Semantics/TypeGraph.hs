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
unionReachesTree typeEnv vaenv (ReachesPartialTree children) = unionAllTypesWithEnv typeEnv vaenv $ map unionChild $ H.toList children
  where
    unionChild (key, val) = case unionReachesTree typeEnv vaenv val of
      (TypeVar (TVArg argName) tl) | makeAbsoluteName (ptName key) == makeAbsoluteName (pkName argName) -> TypeVar (TVArg $ makeAbsolutePk argName) tl
      (TypeVar (TVVar varName) tl) -> TypeVar (TVVar $ makeAbsolutePk varName) tl
      val' -> unionTypesWithEnv typeEnv vaenv (singletonType key) val'
unionReachesTree typeEnv vaenv (ReachesTypeTree children) = unionAllTypesWithEnv typeEnv vaenv (H.keys children ++ map (unionReachesTree typeEnv vaenv . snd) (H.elems children))
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
-- requirement ('isSubtypeUpToSettlingOf' — raw applied claims judged by what they settle to)
-- or its subtree can be cut to within it ('reachesHasCutSubtypeOf'). When the guaranteed
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
        guaranteed key (_, subTree) = isSubtypeUpToSettlingOf typeEnv vaenv key req || reachesHasCutSubtypeOf typeEnv vaenv subTree req
        kept = H.filterWithKey guaranteed arms
        -- The portion of the source each kept arm accepts, re-queried from the graph (the tree
        -- keeps its lean committed shape; see 'typeGraphQueryWithReason').
        keptCoverage = unionAllTypesWithEnv typeEnv vaenv
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

-- | @sub ⊆ sup@ up to settling: a requirement constrains what a term settles to, so a
-- positive leaf the type graph may still rewrite (a raw applied call, kept alongside its
-- settled forms for eval's dispatch keying) is exempt from the check — the results its
-- arrows produce carry their own bounds at their own points. Settled leaves (data values,
-- 'isSettledLeaf'), constants, and the predicate component are checked structurally.
isSubtypeUpToSettlingOf :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
isSubtypeUpToSettlingOf typeEnv vaenv sub sup = case sub of
  UnionType mTop leafs consts ->
    isSubtypeOfWithEnv typeEnv vaenv (UnionType mTop H.empty consts) sup
      && all leafOk (splitUnionType leafs)
  _ -> isSubtypeOfWithEnv typeEnv vaenv sub sup
  where
    leafOk l = not (isSettledLeaf typeEnv l)
             || isSubtypeOfWithEnv typeEnv vaenv (singletonType l) sup

reachesPartials :: (TypeGraph tg) => ReachesEnv tg -> [PartialType] -> ReachesTree
reachesPartials typeEnv partials = ReachesPartialTree $ H.fromList $ zip partials (map (reachesPartial typeEnv) partials)
  where
    reachesPartial :: (TypeGraph tg) => ReachesEnv tg -> PartialType -> ReachesTree
    reachesPartial ReachesEnv{rVisited} p | S.member p rVisited = ReachesLeaf
    reachesPartial ReachesEnv{rVaenv} PartialType{ptName=argName} | TVArg (partialKey argName) `H.member` rVaenv = ReachesTypeTree $ H.singleton (TypeVar (TVArg $ partialKey argName) TVInt) ("arg", ReachesLeaf)
    reachesPartial env@ReachesEnv{rTypeEnv, rVisited, rVaenv} partial = do

      let ttypes = typeGraphQueryWithReason rTypeEnv rVaenv partial

      let env' = env{rVisited=S.insert partial rVisited}
      if null ttypes
        then ReachesLeaf
        else ReachesTypeTree $ H.fromList [(dest, (reason, reaches env' dest)) | (reason, _, dest) <- ttypes]

reaches :: (TypeGraph tg) => ReachesEnv tg -> Type -> ReachesTree
reaches _     (UnionType (Just _) _ _) = ReachesLeaf
reaches _     (TypeVar v _) = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType Nothing src consts) =
  reachesPartials env $ splitUnionType src ++ map constantPartialType consts

reachesTo :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
reachesTo typeEnv vaenv srcType = reachesHasCutSubtypeOf typeEnv vaenv reached
  where
    reachesEnv = ReachesEnv typeEnv vaenv S.empty
    reached = reaches reachesEnv srcType
