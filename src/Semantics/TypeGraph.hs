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
import           Data.Bifunctor      (Bifunctor (second))
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

unionReachesTree :: TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> Type
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

reachesHasCutSubtypeOf :: TypeEnv tg -> TypeVarArgEnv -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf typeEnv vaenv (ReachesPartialTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypeOfWithEnv typeEnv vaenv (singletonType key) superType || reachesHasCutSubtypeOf typeEnv vaenv val superType
reachesHasCutSubtypeOf typeEnv vaenv (ReachesTypeTree children) superType = any childIsSubtype $ H.toList children
  where childIsSubtype (key, (_, val)) = isSubtypeOfWithEnv typeEnv vaenv key superType || reachesHasCutSubtypeOf typeEnv vaenv val superType
reachesHasCutSubtypeOf _ _ ReachesLeaf _ = False

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
        else ReachesTypeTree $ H.fromList $ zip (map snd ttypes) (map (second (reaches env')) ttypes)

reaches :: (TypeGraph tg) => ReachesEnv tg -> Type -> ReachesTree
reaches _     TopType{}     = ReachesLeaf
reaches _     (TypeVar v _) = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType src) = reachesPartials env $ splitUnionType src

reachesTo :: (TypeGraph tg) => TypeEnv tg -> TypeVarArgEnv -> Type -> Type -> Bool
reachesTo typeEnv vaenv srcType = reachesHasCutSubtypeOf typeEnv vaenv reached
  where
    reachesEnv = ReachesEnv typeEnv vaenv S.empty
    reached = reaches reachesEnv srcType
