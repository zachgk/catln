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
import           Data.Aeson          (ToJSON)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List
import           GHC.Generics        (Generic)
import           Semantics.Types
import           Text.Printf

data ReachesTree
  = ReachesTree !(H.HashMap PartialType ReachesTree)
  | ReachesLeaf ![Type]
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON)

class TypeGraph tg where
  typeGraphQuery :: ReachesEnv tg -> PartialType -> [Type]

data ReachesEnv tg = ReachesEnv {
  rTypeEnv   :: !TypeEnv,
  rVaenv     :: !TypeVarArgEnv,
  rTypeGraph :: !tg,
  rVisited   :: !(S.HashSet PartialType)
                             }

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _         = False

unionReachesTree :: TypeEnv -> ReachesTree -> Type
unionReachesTree classGraph (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = UnionType $ joinUnionType keys
  let vals' = map (unionReachesTree classGraph) vals
  let both = keys':vals'
  case partition isTypeVar both of
    ([onlyVar], []) -> onlyVar
    ([], sums)       -> unionAllTypes classGraph sums
    ([TypeVar (TVArg argName) tl], [UnionType leafs]) | all (\PartialType{ptName=n} -> makeAbsoluteName n == makeAbsoluteName (pkName argName)) (splitUnionType leafs) -> TypeVar (TVArg $ makeAbsolutePk argName) tl
    ([TypeVar (TVVar varName) tl], [UnionType _]) -> TypeVar (TVVar $ makeAbsolutePk varName) tl
    (vars, partials)       -> error $ printf "Not yet implemented unionReachesTree with vars %s and partials %s" (show vars) (show partials)
unionReachesTree classGraph (ReachesLeaf leafs) = unionAllTypes classGraph leafs

joinReachesTrees :: ReachesTree -> ReachesTree -> ReachesTree
joinReachesTrees (ReachesTree a) (ReachesTree b) = ReachesTree $ H.unionWith joinReachesTrees a b
joinReachesTrees (ReachesLeaf a) (ReachesLeaf b) = ReachesLeaf (a ++ b)
joinReachesTrees (ReachesTree t) v | H.null t = v
joinReachesTrees v (ReachesTree t) | H.null t = v
joinReachesTrees a b = error $ printf "joinReachesTrees for mixed tree and leaf not yet defined: \n\t%s\n\t%s" (show a) (show b)

joinAllReachesTrees :: Foldable f => f ReachesTree -> ReachesTree
joinAllReachesTrees = foldr1 joinReachesTrees

reachesHasCutSubtypeOf :: TypeEnv -> TypeVarArgEnv -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classGraph vaenv (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypeOfWithEnv classGraph vaenv (singletonType key) superType || reachesHasCutSubtypeOf classGraph vaenv val superType
reachesHasCutSubtypeOf classGraph vaenv (ReachesLeaf leafs) superType = any (\t -> isSubtypeOfWithEnv classGraph vaenv t superType) leafs

reachesPartial :: (TypeGraph tg) => ReachesEnv tg -> PartialType -> ReachesTree
reachesPartial ReachesEnv{rVisited} p | S.member p rVisited = ReachesLeaf []
reachesPartial ReachesEnv{rVaenv} PartialType{ptName=argName} | TVArg (partialKey argName) `H.member` rVaenv = ReachesLeaf [TypeVar (TVArg $ partialKey argName) TVInt]
reachesPartial env@ReachesEnv{rTypeEnv, rVisited} partial = do

  let ttypes = typeGraphQuery env partial

  let env' = env{rVisited=S.insert partial rVisited}
  if null ttypes
    then ReachesLeaf []
    else reaches env' (unionAllTypes rTypeEnv ttypes)

reaches :: (TypeGraph tg) => ReachesEnv tg -> Type -> ReachesTree
reaches _     PTopType            = ReachesLeaf [PTopType]
reaches _     v@TopType{}            = ReachesLeaf [v]
reaches _     (TypeVar v _)            = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType src) = ReachesTree $ H.fromList $ zip partials resultsByPartials
  where
    partials = splitUnionType src
    resultsByPartials = map (reachesPartial env) partials

rootReachesPartial :: (TypeGraph tg) => ReachesEnv tg -> PartialType -> (PartialType, ReachesTree)
rootReachesPartial env src = (src, reachedWithId)
  where
    reached = reachesPartial env src
    reachedWithId = ReachesTree $ H.singleton src reached
