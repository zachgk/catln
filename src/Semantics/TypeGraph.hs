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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Semantics.TypeGraph where
import           CRes
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.Maybe
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf

data ReachesTree
  = ReachesTree !(H.HashMap PartialType ReachesTree)
  | ReachesLeaf ![Type]
  deriving (Show)

data ReachesEnv m = ReachesEnv {
  rClassGraph   :: !ClassGraph,
  rUnionAllType :: !Type,
  rTypeGraph    :: !(H.HashMap TypeName [ObjArr Expr m])
                             }

isTypeVar :: Type -> Bool
isTypeVar TypeVar{} = True
isTypeVar _         = False

unionReachesTree :: ClassGraph -> ReachesTree -> Type
unionReachesTree classGraph (ReachesTree children) = do
  let (keys, vals) = unzip $ H.toList children
  let keys' = UnionType $ joinUnionType keys
  let vals' = map (unionReachesTree classGraph) vals
  let both = keys':vals'
  case partition isTypeVar both of
    ([onlyVar], []) -> onlyVar
    (_, sums)       -> unionAllTypes classGraph sums
unionReachesTree classGraph (ReachesLeaf leafs) = unionAllTypes classGraph leafs

joinReachesTrees :: ReachesTree -> ReachesTree -> ReachesTree
joinReachesTrees (ReachesTree a) (ReachesTree b) = ReachesTree $ H.unionWith joinReachesTrees a b
joinReachesTrees (ReachesLeaf a) (ReachesLeaf b) = ReachesLeaf (a ++ b)
joinReachesTrees a b = error $ printf "joinReachesTrees for mixed tree and leaf not yet defined: \n\t%s\n\t%s" (show a) (show b)

joinAllReachesTrees :: Foldable f => f ReachesTree -> ReachesTree
joinAllReachesTrees = foldr1 joinReachesTrees

isSubtypePartialOfWithMaybeObj :: (Show m, Show (e m), MetaDat m, ExprClass e) => ClassGraph -> Maybe (Object e m) -> PartialType -> Type -> Bool
isSubtypePartialOfWithMaybeObj classGraph (Just obj) = isSubtypePartialOfWithObj classGraph obj
isSubtypePartialOfWithMaybeObj classGraph Nothing    = isSubtypePartialOf classGraph

reachesHasCutSubtypeOf :: (Show m, MetaDat m) => ClassGraph -> MetaVarEnv m -> MetaArgEnv m -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classGraph varEnv argEnv (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypePartialOfWithMetaEnv classGraph varEnv argEnv key superType || reachesHasCutSubtypeOf classGraph varEnv argEnv val superType
reachesHasCutSubtypeOf classGraph varEnv argEnv (ReachesLeaf leafs) superType = any (\t -> isSubtypeOfWithMetaEnv classGraph varEnv argEnv t superType) leafs

reachesPartial :: (MetaDat m, Show m) => ReachesEnv m -> PartialType -> CRes ReachesTree
reachesPartial ReachesEnv{rTypeGraph, rClassGraph} partial@PartialType{ptName=PTypeName name} = do

  let ttypeArrows = H.lookupDefault [] name rTypeGraph
  ttypes <- mapM tryTArrow ttypeArrows

  return $ ReachesLeaf (catMaybes ttypes)
  where
    tryTArrow oa = do
      -- It is possible to send part of a partial through the arrow, so must compute the valid part
      -- If none of it is valid, then there is Nothing
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes rClassGraph (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then return $ Just $ unionAllTypes rClassGraph [arrowDestType True rClassGraph potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
        else return Nothing
reachesPartial env@ReachesEnv{rClassGraph} partial@PartialType{ptName=PClassName{}} = reaches env (expandPartial rClassGraph partial)
reachesPartial env@ReachesEnv{rClassGraph, rUnionAllType} partial@PartialType{ptName=PRelativeName{}} = do
  reachesAsClass <- reaches env (expandPartial rClassGraph partial)
  let (UnionType allObjsUb) = rUnionAllType
  reachesAsType <- mapM (reachesPartial env . (\name -> partial{ptName=name})) (H.keys allObjsUb)
  return $ joinReachesTrees reachesAsClass (joinAllReachesTrees reachesAsType)

reaches :: (MetaDat m, Show m) => ReachesEnv m -> Type -> CRes ReachesTree
reaches _     TopType            = return $ ReachesLeaf [TopType]
reaches _     (TypeVar v)            = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType src) = do
  let partials = splitUnionType src
  resultsByPartials <- mapM (reachesPartial env) partials
  return $ ReachesTree $ H.fromList $ zip partials resultsByPartials

rootReachesPartial :: (MetaDat m, Show m) => ReachesEnv m -> PartialType -> CRes (PartialType, ReachesTree)
rootReachesPartial env src = do
  reached <- reachesPartial env src
  let reachedWithId = ReachesTree $ H.singleton src reached
  return (src, reachedWithId)
