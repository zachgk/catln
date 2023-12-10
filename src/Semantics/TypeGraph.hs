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
  rTypeEnv      :: !TypeEnv,
  rUnionAllType :: !Type,
  rVaenv        :: !TypeVarArgEnv,
  rTypeGraph    :: !(H.HashMap TypeName [ObjArr Expr m])
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
    ([TypeVar (TVArg argName) tl], [UnionType leafs]) | all (\PartialType{ptName=n} -> makeAbsoluteName (fromPartialName n) == makeAbsoluteName (pkName argName)) (splitUnionType leafs) -> TypeVar (TVArg $ makeAbsolutePk argName) tl
    (_, _)       -> error $ printf "Not yet implemented unionReachesTree with vars and partials of %s" (show both)
unionReachesTree classGraph (ReachesLeaf leafs) = unionAllTypes classGraph leafs

joinReachesTrees :: ReachesTree -> ReachesTree -> ReachesTree
joinReachesTrees (ReachesTree a) (ReachesTree b) = ReachesTree $ H.unionWith joinReachesTrees a b
joinReachesTrees (ReachesLeaf a) (ReachesLeaf b) = ReachesLeaf (a ++ b)
joinReachesTrees (ReachesTree t) v | H.null t = v
joinReachesTrees v (ReachesTree t) | H.null t = v
joinReachesTrees a b = error $ printf "joinReachesTrees for mixed tree and leaf not yet defined: \n\t%s\n\t%s" (show a) (show b)

joinAllReachesTrees :: Foldable f => f ReachesTree -> ReachesTree
joinAllReachesTrees = foldr1 joinReachesTrees

reachesHasCutSubtypeOf :: (Show m, MetaDat m) => TypeEnv -> MetaVarArgEnv m -> ReachesTree -> Type -> Bool
reachesHasCutSubtypeOf classGraph vaenv (ReachesTree children) superType = all childIsSubtype $ H.toList children
  where childIsSubtype (key, val) = isSubtypePartialOfWithMetaEnv classGraph vaenv key superType || reachesHasCutSubtypeOf classGraph vaenv val superType
reachesHasCutSubtypeOf classGraph vaenv (ReachesLeaf leafs) superType = any (\t -> isSubtypeOfWithMetaEnv classGraph vaenv t superType) leafs

reachesPartial :: (MetaDat m, Show m) => ReachesEnv m -> PartialType -> CRes ReachesTree
reachesPartial ReachesEnv{rVaenv} PartialType{ptName=PTypeName argName} | TVArg (partialKey argName) `H.member` rVaenv = return $ ReachesLeaf [TypeVar (TVArg $ partialKey argName) TVInt]
reachesPartial ReachesEnv{rTypeGraph, rTypeEnv} partial@PartialType{ptName=PTypeName name} = do

  let ttypeArrows = H.lookupDefault [] name rTypeGraph
  ttypes <- mapM tryTArrow ttypeArrows

  return $ ReachesLeaf (catMaybes ttypes)
  where
    tryTArrow oa = do
      -- It is possible to send part of a partial through the arrow, so must compute the valid part
      -- If none of it is valid, then there is Nothing
      let potentialSrc@(UnionType potSrcLeafs) = intersectTypes rTypeEnv (singletonType partial) (getMetaType $ getExprMeta $ oaObjExpr oa)
      if not (isBottomType potentialSrc)
        -- TODO: Should this line below call `reaches` to make this recursive?
        -- otherwise, no reaches path requiring multiple steps can be found
        then return $ Just $ unionAllTypes rTypeEnv [arrowDestType True rTypeEnv potentialSrcPartial oa | potentialSrcPartial <- splitUnionType potSrcLeafs]
        else return Nothing
reachesPartial env@ReachesEnv{rTypeEnv} partial@PartialType{ptName=PClassName{}} = reaches env (expandPartial rTypeEnv H.empty partial)
reachesPartial env@ReachesEnv{rTypeEnv} PartialType{ptName=(PRelativeName name)} = reaches env (expandRelPartial rTypeEnv H.empty name)

reaches :: (MetaDat m, Show m) => ReachesEnv m -> Type -> CRes ReachesTree
reaches _     (TopType [])            = return $ ReachesLeaf [topType]
reaches _     (TopType _)            = undefined
reaches _     (TypeVar v _)            = error $ printf "reaches with typevar %s" (show v)
reaches env (UnionType src) = do
  let partials = splitUnionType src
  resultsByPartials <- mapM (reachesPartial env) partials
  return $ ReachesTree $ H.fromList $ zip partials resultsByPartials

rootReachesPartial :: (MetaDat m, Show m) => ReachesEnv m -> PartialType -> CRes (PartialType, ReachesTree)
rootReachesPartial env src = do
  reached <- reachesPartial env src
  let reachedWithId = ReachesTree $ H.singleton src reached
  return (src, reachedWithId)
