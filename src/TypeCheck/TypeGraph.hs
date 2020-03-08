--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeGraph
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.TypeGraph where

import qualified Data.HashMap.Strict           as H
import qualified Data.HashSet                  as S
import           Control.Monad.ST
import           Control.Monad
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common

objectToLeaf :: FEnv s -> VObject s -> ST s RawLeafType
objectToLeaf env (Object _ name args) = do
        args' <- mapM
                (\argMeta -> do
                        (SType _ (RawSumType upper)) <- descriptor
                                $ getPnt argMeta
                        return $ head $ S.toList upper
                )
                args
        return $ RawLeafType name args'

buildTypeGraph :: FEnv s -> VObjectMap s -> ST s (FEnv s, TypeGraph s)
buildTypeGraph env = foldM addArrows (env, emptyGraph)
    where
        emptyGraph = H.empty
        addArrows (env, graph) (obj, arrows) = foldM (addArrow obj) (env, graph) arrows
        addArrow obj (env, graph) (Arrow m _) = do
                leaf <- objectToLeaf env obj
                let graph2 = H.insertWith (++) leaf [m] graph
                return (env, graph2)

rawTypeFromScheme :: Scheme -> Maybe RawType
rawTypeFromScheme (SType ub _)  = Just ub
rawTypeFromScheme SCheckError{} = Nothing

unionMaybeRawTypes :: [Maybe RawType] -> Maybe RawType
unionMaybeRawTypes maybeRawTypes = case sequence maybeRawTypes of
        Just rawType -> Just $ foldr unionRawTypes RawBottomType rawType
        Nothing      -> Nothing

reachesLeaf :: TypeGraph s -> RawLeafType -> ST s (Maybe RawType)
reachesLeaf graph leaf = do
  let typePnts = H.lookupDefault [] leaf graph
  schemes <- mapM descriptor typePnts
  let maybeRawTypes = map rawTypeFromScheme schemes
  return $ unionMaybeRawTypes (Just (RawSumType $ S.singleton leaf) : maybeRawTypes)

reaches :: TypeGraph s -> RawType -> ST s (Maybe RawType)
reaches _     RawTopType            = return $ Just RawTopType
reaches _     RawBottomType         = return $ Just RawBottomType
reaches graph (RawSumType srcTypes) = do
        resultParts <- mapM (reachesLeaf graph) $ S.toList srcTypes
        return $ unionMaybeRawTypes resultParts
