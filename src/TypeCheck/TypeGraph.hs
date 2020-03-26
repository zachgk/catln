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

objectToLeaf :: VObject s -> ST s RawLeafType
objectToLeaf (Object _ name args) = do
        args' <- mapM
                (\argMeta -> do
                        scheme <- descriptor $ getPnt argMeta
                        case scheme of
                          (Right (SType (RawSumType upper _) _ _)) -> return $ head $ S.toList upper
                          _ -> error "bad objectToLeaf"
                )
                args
        return $ RawLeafType name args'

buildTypeGraph :: FEnv s -> VObjectMap s -> ST s (FEnv s, TypeGraph s)
buildTypeGraph env = foldM addArrows (env, emptyGraph)
    where
        emptyGraph = H.empty
        addArrows (aenv, graph) (obj, arrows) = foldM (addArrow obj) (aenv, graph) arrows
        addArrow obj (aenv, graph) (Arrow m _ _) = do
                leaf <- objectToLeaf obj
                let graph2 = H.insertWith (++) leaf [m] graph
                return (aenv, graph2)

rawTypeFromScheme :: Scheme -> Maybe RawType
rawTypeFromScheme (Right (SType ub _ _))  = Just ub
rawTypeFromScheme Left{} = Nothing

unionMaybeRawTypes :: [Maybe RawType] -> Maybe RawType
unionMaybeRawTypes maybeRawTypes = case sequence maybeRawTypes of
        Just [] -> Nothing
        Just rawType -> Just $ foldr unionRawTypes rawBottomType rawType
        Nothing      -> Nothing

reachesLeaf :: TypeGraph s -> RawLeafType -> ST s (Maybe RawType)
reachesLeaf graph leaf = do
  let typePnts = H.lookupDefault [] leaf graph
  schemes <- mapM descriptor typePnts
  let maybeRawTypes = map rawTypeFromScheme schemes
  return $ unionMaybeRawTypes (Just (RawSumType (S.singleton leaf) H.empty) : maybeRawTypes)

reaches :: TypeGraph s -> RawType -> ST s (Maybe RawType)
reaches _     RawTopType            = return $ Just RawTopType
reaches graph (RawSumType srcLeafs srcPartials) = if H.null srcPartials
  then do
    resultParts <- mapM (reachesLeaf graph) $ S.toList srcLeafs
    return $ unionMaybeRawTypes resultParts
  else return $ Just RawTopType -- TODO: add reachesPartial method
