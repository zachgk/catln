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
                          (TypeCheckResult _ (SType (RawSumType upper _) _ _)) -> return $ head $ S.toList upper
                          _ -> error "bad objectToLeaf"
                )
                args
        return $ RawLeafType name args'

buildTypeGraph :: FEnv s -> VObjectMap s -> ST s (FEnv s, TypeGraph s, [Constraint s])
buildTypeGraph env objMap = do
  graphObjs <- fresh $ TypeCheckResult [] $ SType RawTopType rawBottomType "typeGraph"
  let emptyGraph = (graphObjs, H.empty)
  (env2, typeGraph, pnts) <- foldM addArrows (env, emptyGraph, []) objMap
  return (env2, typeGraph, [UnionOf graphObjs pnts])
    where
        addArrows :: (FEnv s, TypeGraph s, [Pnt s]) -> (VObject s, [VArrow s]) -> ST s (FEnv s, TypeGraph s, [Pnt s])
        addArrows (aenv, (graphObjs, graphLeafs), pnts) (obj@(Object m _ _), arrows) = do
          (aenv2, graphLeafs') <- foldM (addArrow obj) (aenv, graphLeafs) arrows
          return (aenv2, (graphObjs, graphLeafs'), m:pnts)
        addArrow obj (aenv, graphLeafs) (Arrow m _ _) = do
                leaf <- objectToLeaf obj
                let graphLeafs2 = H.insertWith (++) leaf [m] graphLeafs
                return (aenv, graphLeafs2)

boundSchemeByGraphObjects :: TypeGraph s -> Scheme -> ST s Scheme
boundSchemeByGraphObjects _ (TypeCheckResE err) = return $ TypeCheckResE err
boundSchemeByGraphObjects (graphObjs, _) (TypeCheckResult notes (SType ub lb desc)) = do
  graphObjScheme <- descriptor graphObjs
  case graphObjScheme of
    TypeCheckResE err -> return $ TypeCheckResE (err ++ notes)
    TypeCheckResult notes2 (SType gub _ _) -> do
      let ub' = intersectRawTypeWithPowerset ub gub
      return $ TypeCheckResult (notes2 ++ notes) (SType ub' lb desc)

rawTypeFromScheme :: Scheme -> Maybe RawType
rawTypeFromScheme (TypeCheckResult _ (SType ub _ _))  = Just ub
rawTypeFromScheme TypeCheckResE{} = Nothing

unionMaybeRawTypes :: [Maybe RawType] -> Maybe RawType
unionMaybeRawTypes maybeRawTypes = case sequence maybeRawTypes of
        Just [] -> Nothing
        Just rawType -> Just $ foldr unionRawTypes rawBottomType rawType
        Nothing      -> Nothing

reachesLeaf :: TypeGraph s -> RawLeafType -> ST s (Maybe RawType)
reachesLeaf (_, graphLeafs) leaf = do
  let typePnts = H.lookupDefault [] leaf graphLeafs
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
