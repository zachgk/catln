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
import           Data.Tuple.Sequence

import           Syntax.Types
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
        addArrow obj (aenv, graphLeafs) (Arrow m _ _ _) = do
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

ubFromScheme :: Scheme -> TypeCheckResult RawType
ubFromScheme (TypeCheckResult _ (SType ub _ _))  = return ub
ubFromScheme (TypeCheckResE notes) = TypeCheckResE notes

unionRawTypeList :: [RawType] -> RawType
unionRawTypeList = foldr unionRawTypes rawBottomType

reachesLeaf :: TypeGraph s -> RawLeafType -> ST s (TypeCheckResult RawType)
reachesLeaf (_, graphLeafs) leaf = do
  let typePnts = H.lookupDefault [] leaf graphLeafs
  schemes <- mapM descriptor typePnts
  let joined = return (RawSumType (S.singleton leaf) H.empty) : map ubFromScheme schemes
  return $ unionRawTypesList <$> sequence joined

reachesPartial :: TypeGraph s -> RawPartialType -> ST s (TypeCheckResult RawType)
reachesPartial _ _ = return $ return RawTopType -- TODO: build real reachesPartial method

reaches :: TypeGraph s -> RawType -> ST s (TypeCheckResult RawType)
reaches _     RawTopType            = return $ return RawTopType
reaches graph (RawSumType srcLeafs srcPartials) = do
  resultsByLeafs <- mapM (reachesLeaf graph) $ S.toList srcLeafs
  resultsByPartials <- mapM (reachesPartial graph) $ splitPartialLeafs srcPartials
  return $ unionRawTypeList <$> sequence (resultsByPartials ++ resultsByLeafs)


arrowConstrainUbs :: TypeGraph s -> RawType -> RawType -> ST s (TypeCheckResult (RawType, RawType))
arrowConstrainUbs _ RawTopType dest = return $ return (RawTopType, dest)
arrowConstrainUbs typeGraph (RawSumType srcLeafs srcPartials) dest = do
  let srcLeafList = S.toList srcLeafs
  leafMap <- sequence $ H.fromList $ zip srcLeafList $ map (reachesLeaf typeGraph) srcLeafList
  let srcPartialList = splitPartialLeafs srcPartials
  partialMap <- sequence $ H.fromList $ zip srcPartialList $ map (reachesPartial typeGraph) srcPartialList
  let leafMap' = H.filter (`hasRawType` dest) <$> sequence leafMap
  let partialMap' = H.filter (`hasRawType` dest) <$> sequence partialMap
  return $ do
    (leafMap'', partialMap'') <- sequenceT (leafMap', partialMap')
    let (srcLeafList', destByLeaf) = unzip $ H.toList leafMap''
    let (srcPartialList', destByPartial) = unzip $ H.toList partialMap''
    let srcLeafs' = S.fromList srcLeafList'
    let srcPartials' = joinPartialLeafs srcPartialList'
    let destByGraph = unionRawTypeList (destByLeaf ++ destByPartial)
    dest' <- tryIntersectRawTypes dest destByGraph "executeConstraint ArrowTo"
    return (RawSumType srcLeafs' srcPartials', dest')
