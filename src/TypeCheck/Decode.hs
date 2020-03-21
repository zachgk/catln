--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Decode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module TypeCheck.Decode where

import           Control.Monad
import           Control.Monad.ST
import           Data.Either
import           Data.Functor
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import           Data.UnionFind.ST

import           Syntax
import           TypeCheck.Common
import           TypeCheck.Show (showCon)

mergeTypeCheckResultsList :: [TypeCheckResult r] -> TypeCheckResult [r]
mergeTypeCheckResultsList res = case partitionEithers res of
  ([], rs) -> Right rs
  (ls, _)  -> Left $ concat ls

mergeTypeCheckResultsMap :: (Eq a, Hashable a) => H.HashMap a (TypeCheckResult b) -> TypeCheckResult (H.HashMap a b)
mergeTypeCheckResultsMap res = fmap H.fromList $ mergeTypeCheckResultsList $ map (\(a, typeCheckResultB) -> fmap (a,) typeCheckResultB) $ H.toList res

mergeTypeCheckResultsPair :: (TypeCheckResult a, TypeCheckResult b) -> TypeCheckResult (a, b)
mergeTypeCheckResultsPair (a, b) = case (a,b) of
  (Right a', Right b') -> Right (a', b')
  (a', b')             -> Left $ fromLeft [] a' ++ fromLeft [] b'

mergeTypeCheckResultsTriple :: (TypeCheckResult a, TypeCheckResult b, TypeCheckResult c) -> TypeCheckResult (a, b, c)
mergeTypeCheckResultsTriple (a, b, c) = case (a, b, c) of
  (Right a', Right b', Right c') -> Right (a', b', c')
  (a', b', c')             -> Left $ fromLeft [] a' ++ fromLeft [] b' ++ fromLeft [] c'

mergeTypeCheckResultsQuad :: (TypeCheckResult a, TypeCheckResult b, TypeCheckResult c, TypeCheckResult d) -> TypeCheckResult (a, b, c, d)
mergeTypeCheckResultsQuad (a, b, c, d) = case (a, b, c, d) of
  (Right a', Right b', Right c', Right d') -> Right (a', b', c', d')
  (a', b', c', d')             -> Left $ fromLeft [] a' ++ fromLeft [] b' ++ fromLeft [] c' ++ fromLeft [] d'

fromRawLeafType :: RawLeafType -> LeafType
fromRawLeafType (RawLeafType name ts) = LeafType name (fmap fromRawLeafType ts)

fromRawType :: RawType -> Maybe Type
fromRawType RawTopType = Nothing
fromRawType (RawSumType ts) = Just $ SumType $ S.map fromRawLeafType ts

matchingConstraintHelper :: Pnt s -> Pnt s -> Pnt s -> ST s Bool
matchingConstraintHelper p p2 p3 = do
  c2 <- equivalent p p2
  c3 <- equivalent p p3
  return $ c2 || c3

matchingConstraint :: Pnt s -> Constraint s -> ST s Bool
matchingConstraint p (EqualsKnown p2 _) = equivalent p p2
matchingConstraint p (EqPoints p2 p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (BoundedBy p2 p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (IsTupleOf p2 args) = do
  c1 <- equivalent p p2
  c2 <- mapM (equivalent p) args
  return $ c1 || or c2
matchingConstraint p (ArrowTo p2 p3) = matchingConstraintHelper p p2 p3

type DEnv s = [Constraint s]
showMatchingConstraints :: [Constraint s] -> Pnt s -> ST s [SConstraint]
showMatchingConstraints cons matchVar = do
  filterCons <- filterM (matchingConstraint matchVar) cons
  mapM showCon filterCons

toMeta :: DEnv s -> VarMeta s -> String -> ST s (TypeCheckResult Typed)
toMeta env p name = do
  scheme <- descriptor p
  case scheme of
    SCheckError s -> return $ Left [GenTypeCheckError ("Scheme error on " ++ name ++ ": " ++ s)]
    SType ub _ _ -> case fromRawType ub of
      Nothing -> do
        showMatching <- showMatchingConstraints env p
        return $ Left [FailInfer name scheme showMatching]
      Just t -> return $ Right $ Typed t

toExpr :: DEnv s -> VExpr s -> ST s (TypeCheckResult TExpr)
toExpr env (CExpr m c) = do
  res <- toMeta env m $ "Constant " ++ show c
  return $ res <&> (`CExpr` c)
toExpr env (Value m name) = do
  m' <- toMeta env m $ "Value_" ++ name
  return $ fmap (`Value` name) m'
toExpr env (TupleApply m (baseM, baseExpr) args) = do
  m' <- toMeta env m "TupleApply_M"
  baseM' <- toMeta env baseM "TupleApply_baseM"
  baseExpr' <- toExpr env baseExpr
  args' <- mapM (toExpr env) args
  case m' of -- check for errors
    Right tp@(Typed (SumType sumType)) | all (\(LeafType _ leafArgs) -> H.keysSet args' /= H.keysSet leafArgs) (S.toList sumType) -> do
                                        matchingConstraints <- showMatchingConstraints env m
                                        let sArgs = mergeTypeCheckResultsMap args'
                                        return $ Left [TupleMismatch baseM' baseExpr' tp sArgs matchingConstraints]
    _ -> return $ (\(m'', baseM'', baseExpr'', args'') -> TupleApply m'' (baseM'', baseExpr'') args'') <$> mergeTypeCheckResultsQuad (m', baseM', baseExpr', mergeTypeCheckResultsMap args')

toCompAnnot :: DEnv s -> VCompAnnot s -> ST s (TypeCheckResult TCompAnnot)
toCompAnnot env (CompAnnot name args) = do
  args' <- mapM (toExpr env) args
  return $ fmap (CompAnnot name) (mergeTypeCheckResultsMap args')

toArrow :: DEnv s -> VArrow s -> ST s (TypeCheckResult TArrow)
toArrow env (Arrow m annots maybeExpr) = do
  m' <- toMeta env m "Arrow"
  annotsT <- mapM (toCompAnnot env) annots
  let annots' = mergeTypeCheckResultsList annotsT
  case maybeExpr of
    Just expr -> do
      expr' <- toExpr env expr
      return $ (\(m'', annots'', expr'') -> Arrow m'' annots'' (Just expr'')) <$> mergeTypeCheckResultsTriple (m', annots', expr')
    Nothing -> return $ (\(annots'', m'') -> Arrow m'' annots'' Nothing) <$> mergeTypeCheckResultsPair(annots', m')

toObjectArg :: DEnv s -> Name -> (Name, VarMeta s) -> ST s (TypeCheckResult (Name, Typed))
toObjectArg env objName (name, m) = do
  m' <- toMeta env m $ "Arg_" ++ objName ++ "." ++ name
  return $ case m' of
    Left m'' -> Left m''
    Right m'' -> Right (name, m'')

toObject :: DEnv s -> (VObject s, [VArrow s]) -> ST s (TypeCheckResult (TObject, [TArrow]))
toObject env (Object m name args, arrows) = do
  m' <- toMeta env m $ "Object_" ++ name
  args' <- mapM (toObjectArg env name) $ H.toList args
  let object' = (\(m'', args'') -> Object m'' name args'') <$> mergeTypeCheckResultsPair (m', H.fromList <$> mergeTypeCheckResultsList args')
  arrows' <- mapM (toArrow env) arrows
  let arrows'' = mergeTypeCheckResultsList arrows'
  return $ mergeTypeCheckResultsPair (object', arrows'')

toPrgm :: VPrgm s -> [Constraint s] -> ST s (TypeCheckResult TPrgm)
toPrgm (objMap, classMap) cons = do
  let env = cons
  objects' <- mapM (toObject env) objMap
  let objMap' = H.fromList <$> mergeTypeCheckResultsList objects'
  return $ mergeTypeCheckResultsPair (objMap', return classMap)
