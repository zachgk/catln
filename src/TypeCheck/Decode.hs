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
import           Data.Functor
import           Data.Tuple.Sequence
import qualified Data.HashMap.Strict as H
import           Data.UnionFind.ST

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           TypeCheck.Common
import           TypeCheck.Show (showCon)

fromPartialType :: PartialType -> Maybe PartialType
fromPartialType (name, vars, args) = case sequenceT (traverse fromType vars, traverse fromType args) of
  Just (vars', args') -> Just (name, vars', args')
  Nothing -> Nothing

fromType :: Type -> Maybe Type
fromType TopType = Nothing
fromType (SumType partials) = fmap (SumType . joinPartialLeafs) $ traverse fromPartialType $ splitPartialLeafs partials

matchingConstraintHelper :: Pnt s -> Pnt s -> Pnt s -> ST s Bool
matchingConstraintHelper p p2 p3 = do
  c2 <- equivalent p p2
  c3 <- equivalent p p3
  return $ c2 || c3

matchingConstraint :: Pnt s -> Constraint s -> ST s Bool
matchingConstraint p (EqualsKnown p2 _) = equivalent p p2
matchingConstraint p (EqPoints p2 p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (BoundedBy p2 p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (BoundedByKnown p2 _) = equivalent p p2
matchingConstraint p (BoundedByObjs _ p2) = equivalent p p2
matchingConstraint p (ArrowTo p2 p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (PropEq (p2, _) p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (AddArgs (p2, _) p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (PowersetTo p2 p3) = matchingConstraintHelper p p2 p3
matchingConstraint p (UnionOf p2 p3s) = do
  c2 <- equivalent p p2
  c3s <- mapM (equivalent p) p3s
  return $ c2 || or c3s

type DEnv s = [Constraint s]
showMatchingConstraints :: [Constraint s] -> Pnt s -> ST s [SConstraint]
showMatchingConstraints cons matchVar = do
  filterCons <- filterM (matchingConstraint matchVar) cons
  mapM showCon filterCons

toMeta :: DEnv s -> VarMeta s -> String -> ST s (TypeCheckResult Typed)
toMeta env p name = do
  scheme <- descriptor p
  case scheme of
    TypeCheckResE s -> return $ TypeCheckResE s
    TypeCheckResult notes (SType ub _ _) -> case fromType (compactType ub) of
      Nothing -> do
        showMatching <- showMatchingConstraints env p
        return $ TypeCheckResE (FailInfer name scheme showMatching:notes)
      Just t -> return $ TypeCheckResult notes (Typed t)

toExpr :: DEnv s -> VExpr s -> ST s (TypeCheckResult TExpr)
toExpr env (CExpr m c) = do
  res <- toMeta env m $ "Constant " ++ show c
  return $ res <&> (`CExpr` c)
toExpr env (Value m name) = do
  m' <- toMeta env m $ "Value_" ++ name
  return $ fmap (`Value` name) m'
toExpr env (Arg m name) = do
  m' <- toMeta env m $ "Arg_" ++ name
  return $ fmap (`Arg` name) m'
toExpr env (TupleApply m (baseM, baseExpr) args) = do
  m' <- toMeta env m "TupleApply_M"
  baseM' <- toMeta env baseM "TupleApply_baseM"
  baseExpr' <- toExpr env baseExpr
  args' <- mapM (toExpr env) args
  case m' of -- check for errors
    TypeCheckResult notes tp@(Typed (SumType sumType)) | all (\(_, _, leafArgs) -> not (H.keysSet args' `isSubsetOf` H.keysSet leafArgs)) (splitPartialLeafs sumType) -> do
                                        matchingConstraints <- showMatchingConstraints env m
                                        let sArgs = sequence args'
                                        return $ TypeCheckResE (TupleMismatch baseM' baseExpr' tp sArgs matchingConstraints:notes)
    _ -> return $ (\(m'', baseM'', baseExpr'', args'') -> TupleApply m'' (baseM'', baseExpr'') args'') <$> sequenceT (m', baseM', baseExpr', sequence args')

toCompAnnot :: DEnv s -> VCompAnnot s -> ST s (TypeCheckResult TCompAnnot)
toCompAnnot env (CompAnnot name args) = do
  args' <- mapM (toExpr env) args
  return $ fmap (CompAnnot name) (sequence args')

toGuard :: DEnv s -> VGuard s -> ST s (TypeCheckResult TGuard)
toGuard env (IfGuard expr) = do
  expr' <- toExpr env expr
  return $ IfGuard <$> expr'
toGuard _ ElseGuard = return $ return ElseGuard
toGuard _ NoGuard = return $ return NoGuard

toArrow :: DEnv s -> VArrow s -> ST s (TypeCheckResult TArrow)
toArrow env (Arrow m annots aguard maybeExpr) = do
  m' <- toMeta env m "Arrow"
  annotsT <- mapM (toCompAnnot env) annots
  aguard' <- toGuard env aguard
  let annots' = sequence annotsT
  case maybeExpr of
    Just expr -> do
      expr' <- toExpr env expr
      return $ (\(m'', annots'', aguard'', expr'') -> Arrow m'' annots'' aguard'' (Just expr'')) <$> sequenceT (m', annots', aguard', expr')
    Nothing -> return $ (\(annots'', m'', aguard'') -> Arrow m'' annots'' aguard'' Nothing) <$> sequenceT (annots', m', aguard')

toObjArg :: DEnv s -> String -> (TypeName, VObjArg s) -> ST s (TypeCheckResult (TypeName, TObjArg))
toObjArg env prefix (name, (m, maybeObj)) = do
  let prefix' = prefix ++ "_" ++ name
  m' <- toMeta env m prefix'
  case maybeObj of
    Just obj -> do
      obj' <- toObject env prefix' obj
      return $ (name,) <$> sequenceT (m', sequence $ Just obj')
    Nothing -> return $ (name,) . (,Nothing) <$> m'

toObject :: DEnv s -> String -> VObject s -> ST s (TypeCheckResult TObject)
toObject env prefix (Object m basis name vars args) = do
  let prefix' = prefix ++ "_" ++ name
  m' <- toMeta env m prefix'
  vars' <- mapM (\(varName, varVal) -> (varName,) <$> toMeta env varVal (prefix' ++ "." ++ varName)) $ H.toList vars
  args' <- mapM (toObjArg env prefix') $ H.toList args
  return $ (\(m'', vars'', args'') -> Object m'' basis name vars'' args'') <$> sequenceT  (m', sequence $ H.fromList vars', H.fromList <$> sequence args')

toObjectArrows :: DEnv s -> (VObject s, [VArrow s]) -> ST s (TypeCheckResult (TObject, [TArrow]))
toObjectArrows env (obj, arrows) = do
  obj' <- toObject env "Object" obj
  arrows' <- mapM (toArrow env) arrows
  let arrows'' = sequence arrows'
  return $ sequenceT (obj', arrows'')

toPrgm :: VPrgm s -> [Constraint s] -> ST s (TypeCheckResult TPrgm)
toPrgm (objMap, classMap) cons = do
  let env = cons
  objects' <- mapM (toObjectArrows env) objMap
  let objMap' = H.fromList <$> sequence objects'
  return $ sequenceT (objMap', TypeCheckResult [] classMap)
