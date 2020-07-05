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

import qualified Data.HashMap.Strict as H

import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           TypeCheck.Common
import           TypeCheck.Show (showCon)

matchingConstraintHelper :: FEnv -> Pnt -> Pnt -> Pnt -> Bool
matchingConstraintHelper env p p2 p3 = equivalent env p p2 || equivalent env p p3

matchingConstraint :: FEnv -> Pnt -> Constraint -> Bool
matchingConstraint env p (EqualsKnown p2 _) = equivalent env p p2
matchingConstraint env p (EqPoints p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (BoundedByKnown p2 _) = equivalent env p p2
matchingConstraint env p (BoundedByObjs _ p2) = equivalent env p p2
matchingConstraint env p (ArrowTo p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (PropEq (p2, _) p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (AddArgs (p2, _) p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (PowersetTo p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (UnionOf p2 p3s) = equivalent env p p2 || any (equivalent env p) p3s

showMatchingConstraints :: FEnv -> Pnt -> [SConstraint]
showMatchingConstraints env@(FEnv _ cons _ _) matchVar = map (showCon env) $ filter (matchingConstraint env matchVar) cons

pointUb :: FEnv -> Pnt -> TypeCheckResult Type
pointUb env p = do
  scheme <- descriptor env p
  case scheme of
    (SType ub _ _) -> return ub
    (SVar _ p') -> pointUb env p'

toMeta :: FEnv -> VarMeta -> String -> TypeCheckResult Typed
toMeta env (VarMeta p (PreTyped pt)) _ = case pointUb env p of
  TypeCheckResult notes ub -> case pt of
    TypeVar{} -> return $ Typed pt
    _ -> TypeCheckResult notes $ Typed $ compactType ub
  TypeCheckResE notes -> do
    let matchingConstraints = showMatchingConstraints env p
    TypeCheckResE $ map (TCWithMatchingConstraints matchingConstraints) notes

toExpr :: FEnv -> VExpr -> TypeCheckResult TExpr
toExpr env (CExpr m c) = do
  m' <- toMeta env m $ "Constant " ++ show c
  return $ CExpr m' c
toExpr env (Value m name) = do
  m' <- toMeta env m $ "Value_" ++ name
  return $ Value m' name
toExpr env (Arg m name) = do
  m' <- toMeta env m $ "Arg_" ++ name
  return $ Arg m' name
toExpr env (TupleApply m (baseM, baseExpr) args) = do
  m' <- toMeta env m "TupleApply_M"
  baseM' <- toMeta env baseM "TupleApply_baseM"
  baseExpr' <- toExpr env baseExpr
  args' <- mapM (toExpr env) args
  case m' of -- check for errors
    tp@(Typed (SumType sumType)) | all (\(_, _, leafArgs) -> not (H.keysSet args' `isSubsetOf` H.keysSet leafArgs)) (splitPartialLeafs sumType) -> do
                                        let matchingConstraints = showMatchingConstraints env $ getPnt m
                                        TypeCheckResE [TCWithMatchingConstraints matchingConstraints $ TupleMismatch baseM' baseExpr' tp args']
    _ -> return $ TupleApply m' (baseM', baseExpr') args'

toCompAnnot :: FEnv -> VCompAnnot -> TypeCheckResult TCompAnnot
toCompAnnot env (CompAnnot name args) = do
  args' <- mapM (toExpr env) args
  return $ CompAnnot name args'

toGuard :: FEnv -> VGuard -> TypeCheckResult TGuard
toGuard env (IfGuard expr) = do
  expr' <- toExpr env expr
  return $ IfGuard expr'
toGuard _ ElseGuard = return ElseGuard
toGuard _ NoGuard = return NoGuard

toArrow :: FEnv -> VArrow -> TypeCheckResult TArrow
toArrow env (Arrow m annots aguard maybeExpr) = do
  m' <- toMeta env m "Arrow"
  annots' <- mapM (toCompAnnot env) annots
  aguard' <- toGuard env aguard
  case maybeExpr of
    Just expr -> do
      expr' <- toExpr env expr
      return $ Arrow m' annots' aguard' (Just expr')
    Nothing -> return $ Arrow m' annots' aguard' Nothing

toObjArg :: FEnv -> String -> (TypeName, VObjArg) -> TypeCheckResult (TypeName, TObjArg)
toObjArg env prefix (name, (m, maybeObj)) = do
  let prefix' = prefix ++ "_" ++ name
  m' <- toMeta env m prefix'
  case maybeObj of
    Just obj -> do
      obj' <- toObject env prefix' obj
      return (name, (m', Just obj'))
    Nothing -> return (name, (m', Nothing))

toObject :: FEnv -> String -> VObject -> TypeCheckResult TObject
toObject env prefix (Object m basis name vars args) = do
  let prefix' = prefix ++ "_" ++ name
  m' <- toMeta env m prefix'
  vars' <- mapM (\(varName, varVal) -> (varName,) <$> toMeta env varVal (prefix' ++ "." ++ varName)) $ H.toList vars
  args' <- mapM (toObjArg env prefix') $ H.toList args
  return $ Object m' basis name (H.fromList vars') (H.fromList args')

toObjectArrows :: FEnv -> (VObject, [VArrow]) -> TypeCheckResult (TObject, [TArrow])
toObjectArrows env (obj, arrows) = do
  obj' <- toObject env "Object" obj
  arrows' <- mapM (toArrow env) arrows
  return (obj', arrows')

toPrgm :: FEnv -> VPrgm -> TypeCheckResult TPrgm
toPrgm env (objMap, classMap) = do
  objMap' <- mapM (toObjectArrows env) objMap
  return (H.fromList objMap', classMap)
