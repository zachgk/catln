--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.Decode
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module decodes after type checking is completed to produce
-- the final typechecked program.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module TypeCheck.Decode where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S

import           Syntax
import           Syntax.Prgm
import           Syntax.Types
import           TypeCheck.Common
import           TypeCheck.Show      (showCon)

matchingConstraintHelper :: FEnv -> VarMeta -> VarMeta -> VarMeta -> Bool
matchingConstraintHelper env p p2 p3 = equivalent env p p2 || equivalent env p p3

matchingConstraint :: FEnv -> VarMeta -> Constraint -> Bool
matchingConstraint env p (EqualsKnown p2 _) = equivalent env p p2
matchingConstraint env p (EqPoints p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (BoundedByKnown p2 _) = equivalent env p p2
matchingConstraint env p (BoundedByObjs p2) = equivalent env p p2
matchingConstraint env p (ArrowTo p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (PropEq (p2, _) p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (VarEq (p2, _) p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (AddArg (p2, _) p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (AddInferArg p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (PowersetTo p2 p3) = matchingConstraintHelper env p p2 p3
matchingConstraint env p (UnionOf p2 p3s) = equivalent env p p2 || any (equivalent env p) p3s

showMatchingConstraints :: FEnv -> VarMeta -> [SConstraint]
showMatchingConstraints env@FEnv{feCons} matchVar = map (showCon env) $ filter (matchingConstraint env matchVar) feCons

toMeta :: FEnv -> VarMeta -> String -> TypeCheckResult Typed
toMeta env@FEnv{feClassMap} m@(VarMeta _ (PreTyped pt pos) _) _ = case pointUb env m of
  TypeCheckResult notes ub -> case pt of
    TypeVar{} -> return $ Typed pt pos
    _ -> TypeCheckResult notes $ Typed (intersectTypes feClassMap ub pt) pos
  TypeCheckResE notes -> do
    TypeCheckResult notes (Typed bottomType pos)

member :: String -> [String ] -> Bool  
member x arr = case suffixLookup x arr of
  Just _ -> True 
  Nothing -> False 

toExpr :: FEnv -> VExpr -> TypeCheckResult TExpr
toExpr env (ICExpr m c) = do
  m' <- toMeta env m $ "Constant " ++ show c
  return $ CExpr m' c
toExpr env (IValue m name) = do
  m' <- toMeta env m $ "Value_" ++ name
  return $ Value m' name
toExpr env (IArg m name) = do
  m' <- toMeta env m $ "Arg_" ++ name
  return $ Arg m' name
toExpr env (ITupleApply m (baseM, baseExpr) (Just argName) argExpr) = do
  m' <- toMeta env m "TupleApply_M"
  baseM' <- toMeta env baseM "TupleApply_baseM"
  baseExpr' <- toExpr env baseExpr
  argExpr' <- toExpr env argExpr
  let result = TupleApply m' (baseM', baseExpr') argName argExpr'
  case m' of -- check for errors

    -- Don't check if a bottom type is present
    _ | getMetaType m' == bottomType -> return result
    _ | getMetaType baseM' == bottomType -> return result

    tp@(Typed (UnionType uType) _) | all (\PartialType{ptArgs=leafArgs} -> not (argName `member` H.keys leafArgs)) (splitUnionType uType) ->
                                        TypeCheckResult [TupleMismatch baseM' baseExpr' tp $ H.singleton argName argExpr'] result

    _ -> return result
toExpr env (ITupleApply m (baseM, baseExpr) Nothing argExpr) = do
  let pos = getMetaPos m
  m' <- toMeta env m "TupleApplyInfer_M"
  baseM' <- toMeta env baseM "TupleApplyInfer_baseM"
  baseExpr' <- toExpr env baseExpr
  argExpr' <- toExpr env argExpr
  argName <- case (getMetaType baseM', getMetaType m') of
    (UnionType basePartialLeafs, UnionType partialLeafs) -> case (splitUnionType basePartialLeafs, splitUnionType partialLeafs) of
      ([PartialType{ptArgs=basePartialArgs}], [PartialType{ptArgs}]) -> case S.toList $ S.difference (H.keysSet ptArgs) (H.keysSet basePartialArgs) of
        [argN] -> return argN
        _ -> TypeCheckResE [GenTypeCheckError pos "Failed argument inference due to multiple arg options"]
      _ -> TypeCheckResE [GenTypeCheckError pos "Failed argument inference due to multiple types"]
    _ -> TypeCheckResE [GenTypeCheckError pos "Failed argument inference due to non UnionType"]
  return $ TupleApply m' (baseM', baseExpr') argName argExpr'

toGuard :: FEnv -> VGuard -> TypeCheckResult TGuard
toGuard env (IfGuard expr) = do
  expr' <- toExpr env expr
  return $ IfGuard expr'
toGuard _ ElseGuard = return ElseGuard
toGuard _ NoGuard = return NoGuard

toArrow :: FEnv -> VArrow -> TypeCheckResult TArrow
toArrow env (Arrow m annots aguard maybeExpr) = do
  m' <- toMeta env m "Arrow"
  annots' <- mapM (toExpr env) annots
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
toObject env prefix obj@Object{objM, objVars, objArgs, objPath} = do
  let prefix' = prefix ++ "_" ++ objPath
  m' <- toMeta env objM prefix'
  vars' <- mapM (\(varName, varVal) -> (varName,) <$> toMeta env varVal (prefix' ++ "." ++ varName)) $ H.toList objVars
  args' <- mapM (toObjArg env prefix') $ H.toList objArgs
  return $ obj{objM=m', objVars=H.fromList vars', objArgs=H.fromList args'}

toObjectArrows :: FEnv -> (VObject, [VArrow]) -> TypeCheckResult (TObject, [TArrow])
toObjectArrows env (obj, arrows) = do
  obj' <- toObject env "Object" obj
  arrows' <- mapM (toArrow env) arrows
  return (obj', arrows')

toPrgm :: FEnv -> VPrgm -> TypeCheckResult TPrgm
toPrgm env (objMap, classMap, annots) = do
  objMap' <- mapM (toObjectArrows env) objMap
  annots' <- mapM (toExpr env) annots
  return (objMap', classMap, annots')

toPrgms :: FEnv -> [VPrgm] -> TypeCheckResult [TPrgm]
toPrgms env = mapM (toPrgm env)
