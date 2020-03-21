--------------------------------------------------------------------
-- |
-- Module    :  Eval
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Eval where

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet          as S
import Data.Bifunctor (first)
import           Data.Either                    ( partitionEithers )
import           Syntax

import Eval.Common
import Eval.Runtime

leafFromMeta :: EvalMeta -> LeafType
leafFromMeta (Typed (SumType prodTypes)) = case S.toList prodTypes of
  [leafType] -> leafType
  _ -> error "Arrow has multiple leaves"

makeBaseEnv :: EObjectMap -> Env
makeBaseEnv objMap = (H.union primEnv resEnv, H.empty)
  where
    resEnv = H.fromListWith (++) $ concatMap resFromArrows $ H.toList objMap
    resFromArrows (obj, arrows) = map (resFromArrow obj) arrows
    resFromArrow (Object om _ _) arrow = (leafFromMeta om, [ResEArrow arrow])

evalCompAnnot :: Env -> ECompAnnot -> Either EvalError ()
evalCompAnnot env (CompAnnot "assert" args) =
  case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just (CExpr _ (CStr msg))) -> arrowEvalExpr env test boolType >>= (\(BoolVal b) -> if b then Right () else Left (AssertError msg))
    (Just test, Nothing) -> arrowEvalExpr env test boolType >>= (\(BoolVal b) -> if b then Right () else Left (AssertError "Failed assertion"))
    _ -> Left $ GenEvalError "Invalid assertion"
evalCompAnnot _ (CompAnnot name _) = Left $ GenEvalError $ "Unknown annotation: " ++ name

evalResArrowTree :: Env -> Val -> ResArrowTree -> Either EvalError Val
evalResArrowTree env (TupleVal name vals) (ResArrowTree resArrow afterArrowMap) = do
  newVal <- case resArrow of
    (ResEArrow (Arrow (Typed destType) annots resExpr)) -> do
      let env' = envWithVals env (H.fromList $ map (first (`LeafType` H.empty)) $ H.toList vals)
      mapM_ (evalCompAnnot env') annots
      case resExpr of
        Just resExpr' -> arrowEvalExpr env' resExpr' destType
        Nothing -> Left $ GenEvalError $ "Missing arrow expression for evaluating " ++ name
    (PrimArrow _ f) -> return $ f vals
  let newLeafType = getValType newVal
  case H.lookup newLeafType afterArrowMap of
    Just newResArrowTree -> evalResArrowTree env newVal newResArrowTree
    Nothing -> Left $ GenEvalError "error in evalResArrowTree"
evalResArrowTree _ val ResArrowID = return val
evalResArrowTree _ _ _ = Left $ GenEvalError "Failed to evaluate arrow"

evalExpr :: Env -> EExpr -> Either EvalError Val
evalExpr _ (CExpr _ (CInt i)) = Right $ IntVal i
evalExpr _ (CExpr _ (CFloat f)) = Right $ FloatVal f
evalExpr _ (CExpr _ (CStr s)) = Right $ StrVal s
evalExpr (_, valEnv) (Value (Typed (SumType prodTypes)) name) = case S.toList prodTypes of
    (_:_:_) -> Left $ GenEvalError $ "Found multiple types for value " ++ name
    [] -> Left $ GenEvalError $ "Found no types for value " ++ name ++ " with type " ++ show prodTypes
    [prodType] -> return $ case H.lookup prodType valEnv of
      Just val -> val
      Nothing -> TupleVal name H.empty
evalExpr env (TupleApply (Typed (SumType prodTypes)) (Typed baseType, baseExpr) argExprs) = case S.toList prodTypes of
    (_:_:_) -> Left $ GenEvalError $ "Found multiple types for tupleApply " ++ show baseExpr
    [] -> Left $ GenEvalError $ "Found no types for tupleApply " ++ show baseExpr ++ " with type " ++ show prodTypes ++ " and exprs " ++ show argExprs
    [LeafType _ leafType] | H.keysSet argExprs == H.keysSet leafType -> do
                           baseVal <- arrowEvalExpr env baseExpr baseType
                           argVals <- mapM (\(valDestType, expr) -> arrowEvalExpr env expr (SumType $ S.singleton valDestType)) $ H.intersectionWith (,) leafType argExprs
                           case baseVal of
                             TupleVal baseName baseArgs -> return $ TupleVal baseName (H.union argVals baseArgs)
                             _ -> Left $ GenEvalError "The base to apply was not a tuple"
    _ -> Left $ GenEvalError $ "Found bad types for tupleApply " ++ show baseExpr

arrowEvalExpr :: Env -> EExpr -> Type -> Either EvalError Val
arrowEvalExpr env expr destType = do
  val <- evalExpr env expr
  case envLookup env (getValType val) destType of
    Right resArrowTree -> evalResArrowTree env val resArrowTree
    Left err -> Left err

envWithVals :: Env -> H.HashMap LeafType Val -> Env
envWithVals (resEnv, _) vals = (resEnv, vals)

envLookupTry :: Env -> Type -> Either EvalError ResArrowTree -> ResArrow -> Either EvalError ResArrowTree
envLookupTry _ _ (Right resArrowTree) _ = return resArrowTree
envLookupTry env destType failure resArrow = do
  let (SumType newLeafTypes) = resArrowDestType resArrow
  let eitherAfterArrows = partitionEithers $ map (\leafType -> (leafType,) <$> envLookup env leafType destType) $ S.toList newLeafTypes
  case eitherAfterArrows of
    ([], afterArrows) -> do
      let afterArrowTree = H.fromList afterArrows
      return $ ResArrowTree resArrow afterArrowTree
    (_:_, _) -> failure

envLookup :: Env -> LeafType -> Type -> Either EvalError ResArrowTree
envLookup _ srcType (SumType destTypes) | S.member srcType destTypes = Right ResArrowID
envLookup env@(resEnv, _) srcType destType = case H.lookup srcType resEnv of
  Just resArrows -> do
    -- TODO: Sort resArrows by priority order before trying
    let failure = Left $ GenEvalError $ "Failed to lookup arrow for " ++ show (srcType, destType)
    foldl (envLookupTry env destType) failure resArrows
  Nothing -> Left $ GenEvalError $ "Failed to lookup arrow for " ++ show (srcType, destType)

evalPrgm :: EExpr -> Type -> EPrgm -> Either EvalError Val
evalPrgm src dest (objectMap, _) = arrowEvalExpr (makeBaseEnv objectMap) src dest

evalMain :: EPrgm -> Either EvalError Val
evalMain = evalPrgm main intType
  where main = Value (Typed $ SumType $ S.singleton $ LeafType "main" H.empty) "main"
