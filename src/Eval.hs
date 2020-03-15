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
import           Data.List
import           Data.Either                    ( partitionEithers )
import           Syntax

import           Control.Monad

import Eval.Common
import Eval.Runtime
import           Debug.Trace
import Text.Pretty.Simple
import qualified Data.Text.Lazy as T

leafFromMeta :: EvalMeta -> LeafType
leafFromMeta (Typed (SumType prodTypes)) = case S.toList prodTypes of
  [leafType] -> leafType
  _ -> error "Arrow has multiple leaves"

makeBaseEnv :: EObjectMap -> Env
-- makeBaseEnv objMap | trace (T.unpack $ pShow objMap) False = undefined
makeBaseEnv objMap = (H.union primEnv resEnv, H.empty)
  where
    resEnv = H.fromListWith (++) $ concatMap resFromArrows $ H.toList objMap
    resFromArrows (obj, arrows) = map (resFromArrow obj) arrows
    resFromArrow (Object om _ _) arrow = (leafFromMeta om, [ResEArrow arrow])

evalCompAnnot :: Env -> ECompAnnot -> Either EvalError ()
evalCompAnnot env (CompAnnot "assert" args) =
  case (H.lookup "test" args, H.lookup "msg" args) of
    (Just test, Just (CExpr _ (CStr msg))) -> evalExpr env test boolType >>= (\(BoolVal b) -> if b then Right () else Left (AssertError msg))
    (Just test, Nothing) -> evalExpr env test boolType >>= (\(BoolVal b) -> if b then Right () else Left (AssertError "Failed assertion"))
    _ -> Left $ GenEvalError "Invalid assertion"
evalCompAnnot _ (CompAnnot name _) = Left $ GenEvalError $ "Unknown annotation: " ++ name

evalResArrowTree :: Env -> Val -> ResArrowTree -> Either EvalError Val
evalResArrowTree env (TupleVal name vals) (ResArrowTree resArrow afterArrowMap) | trace ("evalResArrowTree " ++ name) False = undefined
evalResArrowTree env (TupleVal name vals) (ResArrowTree resArrow afterArrowMap) = do
  newVal <- case resArrow of
    (ResEArrow (Arrow (Typed destType) annots resExpr)) | trace (" - found ResEArrow with expr " ++ show resExpr) False -> undefined
    (ResEArrow (Arrow (Typed destType) annots resExpr)) -> do
      let env' = envWithVals env (H.fromList $ map (first (`LeafType` H.empty)) $ H.toList vals)
      env'' <- mapM (evalCompAnnot env') annots
      case resExpr of
        Just resExpr' -> evalExpr env' resExpr' destType
        Nothing -> Left $ GenEvalError $ "Missing arrow expression for evaluating " ++ name
    (PrimArrow _ f) -> return $ f vals
  let newLeafType = getValType newVal
  case H.lookup newLeafType afterArrowMap of
    -- Just newResArrowTree | trace (" - with val " ++ show newVal ++ " --- and type " ++ show newLeafType) False -> undefined
    Just newResArrowTree -> evalResArrowTree env newVal newResArrowTree
    Nothing -> Left $ GenEvalError "error in evalResArrowTree"
-- evalResArrowTree env (TupleVal name _) _ | trace ("evalResArrowTree with other for " ++ name) False = undefined
evalResArrowTree env _ (ResArrowVal val) = return val
evalResArrowTree env val ResArrowID = return val
evalResArrowTree _ _ _ = Left $ GenEvalError "Failed to evaluate arrow"

evalExpr :: Env -> EExpr -> Type -> Either EvalError Val
evalExpr _ (CExpr _ (CInt i)) intType = Right $ IntVal i
evalExpr _ (CExpr _ (CFloat f)) floatType = Right $ FloatVal f
evalExpr _ (CExpr _ (CStr s)) strType = Right $ StrVal s
-- evalExpr env (Tuple typed@(Typed (SumType prodTypes)) name exprs) destType | trace ("evalExpr " ++ name ++ " " ++ show prodTypes) False = undefined
evalExpr env (Tuple typed@(Typed (SumType prodTypes)) name exprs) destType = case S.toList prodTypes of
    (_:_:_) -> Left $ GenEvalError $ "Found multiple types for " ++ name
    [] -> Left $ GenEvalError $ "Found no types for " ++ name ++ " with type " ++ show prodTypes ++ " and exprs " ++ show exprs
    [prodType@(LeafType name leafType)] | H.keysSet exprs == H.keysSet leafType -> do
                           vals <- mapM (\(valDestType, expr) -> evalExpr env (Tuple typed name exprs) (SumType $ S.singleton valDestType)) $ H.intersectionWith (,) leafType exprs
                           case envLookup env prodType destType of
                             -- Right x | trace ("vals " ++ show vals) False -> undefined
                             Right resArrowTree -> evalResArrowTree env (TupleVal name vals) resArrowTree
                             Left err -> Left err
    _ -> Left $ GenEvalError $ "Found bad types for " ++ name

envWithVals :: Env -> H.HashMap LeafType Val -> Env
envWithVals (resEnv, _) vals | trace ("envWithVals " ++ show vals) False = undefined
envWithVals (resEnv, _) vals = (resEnv, vals)

envLookupTry :: Env -> Type -> Either EvalError ResArrowTree -> ResArrow -> Either EvalError ResArrowTree
envLookupTry _ _ (Right resArrowTree) _ = return resArrowTree
-- envLookupTry env destType failure resArrow | trace ("envLookupTry " ++ show destType) False = undefined
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
envLookup env@(resEnv, valEnv) srcType destType = case H.lookup srcType valEnv of
  Just val -> Right (ResArrowVal val)
  Nothing -> case H.lookup srcType resEnv of
    -- Just resArrows | trace ("envLookup " ++ show srcType) False -> undefined
    Just resArrows -> do
      -- TODO: Sort resArrows by priority order before trying
      let failure = Left $ GenEvalError $ "Failed to lookup arrow for " ++ show (srcType, destType)
      foldl (envLookupTry env destType) failure resArrows
    Nothing -> Left $ GenEvalError $ "Failed to lookup arrow for " ++ show (srcType, destType)

evalPrgm :: EExpr -> Type -> EPrgm -> Either EvalError Val
evalPrgm src dest (objectMap, _) = evalExpr (makeBaseEnv objectMap) src dest

evalMain :: EPrgm -> Either EvalError Val
evalMain = evalPrgm main intType
  where main = Tuple (Typed $ SumType $ S.singleton $ LeafType "main" H.empty) "main" H.empty
