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
import           Syntax.Types
import           Syntax.Prgm
import           Syntax

import TreeBuild
import Eval.Common
import Eval.Runtime

buildArrArgs :: EObject -> Val -> Args
buildArrArgs = aux H.empty
  where
    aux acc (Object _ _ objName objArgs) val | H.null objArgs = H.insert objName val acc
    aux acc (Object _ _ _ objArgs) (TupleVal _ tupleArgs) = H.foldrWithKey addArgs acc $ H.intersectionWith (,) objArgs tupleArgs
    aux _ _ val = error $ "Invalid buildArrArgs value: " ++ show val
    addArgs argName ((_, Nothing), argVal) acc = H.insert argName argVal acc
    addArgs _ ((_, Just subObj), argVal) acc = aux acc subObj argVal

envLookupResArrowTree :: Env -> EArrow -> Maybe (ResArrowTree EPrim, [ResArrowTree EPrim])
envLookupResArrowTree env arrow = H.lookup arrow env

evalCompAnnot :: EStacktrace -> Val -> CRes ()
evalCompAnnot st (TupleVal "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
  (Just (BoolVal test), Just (StrVal msg)) -> if test then return () else CErr [AssertCErr msg]
  (Just (BoolVal test), Nothing) -> if test then return () else CErr [AssertCErr "Failed assertion"]
  _ -> CErr [EvalCErr st "Invalid assertion"]
evalCompAnnot st (TupleVal name _) = CErr [EvalCErr st $ "Unknown compiler annotation " ++ name]
evalCompAnnot st _ = CErr [EvalCErr st "Eval: Invalid compiler annotation type"]

eval :: Env -> EStacktrace -> Args  -> Val -> ResArrow EPrim -> CRes Val
eval env st _ val (ResEArrow object arrow) = case envLookupResArrowTree env arrow of
  Just (resArrowTree, compAnnots) -> do
    let newArrArgs = buildArrArgs object val
    mapM_ (\compAnnot -> do
              compAnnot' <- evalTree env (("annot " ++ show compAnnot):st) newArrArgs NoVal compAnnot
              return $ evalCompAnnot st compAnnot'
          ) compAnnots
    evalTree env (("ResEArrow " ++ show arrow):st) newArrArgs val resArrowTree
  Nothing -> CErr [EvalCErr st $ "Failed to find arrow in eval resArrow: " ++ show arrow]
eval _ _ _ (TupleVal _ args) (PrimArrow _ f) = return $ f args
eval _ _ _ _ (ConstantArrow (CInt i)) = return $ IntVal i
eval _ _ _ _ (ConstantArrow (CFloat f)) = return $ FloatVal f
eval _ _ _ _ (ConstantArrow (CStr s)) = return $ StrVal s
eval _ st args _ (ArgArrow _ name) = case H.lookup name args of
  Just arg -> return arg
  Nothing -> CErr [EvalCErr st $ "Could not find arg " ++ name ++ " with args " ++ show (H.toList args)]
eval _ st _ _ _ = error $ "Bad eval resArrow at: " ++ show st

evalTree :: Env -> EStacktrace -> Args -> Val -> ResArrowTree EPrim -> CRes Val
evalTree env st arrArgs val (ResArrowCompose t1 t2) = do
  val' <- evalTree env ("Compose first":st) arrArgs val t1
  evalTree env ("Compose second":st) arrArgs val' t2
evalTree env st _ val (ResArrowMatch opts) = case H.lookup (getValType val) opts of
  Just resArrowTree -> case val of
    (TupleVal _ newArrArgs) ->
      evalTree env (("match with " ++ show val):st) newArrArgs val resArrowTree
    _ ->
      CErr [EvalCErr st "Called match with a non tuple. I don't know if this is valid"]
  Nothing -> CErr [EvalCErr st $ "Failed match in eval resArrowTree: \n\t" ++ show val ++ "\n\t" ++ show opts]
evalTree env st arrArgs val (ResArrowCond [] elseTree) = evalTree env ("else":st) arrArgs val elseTree
evalTree env st arrArgs val (ResArrowCond ((ifCondTree, ifThenTree):restIfTrees) elseTree) = do
  cond' <- evalTree env ("cond":st) arrArgs val ifCondTree
  case cond' of
    (BoolVal True) -> evalTree env (("then for " ++ show ifCondTree):st) arrArgs val ifThenTree
    (BoolVal False) -> evalTree env st arrArgs val (ResArrowCond restIfTrees elseTree)
    _ -> error "Non-Bool evalTree resArrowCond"
evalTree env st arrArgs val (ResArrowTuple name args) = do
  args' <- traverse (evalTree env ("tuple":st) arrArgs val) args
  return $ TupleVal name args'
evalTree env st arrArgs val (ResArrowTupleApply base args) = do
  base' <- evalTree env ("tupleApplyBase":st) arrArgs val base
  case base' of
    TupleVal name baseArgs -> do
      argVals <- mapM (evalTree env ("tupleApplyArg":st) arrArgs val) args
      let args' = H.union argVals baseArgs
      return $ TupleVal name args'
    _ -> CErr [EvalCErr st "Invalid input to tuple application"]
evalTree env st arrArgs val (ResArrowSingle r) = eval env (("ResArrowSingle: " ++ show r):st) arrArgs val r
evalTree _ _ _ val ResArrowID = return val

evalBuildPrgm :: LeafType -> Type -> EPrgm -> CRes (ResArrowTree EPrim, ResExEnv EPrim)
evalBuildPrgm = buildPrgm primEnv

evalBuildMain :: EPrgm -> CRes (ResArrowTree EPrim, ResExEnv EPrim)
evalBuildMain = evalBuildPrgm main intType
  where main = LeafType "main" H.empty

evalPrgm :: LeafType -> Type -> EPrgm -> CRes Val
evalPrgm src@(LeafType srcName _) dest prgm = do
  (resArrowTree, exEnv) <- evalBuildPrgm src dest prgm
  evalTree exEnv [] H.empty (TupleVal srcName H.empty) resArrowTree

evalMain :: EPrgm -> CRes Val
evalMain = evalPrgm main intType
  where main = LeafType "main" H.empty
