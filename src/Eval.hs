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
import           Syntax

import TreeBuild
import Eval.Common
import Eval.Runtime

envLookupResArrowTree :: Env -> EArrow -> Maybe (ResArrowTree EPrim, [ResArrowTree EPrim])
envLookupResArrowTree env arrow = H.lookup arrow env

evalCompAnnot :: Val -> CRes ()
evalCompAnnot (TupleVal "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
  (Just (BoolVal test), Just (StrVal msg)) -> if test then return () else CErr [AssertCErr msg]
  (Just (BoolVal test), Nothing) -> if test then return () else CErr [AssertCErr "Failed assertion"]
  _ -> CErr [EvalCErr "Invalid assertion"]
evalCompAnnot (TupleVal name _) = CErr [EvalCErr $ "Unknown compiler annotation " ++ name]
evalCompAnnot _ = CErr [EvalCErr "Eval: Invalid compiler annotation type"]

eval :: Env -> Val -> ResArrow EPrim -> CRes Val
eval env val (ResEArrow arrow) = case envLookupResArrowTree env arrow of
  Just (resArrowTree, compAnnots) -> do
    mapM_ (\compAnnot -> do
              compAnnot' <- evalTree env NoVal compAnnot
              return $ evalCompAnnot compAnnot'
          ) compAnnots
    evalTree env val resArrowTree
  Nothing -> CErr [EvalCErr "Failed to find arrow in eval resArrow"]
eval _ (TupleVal _ args) (PrimArrow _ f) = return $ f args
eval _ _ (ConstantArrow (CInt i)) = return $ IntVal i
eval _ _ (ConstantArrow (CFloat f)) = return $ FloatVal f
eval _ _ (ConstantArrow (CStr s)) = return $ StrVal s
eval _ _ _ = error "Bad eval resArrow"

evalTree :: Env -> Val -> ResArrowTree EPrim -> CRes Val
evalTree env val (ResArrowCompose t1 t2) = do
  val' <- evalTree env val t1
  evalTree env val' t2
evalTree env val (ResArrowMatch opts) = case H.lookup (getValType val) opts of
  Just resArrowTree -> evalTree env val resArrowTree
  Nothing -> CErr [EvalCErr $ "Failed match in eval resArrowTree: \n\t" ++ show val ++ "\n\t" ++ show opts]
evalTree env val (ResArrowTuple name args) = do
  args' <- traverse (evalTree env val) args
  return $ TupleVal name args'
evalTree env val (ResArrowTupleApply base args) = do
  base' <- evalTree env val base
  case base' of
    TupleVal name baseArgs -> do
      argVals <- mapM (evalTree env val) args
      let args' = H.union argVals baseArgs
      return $ TupleVal name args'
    _ -> CErr [EvalCErr "Invalid input to tuple application"]
evalTree env val (ResArrowSingle r) = eval env val r
evalTree _ val ResArrowID = return val

evalBuildPrgm :: LeafType -> Type -> EPrgm -> CRes (ResArrowTree EPrim, ResExEnv EPrim)
evalBuildPrgm = buildPrgm primEnv

evalBuildMain :: EPrgm -> CRes (ResArrowTree EPrim, ResExEnv EPrim)
evalBuildMain = evalBuildPrgm main intType
  where main = LeafType "main" H.empty

evalPrgm :: LeafType -> Type -> EPrgm -> CRes Val
evalPrgm src dest prgm = do
  (resArrowTree, exEnv) <- evalBuildPrgm src dest prgm
  evalTree exEnv NoVal resArrowTree

evalMain :: EPrgm -> CRes Val
evalMain = evalPrgm main intType
  where main = LeafType "main" H.empty
