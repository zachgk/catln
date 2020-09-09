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
import           Data.Bifunctor
import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           CRes

import TreeBuild
import Eval.Common
import Eval.Runtime
import           Text.Printf

buildArrArgs :: EObject -> Val -> Args
buildArrArgs = aux H.empty
  where
    aux acc (Object _ _ objName _ objArgs) val | H.null objArgs = H.insert objName val acc
    aux _ (Object _ _ objName _ _) (TupleVal tupleName _) | objName /= tupleName = error $ printf "Found name mismatch in buildArrArgs: object %s and tuple %s" objName tupleName
    aux acc (Object _ _ _ _ objArgs) (TupleVal _ tupleArgs) = H.foldrWithKey addArgs acc $ H.intersectionWith (,) objArgs tupleArgs
    aux _ _ val = error $ "Invalid buildArrArgs value: " ++ show val
    addArgs argName ((_, Nothing), argVal) acc = H.insert argName argVal acc
    addArgs _ ((_, Just subObj), argVal) acc = aux acc subObj argVal

replaceTreeArgs :: Args -> ResArrowTree EPrim -> ResArrowTree EPrim
replaceTreeArgs args (ResArrowCompose a b) = ResArrowCompose (replaceTreeArgs args a) (replaceTreeArgs args b)
replaceTreeArgs _ (ResArrowMatch m) = ResArrowMatch m -- Match does not propagate reachesTreeArgs to subtree as the match introduces it's own args later
replaceTreeArgs args (ResArrowCond ifThens els) = ResArrowCond (map (bimap (replaceTreeArgs args) (replaceTreeArgs args)) ifThens) (replaceTreeArgs args els)
replaceTreeArgs args (ResArrowTuple n vs) = ResArrowTuple n (fmap (replaceTreeArgs args) vs)
replaceTreeArgs args (ResArrowTupleApply b an av) = ResArrowTupleApply (replaceTreeArgs args b) an (replaceTreeArgs args av)
replaceTreeArgs args a@(ResArrowSingle (ArgArrow tp name)) = case H.lookup name args of
  Just arg -> ResArrowSingle $ PrimArrow tp $ EPrim (getValType arg) NoGuard (const arg)
  Nothing -> a
replaceTreeArgs _ (ResArrowSingle a) = ResArrowSingle a
replaceTreeArgs _ ResArrowID = ResArrowID

envLookupResArrowTree :: Env -> EArrow -> Maybe (ResArrowTree EPrim, [ResArrowTree EPrim])
envLookupResArrowTree (resExEnv, _) arrow = H.lookup arrow resExEnv

evalCompAnnot :: EStacktrace -> Val -> CRes ()
evalCompAnnot st (TupleVal "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
  (Just b, Just (StrVal _)) | b == true -> return ()
  (Just b, Just (StrVal msg)) | b == false -> CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return ()
  (Just b, Nothing) | b == false -> CErr [MkCNote $ AssertCErr "Failed assertion"]
  _ -> CErr [MkCNote $ EvalCErr st "Invalid assertion"]
evalCompAnnot st (TupleVal name _) = CErr [MkCNote $ EvalCErr st $ "Unknown compiler annotation " ++ name]
evalCompAnnot st _ = CErr [MkCNote $ EvalCErr st "Eval: Invalid compiler annotation type"]

eval :: Env -> EStacktrace -> Val -> ResArrow EPrim -> CRes Val
eval env st val (ResEArrow object arrow) = case envLookupResArrowTree env arrow of
  Just (resArrowTree, compAnnots) -> do
    let newArrArgs = buildArrArgs object val
    mapM_ (\compAnnot -> do
              let treeWithoutArgs = replaceTreeArgs newArrArgs compAnnot
              compAnnot' <- evalTree env (("annot " ++ show compAnnot):st) val treeWithoutArgs
              return $ evalCompAnnot st compAnnot'
          ) compAnnots
    let treeWithoutArgs = replaceTreeArgs newArrArgs resArrowTree
    evalTree env (("ResEArrow " ++ show arrow):st) val treeWithoutArgs
  Nothing -> CErr [MkCNote $ EvalCErr st $ "Failed to find arrow in eval resArrow: " ++ show arrow]
eval _ _ (TupleVal _ args) (PrimArrow _ (EPrim _ _ f)) = return $ f args
eval _ _ _ (ConstantArrow (CInt i)) = return $ IntVal i
eval _ _ _ (ConstantArrow (CFloat f)) = return $ FloatVal f
eval _ _ _ (ConstantArrow (CStr s)) = return $ StrVal s
eval _ st _ (ArgArrow _ name) = CErr [MkCNote $ EvalCErr st $ printf "Unexpected arg %s not removed during evaluation" name]
eval _ st val arr = error $ printf "Bad eval resArrow\n\t\t Arrow: %s\n\t\t Val: %s\n\t\t State: %s" (show arr) (show val) (show st)

evalTree :: Env -> EStacktrace -> Val -> ResArrowTree EPrim -> CRes Val
evalTree env st val (ResArrowCompose t1 t2) = do
  val' <- evalTree env (printf "Compose first with val %s" (show val):st) val t1
  evalTree env (("Compose second with " ++ show val'):st) val' t2
evalTree env@(_, classMap) st val (ResArrowMatch opts) = case H.toList $ H.filterWithKey (\optType _ -> hasPartial classMap (getValType val) (singletonType optType)) opts of
  [(_, resArrowTree)] -> case val of
    (TupleVal _ arrArgs) ->
      evalTree env (printf "match with tuple %s (%s) for options %s" (show val) (show $ getValType val) (show $ H.keys opts):st) val $ replaceTreeArgs arrArgs resArrowTree
    _ -> evalTree env (("match with val " ++ show val):st) val resArrowTree
  [] -> CErr [MkCNote $ EvalCErr st $ "Failed match in eval resArrowTree: \n\tVal: " ++ show val ++ "\n\tOptions: " ++ show opts]
  (_:_:_) -> CErr [MkCNote $ EvalCErr st $ "Multiple matches in eval resArrowTree: \n\tVal: " ++ show val ++ "\n\tOptions: " ++ show opts]
evalTree env st val (ResArrowCond [] elseTree) = evalTree env ("else":st) val elseTree
evalTree env st val (ResArrowCond ((ifCondTree, ifThenTree):restIfTrees) elseTree) = do
  cond' <- evalTree env ("cond":st) val ifCondTree
  case cond' of
    b | b == true -> evalTree env (("then for " ++ show ifCondTree):st) val ifThenTree
    b | b == false -> evalTree env st val (ResArrowCond restIfTrees elseTree)
    _ -> error "Non-Bool evalTree resArrowCond"
evalTree env st val (ResArrowTuple name args) = do
  args' <- traverse (evalTree env ("tuple":st) val) args
  return $ TupleVal name args'
evalTree env st val (ResArrowTupleApply base argName argRATree) = do
  base' <- evalTree env ("tupleApplyBase":st) val base
  case base' of
    TupleVal name baseArgs -> do
      argVal <- evalTree env (printf "tupleApplyArg applying %s" argName:st) val argRATree
      let args' = H.insert argName argVal baseArgs
      return $ TupleVal name args'
    _ -> CErr [MkCNote $ EvalCErr st "Invalid input to tuple application"]
evalTree env st val (ResArrowSingle r) = eval env (("ResArrowSingle: " ++ show r):st) val r
evalTree _ _ val ResArrowID = return val

evalBuildPrgm :: PartialType -> Type -> EPrgm -> CRes (ResArrowTree EPrim, ResExEnv EPrim)
evalBuildPrgm = buildPrgm primEnv

mainPartial :: PartialType
mainPartial = (PTypeName "main", H.empty, H.empty, H.singleton "io" ioType)

evalBuildMain :: EPrgm -> CRes (ResArrowTree EPrim, ResExEnv EPrim)
evalBuildMain = evalBuildPrgm mainPartial ioType

evalPrgm :: PartialType -> Type -> EPrgm -> CRes (IO Integer)
evalPrgm src@(PTypeName srcName, _, _, _) dest prgm@(_, classMap) = do
  (resArrowTree, exEnv) <- evalBuildPrgm src dest prgm
  let env = (exEnv, classMap)
  res <- evalTree env [] (TupleVal srcName (H.singleton "io" (IOVal 0 $ pure ()))) resArrowTree
  case res of
    (IOVal r io) -> return (io >> pure r)
    _ -> CErr [MkCNote $ GenCErr "Eval did not return an instance of IO"]
evalPrgm (PClassName _, _, _, _) _ _ = error "Can't eval class"

evalMain :: EPrgm -> CRes (IO Integer)
evalMain = evalPrgm mainPartial ioType
