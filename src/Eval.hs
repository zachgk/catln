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
{-# LANGUAGE NamedFieldPuns #-}

module Eval where

import Prelude hiding (unzip)
import Data.Zip
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
import Control.Monad

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

evalCompAnnot :: Env -> Val -> CRes Env
evalCompAnnot env (TupleVal "assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
  (Just b, Just (StrVal _)) | b == true -> return env
  (Just b, Just (StrVal msg)) | b == false -> CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return env
  (Just b, Nothing) | b == false -> CErr [MkCNote $ AssertCErr "Failed assertion"]
  _ -> evalError env "Invalid assertion"
evalCompAnnot env (TupleVal name _) = evalError env $ printf "Unknown compiler annotation %s" name
evalCompAnnot env _ = evalError env "Eval: Invalid compiler annotation type"

eval :: Env -> Val -> ResArrow EPrim -> CRes (Val, Env)
eval env val (ResEArrow object arrow) = do
  (resArrowTree, compAnnots, env2) <- evalStartEArrow env object arrow
  let newArrArgs = buildArrArgs object val
  env4s <- forM compAnnots $ \compAnnot -> do
            let treeWithoutArgs = replaceTreeArgs newArrArgs compAnnot
            (compAnnot', env3) <- evalPopVal <$> evalTree (evalPush env2 $ printf "annot %s" (show compAnnot)) val treeWithoutArgs
            evalCompAnnot env3 compAnnot'
  let env4 = case env4s of
        [] -> env2
        _ -> evalEnvJoinAll env4s
  let treeWithoutArgs = replaceTreeArgs newArrArgs resArrowTree
  (res, env5) <- evalPopVal <$> evalTree (evalPush env4 $ printf "ResEArrow %s" (show arrow)) val treeWithoutArgs
  return (res, evalEndEArrow env5 res)
eval env (TupleVal _ args) (PrimArrow _ (EPrim _ _ f)) = return (f args, env)
eval env _ (ConstantArrow (CInt i)) = return (IntVal i, env)
eval env _ (ConstantArrow (CFloat f)) = return (FloatVal f, env)
eval env _ (ConstantArrow (CStr s)) = return (StrVal s, env)
eval env _ (ArgArrow _ name) = evalError env $ printf "Unexpected arg %s not removed during evaluation" name
eval Env{evCallStack} val arr = error $ printf "Bad eval resArrow\n\t\t Arrow: %s\n\t\t Val: %s\n\t\t State: %s" (show arr) (show val) (show evCallStack)

evalTree :: Env -> Val -> ResArrowTree EPrim -> CRes (Val, Env)
evalTree env val (ResArrowCompose t1 t2) = do
  (val', env2) <- evalPopVal <$> evalTree (evalPush env $ printf "Compose first with val %s" (show val)) val t1
  evalPopVal <$> evalTree (evalPush env2 $ "Compose second with " ++ show val') val' t2
evalTree env@Env{evClassMap} val (ResArrowMatch opts) = case H.toList $ H.filterWithKey (\optType _ -> hasPartial evClassMap (getValType val) (singletonType optType)) opts of
  [(_, resArrowTree)] -> case val of
    (TupleVal _ arrArgs) ->
      fmap evalPopVal <$> evalTree (evalPush env $ printf "match with tuple %s (%s) for options %s" (show val) (show $ getValType val) (show $ H.keys opts)) val $ replaceTreeArgs arrArgs resArrowTree
    _ -> evalPopVal <$> evalTree (evalPush env $ "match with val " ++ show val) val resArrowTree
  [] -> evalError env $ printf "Failed match in eval resArrowTree: \n\tVal: %s \n\tOptions: %s" (show val) (show opts)
  (_:_:_) -> evalError env $ printf "Multiple matches in eval resArrowTree: \n\tVal: %s \n\tOptions: %s " (show val) (show opts)
evalTree env val (ResArrowCond [] elseTree) = evalPopVal <$> evalTree (evalPush env "else") val elseTree
evalTree env val (ResArrowCond ((ifCondTree, ifThenTree):restIfTrees) elseTree) = do
  (cond', env2) <- evalPopVal <$> evalTree (evalPush env "cond") val ifCondTree
  case cond' of
    b | b == true -> evalPopVal <$> evalTree (evalPush env2 $ "then for " ++ show ifCondTree) val ifThenTree
    b | b == false -> evalPopVal <$> evalTree (evalPush env $ "else for " ++ show ifCondTree) val (ResArrowCond restIfTrees elseTree)
    _ -> error "Non-Bool evalTree resArrowCond"
evalTree env _ (ResArrowTuple name args) | H.null args = return (TupleVal name H.empty, env)
evalTree env val (ResArrowTuple name args) = do
  args' <- traverse (evalTree (evalPush env "tuple") val) args
  let (args'', env2s) = unzip args'
  let env2 = evalEnvJoinAll $ fmap evalPop env2s
  return (TupleVal name args'', env2)
evalTree env val (ResArrowTupleApply base argName argRATree) = do
  (base', env2) <- evalPopVal <$> evalTree (evalPush env "tupleApplyBase") val base
  case base' of
    TupleVal name baseArgs -> do
      (argVal, env3) <- evalTree (evalPush env2 $ printf "tupleApplyArg applying %s" argName) val argRATree
      let args' = H.insert argName argVal baseArgs
      return (TupleVal name args', evalPop env3)
    _ -> evalError env "Invalid input to tuple application"
evalTree env val (ResArrowSingle r) = do
  (res, env2) <- eval (evalPush env $ "ResArrowSingle: " ++ show r) val r
  return (res, evalPop env2)
evalTree env val ResArrowID = return (val, env)

evalBuildPrgm :: PartialType -> Type -> EPrgm -> CRes (ResArrowTree EPrim, Env)
evalBuildPrgm srcType destType prgm@(objMap, classMap) = do
  (initTree, tbEnv) <- buildRoot primEnv srcType destType prgm
  let env = Env {
        evObjMap = objMap,
        evClassMap = classMap,
        evExEnv = H.empty,
        evTbEnv = tbEnv,
        evCallStack = [],
        evCoverage = H.empty,
        evTreebugOpen = [],
        evTreebugClosed = []
                }
  return (initTree, env)

mainPartial :: PartialType
mainPartial = (PTypeName "main", H.empty, H.empty, H.singleton "io" ioType)

evalPrgm :: PartialType -> Type -> EPrgm -> CRes (IO (Integer, EvalResult))
evalPrgm src@(PTypeName srcName, _, _, _) dest prgm = do
  (initTree, env) <- evalBuildPrgm src dest prgm
  (res, env') <- evalTree env (TupleVal srcName (H.singleton "io" (IOVal 0 $ pure ()))) initTree
  case res of
    (IOVal r io) -> return (io >> pure (r, evalResult env'))
    _ -> CErr [MkCNote $ GenCErr "Eval did not return an instance of IO"]
evalPrgm (PClassName _, _, _, _) _ _ = error "Can't eval class"

evalMain :: EPrgm -> CRes (IO (Integer, EvalResult))
evalMain = evalPrgm mainPartial ioType

evalMainb :: EPrgm -> CRes (String, String, EvalResult)
evalMainb prgm = do
  let srcName = "mainb"
  let src = (PTypeName srcName, H.empty, H.empty, H.empty)
  let dest = singletonType (PTypeName "CatlnResult", H.empty, H.empty, H.fromList [("name", strType), ("contents", strType)])
  (initTree, env) <- evalBuildPrgm src dest prgm
  (res, env') <- evalTree env NoVal initTree
  case res of
    (TupleVal "CatlnResult" args) -> case (H.lookup "name" args, H.lookup "contents" args) of
      (Just (StrVal n), Just (StrVal c)) -> return (n, c, evalResult env')
      _ -> CErr [MkCNote $ GenCErr "Eval mainb returned a CatlnResult with bad args"]
    _ -> CErr [MkCNote $ GenCErr "Eval mainb did not return a CatlnResult"]
