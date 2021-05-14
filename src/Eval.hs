--------------------------------------------------------------------
-- |
-- Module    :  Eval
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is used to run the interpreter (or compiler macro)
-- on a program. It can build, run, and compute the annotations
-- for the program.
--------------------------------------------------------------------
{-# LANGUAGE NamedFieldPuns #-}

module Eval where

import Prelude hiding (unzip)
import Data.Zip
import qualified Data.HashMap.Strict as H
import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           CRes

import TreeBuild
import Eval.Common
import Eval.Runtime
import Eval.Env
import           Text.Printf
import Control.Monad
import Emit (codegenExInit)
import Data.Graph
import Data.Maybe
import Utils

-- Checks if mainx is defined (not declared)
containsMainx :: String -> EPrgmGraphData -> Bool
containsMainx prgmName prgmGraphData = any objArrowsContains objMap
  where
    (objMap, _, _) = prgmFromGraphData prgmName prgmGraphData
    objArrowsContains (Object{objName}, arrows) = objName == "mainx" && any arrowDefined arrows
    arrowDefined (Arrow _ _ _ maybeExpr) = isJust maybeExpr

evalCompAnnot :: Env -> Val -> CRes Env
evalCompAnnot env (TupleVal "#assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
  (Just b, Just (StrVal _)) | b == true -> return env
  (Just b, Just (StrVal msg)) | b == false -> CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return env
  (Just b, Nothing) | b == false -> CErr [MkCNote $ AssertCErr "Failed assertion"]
  _ -> evalError env "Invalid assertion"
evalCompAnnot env (TupleVal name _) = evalError env $ printf "Unknown compiler annotation %s" name
evalCompAnnot env _ = evalError env "Eval: Invalid compiler annotation type"

eval :: Env -> ResArrowTree -> CRes (Val, Env)
eval env (ResEArrow input object arrow) = do
  (input', env2) <- evalPopVal <$> eval (evalPush env "resEArrow input") input
  let newArrArgs = buildArrArgs object input'
  (resArrowTree, compAnnots, oldArgs, env3) <- evalStartEArrow env2 (getValType input') object arrow newArrArgs
  env5s <- forM compAnnots $ \compAnnot -> do
            (compAnnot', env4) <- evalPopVal <$> eval (evalPush env3 $ printf "annot %s" (show compAnnot)) compAnnot
            evalCompAnnot env4 compAnnot'
  let env5 = case env5s of
        [] -> env3
        _ -> evalEnvJoinAll env5s
  (res, env6) <- evalPopVal <$> eval (evalPush env5 $ printf "ResEArrow %s" (show arrow)) resArrowTree
  return (res, evalEndEArrow env6 res oldArgs)
eval env (PrimArrow input _ (EPrim _ _ f)) = do
  (input', env2) <- evalPopVal <$> eval (evalPush env "PrimArrow input") input
  case input' of
    (TupleVal _ args) -> return (f args, env2)
    _ -> error "Unexpected eval PrimArrow input"
eval env MacroArrow{} = evalError env $ printf "Can't evaluate a macro - it should be removed during TreeBuild"
eval env ExprArrow{} = evalError env $ printf "Can't evaluate an expr - it should be removed during TreeBuild"
eval env (ConstantArrow v) = return (v, env)
eval env@Env{evArgs} (ArgArrow _ name) = case H.lookup name evArgs of
  Just arg' -> return (arg', env)
  Nothing -> evalError env $ printf "Unknown arg %s found during evaluation \n\t\t with arg env %s" name (show evArgs)
eval env@Env{evClassMap} (ResArrowMatch m _ opts) = do
  (m', env2) <- evalPopVal <$> eval (evalPush env "match input") m
  case H.toList $ H.filterWithKey (\optType _ -> hasPartial evClassMap (getValType m') (singletonType optType)) opts of
    [(_, resArrowTree)] -> evalPopVal <$> eval (evalPush env2 $ "match with val " ++ show m') resArrowTree
    [] -> evalError env2 $ printf "Failed match in eval resArrowTree: \n\tVal: %s \n\tOptions: %s" (show m') (show opts)
    (_:_:_) -> evalError env $ printf "Multiple matches in eval resArrowTree: \n\tVal: %s \n\tOptions: %s " (show m') (show opts)
eval env (ResArrowCond _ [] elseTree) = evalPopVal <$> eval (evalPush env "else") elseTree
eval env@Env{evArgs} (ResArrowCond resType (((ifCondTree, ifCondInput, ifObj), ifThenTree):restIfTrees) elseTree) = do
  (ifCondInput', env2) <- evalPopVal <$> eval (evalPush env "condInput") ifCondInput
  let env3 = evalSetArgs env2 $ buildArrArgs ifObj ifCondInput'
  (cond', env4) <- evalPopVal <$> eval (evalPush env3 "cond") ifCondTree
  let env5 = evalSetArgs env4 evArgs
  case cond' of
    b | b == true -> evalPopVal <$> eval (evalPush env5 $ "then for " ++ show ifCondTree) ifThenTree
    b | b == false -> evalPopVal <$> eval (evalPush env5 $ "else for " ++ show ifCondTree) (ResArrowCond resType restIfTrees elseTree)
    _ -> error "Non-Bool eval resArrowCond"
eval env (ResArrowTuple name args) | H.null args = return (TupleVal name H.empty, env)
eval env (ResArrowTuple name args) = do
  args' <- traverse (eval (evalPush env "tuple")) args
  let (args'', env2s) = unzip args'
  let env2 = evalEnvJoinAll $ fmap evalPop env2s
  return (TupleVal name args'', env2)
eval env (ResArrowTupleApply base argName argRATree) = do
  (base', env2) <- evalPopVal <$> eval (evalPush env "tupleApplyBase") base
  case base' of
    TupleVal name baseArgs -> do
      (argVal, env3) <- eval (evalPush env2 $ printf "tupleApplyArg applying %s" argName) argRATree
      let args' = H.insert argName argVal baseArgs
      return (TupleVal name args', evalPop env3)
    _ -> evalError env "Invalid input to tuple application"

evalBaseEnv :: EPrgm -> Env
evalBaseEnv prgm@(objMap, classMap, _) = Env {
        evObjMap = objMap,
        evClassMap = classMap,
        evArgs = H.empty,
        evExEnv = H.empty,
        evTbEnv = buildTBEnv primEnv prgm,
        evCallStack = [],
        evCoverage = H.empty,
        evTreebugOpen = [],
        evTreebugClosed = []
                }

prgmFromGraphData :: String -> EPrgmGraphData -> EPrgm
prgmFromGraphData prgmName (prgmGraph, nodeFromVertex, vertexFromKey) = mergePrgms $ map (fst3 . nodeFromVertex) $ reachable prgmGraph $ fromJust $ vertexFromKey prgmName

evalBuildPrgm :: EExpr -> PartialType -> Type -> EPrgm -> CRes (ResArrowTree, Env)
evalBuildPrgm input srcType destType prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  initTree <- buildRoot evTbEnv input srcType destType
  return (initTree, env)

evalAnnots :: String -> EPrgmGraphData -> CRes [(EExpr, Val)]
evalAnnots prgmName prgmGraphData = do
  let prgm@(_, _, annots) = prgmFromGraphData prgmName prgmGraphData
  let env@Env{evTbEnv} = evalBaseEnv prgm
  forM annots $ \annot -> do
    let exprType = getMetaType $ getExprMeta annot
    let inTree = ExprArrow annot exprType exprType
    let emptyType = PartialType (PTypeName "EmptyObj") H.empty H.empty H.empty PtArgExact
    let emptyObj = Object (Typed (singletonType emptyType) Nothing) FunctionObj "EmptyObj" H.empty H.empty
    tree <- resolveTree evTbEnv (emptyType, emptyObj) inTree
    val <- fst <$> eval env tree
    return (annot, val)

evalPrgm :: EExpr -> PartialType -> Type -> String -> EPrgmGraphData -> CRes (IO (Integer, EvalResult))
evalPrgm input src@PartialType{ptName=PTypeName{}} dest prgmName prgmGraphData = do
  let prgm = prgmFromGraphData prgmName prgmGraphData
  (initTree, env) <- evalBuildPrgm input src dest prgm
  let env2 = evalSetArgs env (H.singleton "io" (IOVal 0 $ pure ()))
  (res, env') <- eval env2 initTree
  case res of
    (IOVal r io) -> return (io >> pure (r, evalResult env'))
    _ -> CErr [MkCNote $ GenCErr Nothing "Eval did not return an instance of IO"]
evalPrgm _ PartialType{ptName=PClassName{}} _ _ _ = error "Can't eval class"

evalMainx :: String -> EPrgmGraphData -> CRes (IO (Integer, EvalResult))
evalMainx = evalPrgm mainExpr mainPartial ioType
  where mainPartial = PartialType (PTypeName "mainx") H.empty H.empty (H.singleton "io" ioType) PtArgExact
        mainPartialEmpty = Typed (singletonType (PartialType (PTypeName "mainx") H.empty H.empty H.empty PtArgExact)) Nothing
        mainExpr = TupleApply (Typed (singletonType mainPartial) Nothing) (mainPartialEmpty, Value mainPartialEmpty "mainx") "io" (Arg (Typed ioType Nothing) "io")

evalMain :: String -> EPrgmGraphData -> CRes (IO (Val, EvalResult))
evalMain prgmName prgmGraphData = do
  let srcName = "main"
  let src = PartialType (PTypeName srcName) H.empty H.empty H.empty PtArgExact
  let input = Value (Typed (singletonType src) Nothing) "main"
  let dest = singletonType (PartialType (PTypeName "CatlnResult") H.empty H.empty (H.fromList [("name", strType), ("contents", strType)]) PtArgExact)
  let prgm = prgmFromGraphData prgmName prgmGraphData
  (initTree, env) <- evalBuildPrgm input src dest prgm
  (res, env') <- eval env initTree
  case res of
    val@(TupleVal "CatlnResult" args) -> case (H.lookup "name" args, H.lookup "contents" args) of
      (Just (StrVal _), Just (StrVal _)) -> return $ return (val, evalResult env')
      _ -> CErr [MkCNote $ GenCErr Nothing "Eval main returned a CatlnResult with bad args"]
    (LLVMVal toCodegen) -> return $ do
      llvmStr <- codegenExInit toCodegen
      return (TupleVal "CatlnResult" (H.fromList [("name", StrVal "out.ll"), ("contents", StrVal llvmStr)]), evalResult env')
    _ -> CErr [MkCNote $ GenCErr Nothing "Eval main did not return a CatlnResult"]
