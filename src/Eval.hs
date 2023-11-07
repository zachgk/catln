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
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Eval where

import           CRes
import qualified Data.HashMap.Strict as H
import           Semantics
import           Semantics.Prgm
import           Semantics.Types

import           Control.Monad
import           Data.Graph
import           Data.Maybe
-- import           Emit                (codegenExInit)
import           Eval.Common
import           Eval.Env
import           Eval.ExprBuilder
import           Eval.Runtime
import           Text.Printf
import           TreeBuild
import           Utils

-- | 'EvalMode' contains how to evaluate the function and the 'PTypeName' to eval
data EvalMode
  = EvalRunWithContext String -- ^ Run f{IO io} -> IO
  | EvalRun String -- ^ Run f -> Show
  | EvalBuildWithContext String -- ^ Build f{IO io} -> CatlnResult
  | EvalBuild String -- ^ Build f -> CatlnResult
  | NoEval -- ^ Can't run or build
  deriving (Eq, Show)

evalRunnable :: EvalMode -> Bool
evalRunnable EvalRunWithContext{} = True
evalRunnable EvalRun{}            = True
evalRunnable _                    = False

evalBuildable :: EvalMode -> Bool
evalBuildable EvalRunWithContext{}   = True
evalBuildable EvalBuildWithContext{} = True
evalBuildable EvalBuild{}            = True
evalBuildable _                      = False

-- | Checks if a function is defined (not declared) and finds the 'EvalMode' of the function.
-- It will look through all of the objects to see if they match the function name or are Context(value=function name).
-- Then, those found functions can be checked for run/build based on whether the return type is a CatlnResult.
-- It also will pass the function name back through EvalMode in order to convert 'PRelativeName' into the matching 'PTypeName'
-- TODO The use of listToMaybe will secretly discard if multiple evalTargetModes or function names are found. Instead, an error should be thrown
evalTargetMode :: String -> String -> EPrgmGraphData -> EvalMode
evalTargetMode function prgmName prgmGraphData = fromMaybe NoEval $ listToMaybe $ mapMaybe objArrowsContains objMap
  where
    (objMap, classGraph, _) = prgmFromGraphData prgmName prgmGraphData
    objArrowsContains ObjArr{oaArr=(Nothing, _)} = Nothing
    objArrowsContains oa@ObjArr{oaArr=(_, oaM)} = case oaObjPath oa of
      "/Context" -> case H.lookup "value" $ exprAppliedArgsMap $ oaObjExpr oa of
        Just (_, Just valObjExpr) -> if relativeNameMatches function (exprPath valObjExpr)
          then Just $ if isBuildable oa (getMetaType oaM)
            then EvalBuildWithContext (exprPath valObjExpr)
            else EvalRunWithContext (exprPath valObjExpr)

          else Nothing
        _ -> Nothing
      _ | relativeNameMatches function (oaObjPath oa) -> if isBuildable oa (getMetaType oaM)
          then Just $ EvalBuild (oaObjPath oa)
          else Just $ EvalRun (oaObjPath oa)
      _ -> Nothing
    isBuildable oa tp = not $ isBottomType $ snd $ intersectTypesWithVarEnv classGraph (fmap (intersectAllTypes classGraph . fmap getMetaType) $ exprVarArgs $ oaObjExpr oa) tp resultType

-- | evaluate annotations such as assertions that require compiler verification
evalCompAnnot :: Env -> Val -> CRes Env
evalCompAnnot env (TupleVal "/Catln/#assert" args) = case (H.lookup "test" args, H.lookup "msg" args) of
  (Just b, Just (StrVal _)) | b == true -> return env
  (Just b, Just (StrVal msg)) | b == false -> CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return env
  (Just b, Nothing) | b == false -> CErr [MkCNote $ AssertCErr "Failed assertion"]
  _ -> evalError env $ printf "Invalid assertion with unexpected args %s" (show args)
evalCompAnnot env TupleVal{} = return env
evalCompAnnot env _ = evalError env "Eval: Invalid compiler annotation type"

evalCallTree :: Env -> Val -> TCallTree -> CRes (Val, Env)
evalCallTree env v TCTId = return (v, env)
evalCallTree env1@Env{evClassGraph} m (TCMatch opts) = do
  case H.toList $ H.filterWithKey (\optType _ -> isSubtypePartialOf evClassGraph (getValType m) (singletonType optType)) opts of
    [(_, resArrowTree)] -> evalPopVal <$> evalCallTree (evalPush env1 $ "match with val " ++ show m) m resArrowTree
    [] -> evalError env1 $ printf "Failed match in eval callTree: \n\tVal: %s \n\tVal type: %s\n\tOptions: %s" (show m) (show $ getValType m) (show opts)
    (_:_:_) -> evalError env1 $ printf "Multiple matches in eval callTree: \n\tVal: %s \n\tOptions: %s " (show m) (show opts)
evalCallTree env v (TCSeq a b) = do
  (v', env') <- evalCallTree env v a
  evalCallTree env' v' b
evalCallTree env m (TCCond _ [] elseTree) = evalCallTree env m elseTree
evalCallTree env1@Env{evArgs} m (TCCond resType (((ifCondTree, ifObj), ifThenTree):restIfTrees) elseTree) = do
  let env2 = evalSetArgs env1 $ buildArrArgs ifObj m
  (cond', env3) <- evalPopVal <$> evalExpr (evalPush env2 "cond") ifCondTree
  let env4 = evalSetArgs env3 evArgs
  case cond' of
    b | b == true -> evalPopVal <$> evalCallTree (evalPush env4 $ "then for " ++ show ifCondTree) m ifThenTree
    b | b == false -> evalPopVal <$> evalCallTree (evalPush env4 $ "else for " ++ show ifCondTree) m (TCCond resType restIfTrees elseTree)
    _ -> error "Non-Bool eval resArrowCond"
evalCallTree env@Env{evArgs} _ (TCArg _ name) = case H.lookup name evArgs of
  Just arg' -> return (arg', env)
  Nothing -> evalError env $ printf "Unknown arg %s found during evaluation \n\t\t with arg env %s" name (show evArgs)
evalCallTree env1 input (TCObjArr oa) = do
  let newArrArgs = buildArrArgs oa input
  (resArrowTree, compAnnots, oldArgs, env2) <- evalStartEArrow env1 (getValType input) oa newArrArgs
  env4s <- forM compAnnots $ \compAnnot -> do
            (compAnnot', env3) <- evalPopVal <$> evalExpr (evalPush env2 $ printf "annot %s" (show compAnnot)) compAnnot
            evalCompAnnot env3 compAnnot'
  let env4 = case env4s of
        [] -> env2
        _  -> evalEnvJoinAll env4s
  (res, env5) <- evalPopVal <$> evalExpr (evalPush env4 $ printf "ResEArrow %s" (show oa)) resArrowTree
  return (res, evalEndEArrow env5 res oldArgs)
evalCallTree env1 input (TCPrim _ (EPrim _ _ f)) = do
  case input of
    (TupleVal _ args) -> return (f args, env1)
    _                 -> error "Unexpected eval PrimArrow input"
evalCallTree env _ TCMacro{} = evalError env $ printf "Can't evaluate a macro - it should be removed during TreeBuild"

evalExpr :: Env -> TExpr () -> CRes (Val, Env)
evalExpr env (TCExpr _ v) = return (v, env)
evalExpr env (TValue m _) = do
  let UnionType leafs = getMetaType m
  let [PartialType{ptName}] = splitUnionType leafs
  return (TupleVal (fromPartialName ptName) H.empty, env)
evalExpr _ (THoleExpr m h) = CErr [MkCNote $ GenCErr (getMetaPos m) $ printf "Can't evaluate hole %s" (show h)]
evalExpr env (TAliasExpr b _) = evalExpr env b
evalExpr env@Env{evArgs} (TTupleApply _ b oa@ObjArr{oaObj=Just (GuardExpr (TValue _ "io") Nothing), oaArr=(Nothing, _)}) = do
  (TupleVal n args, env') <- evalExpr env b
  case H.lookup "io" evArgs of
    Just io -> return (TupleVal n (H.insert (oaObjPath oa) io args), env')
    Nothing -> error $ printf "evalExpr with no io"
evalExpr env (TTupleApply _ b oa) = do
  (TupleVal n args, env') <- evalExpr env b
  case oaArr oa of
    (Just (GuardExpr oaExpr _), _) -> do
      (v, env'') <- evalExpr env' oaExpr
      return (TupleVal n (H.insert (oaObjPath oa) v args), env'')
    (Nothing, _) -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
evalExpr env (TVarApply _ b _ _) = evalExpr env b
evalExpr env (TCalls _ b callTree) = do
  (b', env') <- evalExpr env b
  evalCallTree env' b' callTree

evalBaseEnv :: EPrgm -> Env
evalBaseEnv prgm@(objMap, classGraph, _) = Env {
        evObjMap = objMap,
        evClassGraph = classGraph,
        evArgs = H.empty,
        evExEnv = H.empty,
        evTbEnv = buildTBEnv primEnv prgm,
        evCallStack = [],
        evCoverage = H.empty,
        evTreebugOpen = [],
        evTreebugClosed = []
                }

prgmFromGraphData :: String -> EPrgmGraphData -> EPrgm
prgmFromGraphData prgmName (prgmGraph, nodeFromVertex, vertexFromKey) = mergeExprPrgms $ map (fst3 . nodeFromVertex) $ reachable prgmGraph $ fromJust $ vertexFromKey prgmName

evalBuildPrgm :: EExpr -> PartialType -> Type -> EPrgm -> CRes (TExpr (), Env)
evalBuildPrgm input srcType destType prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  initTree <- buildRoot evTbEnv input srcType destType
  return (initTree, env)

evalAnnots :: String -> EPrgmGraphData -> CRes [(EExpr, Val)]
evalAnnots prgmName prgmGraphData = do
  let prgm@(_, _, annots) = prgmFromGraphData prgmName prgmGraphData
  let env@Env{evTbEnv} = evalBaseEnv prgm
  forM annots $ \annot -> do
    let emptyType = partialVal (PTypeName "EmptyObj")
    let emptyObj = ObjArr (Just (GuardExpr (Value (Meta (singletonType emptyType) Nothing emptyMetaDat) "EmptyObj") Nothing)) FunctionObj Nothing [] (Nothing, emptyMetaN)
    tree <- toTExpr evTbEnv (emptyType, emptyObj) annot
    val <- fst <$> evalExpr env tree
    return (annot, val)

evalRun :: String -> String -> EPrgmGraphData -> CRes (IO (Integer, EvalResult))
evalRun function prgmName prgmGraphData = do
  let prgm = prgmFromGraphData prgmName prgmGraphData
  input <-  case evalTargetMode function prgmName prgmGraphData of
        EvalRunWithContext function' ->
          -- Case for eval Context(value=main, io=IO)

          return $ eApplyM (eApply (eVal "/Context") "value" (eVal function')) "io" ioM
        EvalRun function' ->
          -- Case for eval main
          return $ eVal function'
        _ -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to run" (show function)]
  let src = getExprPartialType input
  let dest = ioType
  (expr, env) <- evalBuildPrgm input src dest prgm
  let env2 = evalSetArgs env (H.singleton "io" (IOVal 0 $ pure ()))
  (res, env') <- evalExpr env2 expr
  case res of
    (IOVal r io) -> return (io >> pure (r, evalResult env'))
    _ -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval did not return an instance of IO \n\tInstead returned %s \n\t With expr %s" (show res) (show expr)]

evalBuild :: String -> String -> EPrgmGraphData -> CRes (IO (Val, EvalResult))
evalBuild function prgmName prgmGraphData = do
  let prgm = prgmFromGraphData prgmName prgmGraphData

  input <-  case evalTargetMode function prgmName prgmGraphData of
        EvalRunWithContext function' ->
          -- Case for eval llvm(c=Context(value=main, io=IO))
          return $ eApply (eVal "/Catln/llvm") "c" (eVal function')
        EvalBuildWithContext function' ->
          -- Case for buildable Context(value=main, io=IO)
          return $ eApplyM (eApply (eVal "/Context") "value" (eVal function')) "io" ioM
        EvalBuild function' ->
          -- Case for buildable main
          return $ eVal function'
        unexpectedEvalMode -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to build with unexpected eval mode %s" (show function) (show unexpectedEvalMode)]
  let src = getExprPartialType input
  let dest = resultType
  (expr, env) <- evalBuildPrgm input src dest prgm
  (res, env') <- evalExpr env expr
  case res of
    val@(TupleVal "/Catln/CatlnResult" args) -> case (H.lookup "name" args, H.lookup "contents" args) of
      (Just (StrVal _), Just (StrVal _)) -> return $ return (val, evalResult env')
      _ -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval %s returned a /Catln/CatlnResult with bad args" function]
    (LLVMVal _) -> return $ do
      -- llvmStr <- codegenExInit toCodegen
      let llvmStr = "LLVM Placeholder result"
      return (TupleVal "/Catln/CatlnResult" (H.fromList [("name", StrVal "out.ll"), ("contents", StrVal llvmStr)]), evalResult env')
    val -> CErr [MkCNote $ GenCErr Nothing $ printf "Eval %s did not return a /Catln/CatlnResult. Instead it returned %s" function (show val)]
