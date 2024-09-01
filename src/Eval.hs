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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Eval where

import           CRes
import qualified Data.HashMap.Strict as H
import           Semantics.Prgm
import           Semantics.Types

import           Control.Monad
import           Data.Graph
import           Data.Maybe
-- import           Emit                (codegenExInit)
import           Control.Monad.State
import           CtConstants
import qualified Data.HashSet        as S
import           Data.UUID           (UUID)
import           Eval.Common
import           Eval.Env
import           Eval.ExprBuilder
import           Eval.Runtime
import           Semantics
import           Semantics.TypeGraph (ReachesEnv (ReachesEnv),
                                      reachesHasCutSubtypeOf, reachesPartials,
                                      reachesTo)
import           Text.Printf
import           TreeBuild
import           Utils

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
evalPrgmTargetMode :: String -> EPrgm -> EvalMode
evalPrgmTargetMode function prgm = case (funCtxReaches, funReaches) of
  (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuildWithContext function'
  (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRunWithContext function'
  (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuild function'
  (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRun function' -- Should require isShowable for run result
  _ -> NoEval funCtxReaches funReaches
  where
    typeEnv = mkTypeEnv prgm
    function' = case maybeGetSingleton $ expandRelPartial typeEnv H.empty (partialVal function) of
      Just f -> ptName f
      Nothing -> error $ printf "Expected one typeName in evalTargetMode for %s. Instead found %s" (show function) (show $ expandRelPartial typeEnv H.empty (partialVal function))
    reachEnv = ReachesEnv typeEnv H.empty S.empty
    funCtxTp = (partialVal ContextStr){ptArgs=H.fromList [(partialKey contextValStr, typeVal function')], ptArgMode=PtArgAny}
    funTp = partialVal function'
    funCtxReaches = reachesPartials reachEnv [funCtxTp]
    funReaches = reachesPartials reachEnv [funTp]

evalTargetMode :: String -> FileImport -> EPrgmGraphData -> CRes EvalMode
evalTargetMode function prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  return $ evalPrgmTargetMode function prgm

-- | Gets the target modes for all objects in the file
evalAllTargetModes :: EPrgm -> H.HashMap UUID EvalMode
evalAllTargetModes prgm@(objMap, _, _) = H.fromList $ mapMaybe targetMode objMap
  where
    targetMode oa = case oaObjPath oa of
      ContextStr -> case H.lookupDefault Nothing (partialKey contextValStr) $ exprAppliedArgsMap $ oaObjExpr oa of
        Just (_, Just valE) -> case maybeExprPath valE of
          Just valName -> Just (getMetaID $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode valName prgm)
          _ -> Nothing
        _ -> Nothing
      _ -> Just (getMetaID $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (oaObjPath oa) prgm)

-- | evaluate annotations such as assertions that require compiler verification
evalCompAnnot :: Val -> StateT Env CRes ()

evalCompAnnot (TupleVal n args) | n == assertStr = case (H.lookup assertTestStr args, H.lookup assertMsgStr args) of
  (Just b, Just (StrVal _)) | b == true -> return ()
  (Just b, Just (StrVal msg)) | b == false -> lift $ CErr [MkCNote $ AssertCErr msg]
  (Just b, Nothing) | b == true -> return ()
  (Just b, Nothing) | b == false -> lift $ CErr [MkCNote $ AssertCErr "Failed assertion"]
  _ -> evalError $ printf "Invalid assertion with unexpected args %s" (show args)
evalCompAnnot TupleVal{} = return ()
evalCompAnnot _ = evalError "Eval: Invalid compiler annotation type"

evalGuards :: Env -> Val -> EObjArr -> CRes (Bool, Env)
evalGuards = undefined
  -- case cond' of
  --   b | b == true -> evalPopVal <$> evalCallTree (evalPush env4 $ "then for " ++ show ifObj) m ifThenTree
  --   b | b == false -> evalPopVal <$> evalCallTree (evalPush env4 $ "else for " ++ show ifObj) m (TCCond resType restIfTrees elseTree)
  --   _ -> error "Non-Bool eval resArrowCond"

evalObjArr :: Val -> AnyObjArr -> StateT Env CRes Val
evalObjArr input oa = do
  let newArrArgs = buildArrArgs oa input
  (resArrowTree, compAnnots, oldArgs) <- evalStartEArrow (getValType input) oa newArrArgs
  forM_ compAnnots $ \compAnnot -> do
            compAnnot' <- withEvalPush (printf "annot %s" (show compAnnot)) $ evalExpr compAnnot
            evalCompAnnot compAnnot'
  res <- withEvalPush (printf "ResEArrow %s" (show oa)) $ evalExpr resArrowTree
  modify $ evalEndEArrow input res oldArgs
  return res

evalCallTree :: Val -> TCallTree -> StateT Env CRes Val
-- evalCallTree v ct | trace (printf "evalCallTree %s using %s" (show v) (show ct)) False = undefined
evalCallTree v TCTId = return v
evalCallTree  m (TCMatch opts) = do
  Env{evTypeEnv} <- get
  case H.toList $ H.filterWithKey (\optType _ -> isSubtypePartialOf evTypeEnv (getValType m) (singletonType optType)) opts of
    [(_, resArrowTree)] -> do
      withEvalPush ("match with val " ++ show m) $ evalCallTree m resArrowTree
    [] -> evalError $ printf "Failed match in eval callTree: \n\tVal: %s \n\tVal type: %s\n\tOptions: %s" (show m) (show $ getValType m) (show opts)
    (_:_:_) -> evalError $ printf "Multiple matches in eval callTree: \n\tVal: %s \n\tOptions: %s " (show m) (show opts)
evalCallTree v (TCSeq a b) = do
  v' <- evalCallTree v a
  evalCallTree v' b
evalCallTree _ (TCCond _ []) = evalError "Found no matching conditions in evalTree"
evalCallTree m (TCCond resType ((cond, ifThenTree):restIfTrees)) = do
  Env{evTypeEnv} <- get
  cond' <- case cond of
    -- TODO Remove the use of clearUnionTypePreds below. It is a workaround as type predicates are not supported by subtypeOf
    Just (_, oa) | not (isSubtypePartialOf evTypeEnv (getValType m) (clearUnionTypePreds $ getExprType $ oaObjExpr oa)) -> return False
    Nothing -> return True
    Just ([], _) -> return True
    Just (ifCondTrees, ifObj) -> do
      Env{evArgs} <- get
      modify $ evalSetArgs $ buildArrArgs (Left ifObj) m
      conds' <- withEvalPush "cond" $ forM ifCondTrees $ \ifCondTree ->
        evalExpr ifCondTree
      modify $ evalSetArgs evArgs
      return $ all (true ==) conds'
  if cond'
    then withEvalPush ("then for " ++ show cond) $ evalCallTree m ifThenTree
    else withEvalPush ("else for " ++ show cond) $ evalCallTree m (TCCond resType restIfTrees)
evalCallTree input (TCArg _ name) = do
  Env{evArgs} <- get
  case H.lookup name evArgs of
    Just (ObjArrVal oa) -> evalObjArr input (Right oa)
    Just arg' -> return arg'
    Nothing -> evalError $ printf "Unknown arg %s found during evaluation \n\t\t with arg env %s" name (show evArgs)
evalCallTree input (TCObjArr oa) = evalObjArr input (Left oa)
evalCallTree input (TCPrim _ (EPrim _ f)) = do
  case input of
    (TupleVal _ args) -> case f args of
      Right val -> return val
      Left err  -> evalError err
    _                 -> error "Unexpected eval PrimArrow input"
evalCallTree _ TCMacro{} = evalError $ printf "Can't evaluate a macro - it should be removed during TreeBuild"

evalExpr :: TExpr EvalMetaDat -> StateT Env CRes Val
-- evalExpr e | trace (printf "eval %s" (show e)) False = undefined
evalExpr (TCExpr _ v) = return v
evalExpr (TValue m _) = do
  let PartialType{ptName} = getSingleton $ getMetaType m
  return $ TupleVal ptName H.empty
evalExpr (THoleExpr m h) = lift $ CErr [MkCNote $ GenCErr (getMetaPos m) $ printf "Can't evaluate hole %s" (show h)]
evalExpr (TAliasExpr b _) = evalExpr b
evalExpr (TWhere _ b _) = evalExpr b
evalExpr (TTupleApply _ (_, b) (EAppArg oa@ObjArr{oaObj=Just (TValue _ "/io"), oaArr=Just (Nothing, _)})) = do
  Env{evArgs} <- get
  TupleVal n args <- evalExpr b
  case H.lookup "/io" evArgs of
    Just io -> return $ TupleVal n (H.insert (oaObjPath oa) io args)
    Nothing -> error $ printf "evalExpr with no io"
evalExpr (TTupleApply _ (_, b) arg) = do
  b'@(TupleVal n args) <- evalExpr b
  case arg of
    EAppArg oa -> do
      v <- case oaArr oa of
        Just (Just oaExpr, _) -> case oaObj oa of
          Just TValue{} -> evalExpr oaExpr
          Just TTupleApply{} -> return $ ObjArrVal oa
          _ -> error $ printf "Unsupported eval argument of %s" (show oa)
        Just (Nothing, _) -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
        Nothing -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
      return $ TupleVal n (H.insert (oaObjPath oa) v args)
    EAppVar{} -> return b'
    EAppSpread a -> error $ printf "Not yet implemented evalExpr %s" (show a)
evalExpr (TCalls _ b callTree) = do
  b' <- evalExpr b
  evalCallTree b' callTree

evalBaseEnv :: EPrgm -> Env
evalBaseEnv prgm@(objMap, _, _) = Env {
        evObjMap = objMap,
        evTypeEnv = mkTypeEnv prgm,
        evArgs = H.empty,
        evExEnv = H.empty,
        evTbEnv = buildTBEnv primEnv prgm,
        evCallStack = [],
        evCoverage = H.empty,
        evTreebugOpen = [],
        evTreebugClosed = []
                }

prgmFromGraphData :: FileImport -> EPrgmGraphData -> CRes EPrgm
prgmFromGraphData prgmName (prgmGraph, nodeFromVertex, vertexFromKey) = case vertexFromKey prgmName of
  Just v -> return $ mergePrgms $ map (fst3 . nodeFromVertex) $ reachable prgmGraph v
  Nothing -> fail $ printf "Could not find prgm %s" (show prgmName)

-- | Tries to TreeBuild all ObjArrs, returning all built successfully
evalBuildAll :: EPrgmGraphData -> CRes (GraphData (Prgm TExpr EvalMetaDat) FileImport)
evalBuildAll prgmGraphData = do
  let prgms = graphToNodes prgmGraphData
  prgms' <- forM prgms $ \((objMap, cg, annots), prgmName, deps) -> do
    evalPrgm <- prgmFromGraphData prgmName prgmGraphData
    let Env{evTbEnv} = evalBaseEnv evalPrgm
    objMap' <- catCRes $ fmap (buildRootOA evTbEnv) objMap
    annots' <- catCRes $ fmap (toTExpr evTbEnv []) annots
    return ((objMap', cg, annots'), prgmName, deps)
  return $ graphFromEdges prgms'

evalBuildPrgm :: EExpr -> PartialType -> Type -> EPrgm -> CRes (TExpr EvalMetaDat, Env)
evalBuildPrgm input srcType destType prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  initTree <- buildRoot evTbEnv input srcType destType
  return (initTree, env)

evalAnnots :: FileImport -> EPrgmGraphData -> CRes [(EExpr, Val)]
evalAnnots prgmName prgmGraphData = do
  prgm@(objMap, _, globalAnnots) <- prgmFromGraphData prgmName prgmGraphData
  let env = evalBaseEnv prgm
  globalAnnots' <- buildAnnots env [] globalAnnots
  objMapAnnots' <- forM objMap $ \oa -> do
    -- TODO Make this build annots within arguments
    buildAnnots env [(getExprPartialType $ oaObjExpr oa, oa)] (oaAnnots oa)
  return $ concat (globalAnnots' : objMapAnnots')
  where
    showAnnot a = eVal "/Catln/Doc/dshow" `eApply ` ("/s", a)
    destType = classPartial $ partialVal "/Catln/Doc/DShow"
    destM = emptyMetaT destType
    canBuild typeEnv annot = reachesTo typeEnv H.empty (getExprType $ showAnnot annot) destType
    buildAnnots env@Env{evTbEnv, evTypeEnv} os annots = do
      let annots' = filter (canBuild evTypeEnv) annots
      forM annots' $ \annot -> do
        tree <- toTExprDest evTbEnv os (showAnnot annot) destM
        val <- evalStateT (evalExpr tree) env
        return (annot, val)

evalRun :: String -> FileImport -> EPrgmGraphData -> CResT IO (Integer, EvalResult)
evalRun function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  targetMode <- asCResT $ evalTargetMode function prgmName prgmGraphData
  input <- case targetMode of
        EvalRunWithContext function' ->
          -- Case for eval Context(value=main, io=IO)

          return $ eApplyM (eVal ContextStr `eApply` (contextValStr, eVal function')) "/io" ioM
        EvalRun function' ->
          -- Case for eval main
          return $ eVal function'
        _ -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to run" (show function)]
  let src = getExprPartialType input
  let dest = ioType
  (expr, env) <- asCResT $ evalBuildPrgm input src dest prgm
  let env2 = evalSetArgs (H.singleton "/io" (IOVal 0 $ pure ())) env
  (res, env') <- asCResT $ runStateT (evalExpr expr) env2
  case res of
    (IOVal r io) -> lift (io >> pure (r, evalResult env'))
    _ -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval did not return an instance of IO \n\tInstead returned %s \n\t With expr %s" (show res) (show expr)]

evalBuild :: String -> FileImport -> EPrgmGraphData -> CResT IO (Val, EvalResult)
evalBuild function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  targetMode <- asCResT $ evalTargetMode function prgmName prgmGraphData
  input <-  case targetMode of
        EvalRunWithContext function' ->
          -- Case for eval llvm(c=Context(value=main, io=IO))
          return $ eVal "/Catln/llvm" `eApply` ("/c", eVal function')
        EvalBuildWithContext function' ->
          -- Case for buildable Context(value=main, io=IO)
          return $ eApplyM (eVal ContextStr `eApply` (contextValStr, eVal function')) "/io" ioM
        EvalBuild function' ->
          -- Case for buildable main
          return $ eVal function'
        unexpectedEvalMode -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to build with unexpected eval mode %s" (show function) (show unexpectedEvalMode)]
  let src = getExprPartialType input
  let dest = resultType
  (expr, env) <- asCResT $ evalBuildPrgm input src dest prgm
  (res, env') <- asCResT $ runStateT (evalExpr expr) env
  case res of
    val@(TupleVal "/Catln/CatlnResult" args) -> case (H.lookup "/name" args, H.lookup "/contents" args) of
      (Just (StrVal _), Just (StrVal _)) -> return (val, evalResult env')
      _ -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval %s returned a /Catln/CatlnResult with bad args" function]
    (LLVMVal _) -> do
      -- llvmStr <- codegenExInit toCodegen
      let llvmStr = "LLVM Placeholder result"
      return (TupleVal "/Catln/CatlnResult" (H.fromList [("/name", StrVal "out.ll"), ("/contents", StrVal llvmStr)]), evalResult env')
    val -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval %s did not return a /Catln/CatlnResult. Instead it returned %s" function (show val)]
