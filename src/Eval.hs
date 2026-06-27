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

import           Control.Concurrent  (forkIO, killThread, newEmptyMVar,
                                      putMVar, takeMVar)
import           Control.Monad
import           Data.Graph
import           Data.IORef
import           Data.List           (nub, nubBy, tails)
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
import qualified Hedgehog
import qualified Hedgehog.Gen               as HG
import           Hedgehog.Internal.Property (propertyConfig, propertyTest)
import           Hedgehog.Internal.Report   (Result (OK), reportStatus)
import           Hedgehog.Internal.Runner   (checkReport)
import qualified Hedgehog.Internal.Seed     as HedgehogSeed
import           Semantics
import           Semantics.Annots    (getRuntimeAnnot, hasAnnot)
import           Semantics.TypeGraph (ReachesEnv (ReachesEnv),
                                      reachesHasCutSubtypeOf, reachesPartials,
                                      reachesTo)
import           System.Timeout      (timeout)
import           Testing.Generation  (genValForType)
import           Text.Printf
import           TreeBuild
import           Utils

-- | Iteration count for derived property tests (the Hedgehog default).
propTestIterations :: Hedgehog.TestLimit
propTestIterations = 100

-- | Per-property wall-clock timeout, in microseconds (10 seconds).
propTestTimeoutMicros :: Int
propTestTimeoutMicros = 10 * 1000 * 1000

-- | Run a Hedgehog property capped at both 'propTestIterations' variations and a
-- 'propTestTimeoutMicros' wall-clock timeout, returning whether it passed. On timeout the
-- property is judged on the variations that completed within the window: the runner returns
-- 'False' as soon as it finds a failing case, so if the timeout elapses before a result is
-- produced, no counterexample was found among the completed variations and it counts as a
-- pass.
--
-- The check runs in a worker thread while the timeout is applied to the main thread's wait
-- for its result. This keeps the timeout's async exception off the property computation
-- itself — wrapping the runner directly would let Hedgehog catch that exception and
-- misreport the interruption as a failing test case.
checkPropWithTimeout :: Hedgehog.Property -> IO Bool
checkPropWithTimeout prop = do
  resultVar <- newEmptyMVar
  tid <- forkIO $ runPropQuietly (Hedgehog.withTests propTestIterations prop) >>= putMVar resultVar
  result <- timeout propTestTimeoutMicros (takeMVar resultVar)
  case result of
    Just ok -> return ok
    Nothing -> killThread tid >> return True

-- | Run a Hedgehog property to completion without rendering any progress or result output,
-- returning whether it passed. Uses 'checkReport' with a no-op progress callback so that a
-- worker thread killed at the timeout boundary prints nothing — unlike 'Hedgehog.check',
-- whose interrupted live render emits a spurious "thread killed" failure report.
runPropQuietly :: Hedgehog.Property -> IO Bool
runPropQuietly prop = do
  seed <- HedgehogSeed.random
  report <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) (const (pure ()))
  return $ case reportStatus report of
    OK -> True
    _  -> False

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
evalPrgmTargetMode function prgm =
  case maybeGetSingleton $ expandRelPartial typeEnv H.empty (partialVal function) of
    Nothing -> NoEval (reachesPartials reachEnv [partialVal function]) (reachesPartials reachEnv [partialVal function])
    Just f ->
      let function' = ptName f
          funCtxTp = (partialVal ContextStr){ptArgs=H.fromList [(partialKey contextValStr, typeVal function')], ptArgMode=PtArgAny}
          funTp = partialVal function'
          funCtxReaches = reachesPartials reachEnv [funCtxTp]
          funReaches = reachesPartials reachEnv [funTp]
      in case (funCtxReaches, funReaches) of
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuildWithContext function'
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRunWithContext function'
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuild function'
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRun function' -- Should require isShowable for run result
           _ -> NoEval funCtxReaches funReaches
  where
    typeEnv = mkTypeEnv prgm
    reachEnv = ReachesEnv typeEnv H.empty S.empty

evalTargetMode :: String -> FileImport -> EPrgmGraphData -> CRes EvalMode
evalTargetMode function prgmName prgmGraphData = do
  prgm <- prgmFromGraphData prgmName prgmGraphData
  return $ evalPrgmTargetMode function prgm

-- | Gets the target modes for all objects in the file
evalAllTargetModes :: EPrgm -> H.HashMap UUID EvalMode
evalAllTargetModes prgm@Prgm{prgmObjMap} = H.fromList $ mapMaybe targetMode $ flatObjectMap prgmObjMap
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

-- | Reduce each partial's argument keys to their base name, dropping the function-parameter
-- signature carried in a higher-order argument's key (e.g. @/fn(/v=..)@ ↦ @/fn@). A runtime
-- value keys a function argument by its bare name (@/fn@) while the typed arrow keys it by
-- the function signature (@/fn(/v=..)@); without this they never match during dispatch even
-- though the function argument is non-discriminating (it is identical across the arrows).
simplifyArgKeys :: Type -> Type
simplifyArgKeys (UnionType m pos cs) = UnionType m (joinUnionType $ map reKey $ splitUnionType pos) cs
  where reKey p = p{ptArgs = H.mapKeys (partialKey . pkName) (ptArgs p)}
simplifyArgKeys t = t

evalCallTree :: Val -> TCallTree -> StateT Env CRes Val
-- evalCallTree v ct | trace (printf "evalCallTree %s using %s" (show v) (show ct)) False = undefined
evalCallTree v TCTId = return v
evalCallTree  m (TCMatch opts) = do
  Env{evTypeEnv} <- get
  -- Normalize higher-order argument keys (bare @/fn@ on the runtime value vs the
  -- function-signature key @/fn(/v=..)@ on the typed arrow) before matching, mirroring
  -- the 'simplifyArgKeys' handling in the 'TCCond' branch below.
  let reKeyP p = p{ptArgs = H.mapKeys (partialKey . pkName) (ptArgs p)}
  case H.toList $ H.filterWithKey (\optType _ -> isSubtypePartialOf evTypeEnv (reKeyP $ getValType m) (singletonType $ reKeyP optType)) opts of
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
    Just (_, oa) | not (isSubtypeOf evTypeEnv (simplifyArgKeys $ singletonType $ getValType m) (simplifyArgKeys $ clearUnionTypePreds $ getExprType $ oaObjExpr oa)) -> return False
    Nothing -> return True
    Just (ifCondTrees, ifObj) -> do
      Env{evArgs} <- get
      modify $ evalSetArgs $ buildArrArgs (Left ifObj) m
      conds' <- withEvalPush "cond" $ forM ifCondTrees $ \ifCondTree ->
        evalExpr ifCondTree
      modify $ evalSetArgs evArgs
      -- A non-linear pattern (@f(a@x, b@x)@) only applies when its shared positions are equal. That
      -- constraint is not a where-condition; it is checked here from the @ifObj@, comparing the
      -- relevant input fields through the argument type's 'Eq' (see 'evalNonLinearEq').
      nlOk <- withEvalPush "nonLinear" $ evalNonLinearEq m ifObj
      return $ all (true ==) conds' && nlOk
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

-- | Checks the implicit equality of a non-linear pattern's shared argument positions. For an
-- @ifObj@ such as @f(a@x, b@x)@, each non-linear group names input fields (here @a@ and @b@) that
-- must be equal for the clause to apply; this compares the corresponding values of the input tuple
-- @m@ pairwise. Equality goes through the argument type's 'Eq' (the @==@ operator over the two
-- values as constants), not Haskell structural equality, so a type with a custom equality behaves
-- as the user expects. If a value's type has no 'Eq', that surfaces as a build error here.
evalNonLinearEq :: Val -> EObjArr -> StateT Env CRes Bool
evalNonLinearEq (TupleVal _ tupleArgs) ifObj = allM
  [ evalValEq va vb
  | grp <- exprNonLinearArgGroups (oaObjExpr ifObj)
  , ((ea, _), (eb, _)) <- zip grp (tail grp)
  , Just va <- [H.lookup (exprPath ea) tupleArgs]
  , Just vb <- [H.lookup (exprPath eb) tupleArgs]
  ]
  where
    allM = foldr (\mb acc -> mb >>= \b -> if b then acc else return False) (return True)
evalNonLinearEq _ _ = return True

-- | Whether two runtime values are equal under their type's 'Eq', by building and evaluating
-- @l == r@ over the values as constants.
evalValEq :: Val -> Val -> StateT Env CRes Bool
evalValEq v1 v2 = do
  Env{evTbEnv} <- get
  let eqExpr = eVal operatorEq `eApply` (operatorArgL, valToEExpr v1) `eApply` (operatorArgR, valToEExpr v2)
  tree <- lift $ buildRoot evTbEnv eqExpr (getExprPartialType eqExpr) boolType
  (== true) <$> evalExpr tree

evalExpr :: TExpr EvalMetaDat -> StateT Env CRes Val
-- evalExpr e | trace (printf "eval %s" (show e)) False = undefined
evalExpr (TCExpr _ v) = return v
evalExpr (TValue m _) = do
  let tp = getMetaType m
  let PartialType{ptName} = case maybeGetSingleton tp of
        Just p -> p
        Nothing -> case tp of
          UnionType Nothing leafs [] -> head $ splitUnionType leafs
          _ -> error $ printf "TValue with unexpected type %s" (show tp)
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
evalExpr (TTupleApply _ (_, b) (EAppVar{})) = evalExpr b
evalExpr (TTupleApply _ (_, b) (EAppArg oa)) = do
  (TupleVal n args) <- evalExpr b
  v <- case oaArr oa of
    Just (Just oaExpr, _) -> case oaObj oa of
      Just TValue{} -> evalExpr oaExpr
      Just TTupleApply{} -> return $ ObjArrVal oa
      _ -> error $ printf "Unsupported eval argument of %s" (show oa)
    Just (Nothing, _) -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
    Nothing -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
  return $ TupleVal n (H.insert (oaObjPath oa) v args)
evalExpr (TTupleApply _ (_, _) (EAppSpread a)) = error $ printf "Not yet implemented evalExpr %s" (show a)
evalExpr (TCalls _ b callTree) = do
  b' <- evalExpr b
  evalCallTree b' callTree

evalBaseEnv :: EPrgm -> Env
evalBaseEnv prgm@Prgm{prgmObjMap} = Env {
        evObjMap = prgmObjMap,
        evTypeEnv = (mkTypeEnv prgm){teArgModeProjection=True},
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
  Just v -> return $ mconcat $ map (fst3 . nodeFromVertex) $ reachable prgmGraph v
  Nothing -> fail $ printf "Could not find prgm %s" (show prgmName)

-- | Tries to TreeBuild all ObjArrs, returning all built successfully
evalBuildAll :: EPrgmGraphData -> CRes (GraphData (Prgm TExpr EvalMetaDat) FileImport)
evalBuildAll prgmGraphData = do
  let prgms = graphToNodes prgmGraphData
  prgms' <- forM prgms $ \(Prgm objMap cg annots, prgmName, deps) -> do
    evalPrgm <- prgmFromGraphData prgmName prgmGraphData
    let Env{evTbEnv} = evalBaseEnv evalPrgm
    objMap' <- catCRes (buildRootOA evTbEnv <$> flatObjectMap objMap)
    annots' <- catCRes $ fmap (toTExpr evTbEnv []) annots
    let objMap'' = objectMapFromList objMap'
    return (Prgm objMap'' cg annots', prgmName, deps)
  return $ graphFromEdges prgms'

evalBuildPrgm :: EExpr -> PartialType -> Type -> EPrgm -> CRes (TExpr EvalMetaDat, Env)
evalBuildPrgm input srcType destType prgm = do
  let env@Env{evTbEnv} = evalBaseEnv prgm
  initTree <- buildRoot evTbEnv input srcType destType
  return (initTree, env)

evalAnnots :: FileImport -> EPrgmGraphData -> CRes [(EExpr, Val)]
evalAnnots prgmName prgmGraphData = do
  prgm@(Prgm objMap _ globalAnnots) <- prgmFromGraphData prgmName prgmGraphData
  let env = evalBaseEnv prgm
  globalAnnots' <- buildAnnots env [] globalAnnots
  objMapAnnots' <- forM (flatObjectMap objMap) $ \oa -> do
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

-- | A mock IO handle that captures printed lines in an internal buffer
-- instead of writing to real stdout.  Create with 'newMockIO', pass to
-- 'evalRunMock', then read captured output with 'getMockIOOutput'.
newtype MockIO = MockIO (IORef [String])

newMockIO :: IO MockIO
newMockIO = MockIO <$> newIORef []

-- | Returns all lines printed to the MockIO, joined with newlines
-- (equivalent to what would have appeared on stdout).
getMockIOOutput :: MockIO -> IO String
getMockIOOutput (MockIO ref) = unlines . reverse <$> readIORef ref

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

-- | Like 'evalRun' but captures IO output into the given 'MockIO' buffer
-- instead of writing to real stdout.
evalRunMock :: MockIO -> String -> FileImport -> EPrgmGraphData -> CResT IO (Integer, EvalResult)
evalRunMock (MockIO ref) function prgmName prgmGraphData = do
  prgm <- asCResT $ prgmFromGraphData prgmName prgmGraphData
  targetMode <- asCResT $ evalTargetMode function prgmName prgmGraphData
  input <- case targetMode of
        EvalRunWithContext function' ->
          return $ eApplyM (eVal ContextStr `eApply` (contextValStr, eVal function')) "/io" ioM
        EvalRun function' ->
          return $ eVal function'
        _ -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "Eval could not find a function %s to run" (show function)]
  let src = getExprPartialType input
  let dest = ioType
  (expr, env) <- asCResT $ evalBuildPrgm input src dest prgm
  let env2 = evalSetArgs (H.singleton "/io" (MockIOVal 0 (pure ()) ref)) env
  (res, env') <- asCResT $ runStateT (evalExpr expr) env2
  case res of
    (MockIOVal r io _) -> lift (io >> pure (r, evalResult env'))
    _ -> asCResT $ CErr [MkCNote $ GenCErr Nothing $ printf "evalRunMock did not return a MockIOVal \n\tInstead returned %s \n\t With expr %s" (show res) (show expr)]

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

-- | Derive a property test for every pair of evaluable same-name definitions whose domains
-- overlap (their argument types intersect): on the overlap the two definitions must AGREE. A
-- non-linear "diagonal" definition (e.g. @rel(a@x, b@x)= True@) overlaps every full definition it
-- shadows, so this turns the diagonal into a check that each full definition matches it where they
-- meet — for @Eqv.rel@ that overlap is exactly reflexivity. Each definition is evaluated in
-- isolation (bypassing dispatch, which would only ever run one of them) and the results compared
-- through their 'Eq'. Inputs are generated in the overlap, with positions that a non-linear
-- pattern ties together generated equal so the diagonal is actually exercised.
deriveOverlapTests :: EPrgm -> [(String, CResT IO Val)]
deriveOverlapTests prgm@(Prgm (ObjectMap om) _ _) =
  [ mkOverlapTest omi oaA oaB dom
  | omi <- H.elems om
  , let parts = filter evaluable (omiDefs omi ++ omiRuntimeDecls omi)
  , (oaA, oaB) <- pairs parts
  , let dom = intersectTypes typeEnv (clauseDom oaA) (clauseDom oaB)
  , not (isBottomType dom)
  , isJust (maybeGetSingleton dom)
  ]
  where
    TypeEnv{teNames} = typeEnv
    typeEnv = mkTypeEnv prgm
    -- A clause we can evaluate: a definition with a body, or a runtime primitive (evaluated through
    -- its @#runtime@ implementation). Pairing a non-linear diagonal against a primitive sibling is
    -- how @(_@x - _@x)= 0@ becomes a derived test of the built-in subtraction (@x - x == 0@).
    evaluable oa = case oaArr oa of
      Just (Just _, _)  -> True
      Just (Nothing, _) -> isJust (getRuntimeAnnot (oaAnnots oa))
      _                 -> False
    clauseDom oa = getExprType (oaObjExpr oa)
    pairs xs = [(a, b) | (a:rest) <- tails xs, b <- rest]

    mkOverlapTest omi oaA oaB dom = (testName, action)
      where
        -- The function's declared return type (predicates cleared) is the destination to build each
        -- call against. It is narrower than a clause's inferred result, so the call's own type is not
        -- already a subtype of it — which forces the tree builder to actually build and evaluate the
        -- body to a concrete value rather than leaving it as an unevaluated application.
        retDest = clearUnionTypePreds $ case [getMetaType m | d <- omiDecls omi ++ omiRuntimeDecls omi, Just (_, m) <- [oaArr d]] of
          (t:_) -> t
          []    -> intType
        testName = printf "%s :: overlap-agreement" (oaObjPath oaA)
        partial = getSingleton dom
        groups = map (map (exprPath . fst)) (exprNonLinearArgGroups (oaObjExpr oaA) ++ exprNonLinearArgGroups (oaObjExpr oaB))
        repOf n = case filter (n `elem`) groups of
          (g:_) -> minimum g
          []    -> n
        argTypeMap = H.fromList [(pkName k, t) | (k, t) <- H.toList (ptArgs partial)]
        genArg t = genValForType (UnionType Nothing (expandTypeWithNames typeEnv H.empty t teNames) [])
        genInput = do
          let argNames = H.keys argTypeMap
          let reps = nub (map repOf argNames)
          repVals <- mapM (\r -> (,) r <$> genArg (H.lookupDefault PTopType r argTypeMap)) reps
          let repMap = H.fromList repVals
          return $ TupleVal (ptName partial) (H.fromList [(n, H.lookupDefault NoVal (repOf n) repMap) | n <- argNames])
        -- To evaluate one clause of the overlapping pair in isolation, remove the OTHER clause from
        -- its function so a normal call dispatches to the one we want (bypassing the overlap), then
        -- build and evaluate that call. Building the full call (rather than the body alone) resolves
        -- the body's nested operators/primitives against a concrete result type. Removing (rather
        -- than keeping only one) preserves a runtime-primitive sibling, which has no body to keep.
        withoutClause dropOa o = o{ omiDefs = filter (/= dropOa) (omiDefs o)
                                  , omiRuntimeDecls = filter (/= dropOa) (omiRuntimeDecls o) }
        soloEnv dropOa = evalBaseEnv prgm{prgmObjMap = ObjectMap (H.adjust (withoutClause dropOa) (oaObjPath oaA) om)}
        envA = soloEnv oaB
        envB = soloEnv oaA
        runClause env input = do
          let inputE = valToEExpr input
          tree <- buildRoot (evTbEnv env) inputE (getExprPartialType inputE) retDest
          fst <$> runStateT (evalExpr tree) env
        prop = Hedgehog.property $ do
          input <- Hedgehog.forAll genInput
          case (runClause envA input, runClause envB input) of
            (CRes _ va, CRes _ vb) -> case runStateT (evalValEq va vb) envA of
              CRes _ (True, _)  -> Hedgehog.success
              CRes _ (False, _) -> Hedgehog.footnote (printf "Overlapping definitions disagree on %s:\n  %s\n  %s" (show input) (show va) (show vb)) >> Hedgehog.failure
              CErr ns           -> Hedgehog.footnote (printf "Eq compare of results failed (%s vs %s): %s" (show va) (show vb) (prettyCNotes ns)) >> Hedgehog.failure
            (ra, rb) -> Hedgehog.footnote (printf "eval failed:\n  %s\n  %s" (errOf ra) (errOf rb)) >> Hedgehog.failure
        errOf (CErr ns) = prettyCNotes ns
        errOf _         = "ok"
        action = CResT $ do
          ok <- checkPropWithTimeout prop
          return $ if ok
            then CRes [] (TupleVal "()" H.empty)
            else CErr [MkCNote $ GenCErr Nothing $ printf "Overlap agreement test failed: %s" testName]

evalTest :: EPrgmGraphData -> CRes [(String, CResT IO Val)]
evalTest prgmGraphData = do
  let prgm@(Prgm objMap _ _) = mconcat $ map fst3 $ graphToNodes prgmGraphData
  let testOAs = nubBy (\a b -> oaObjPath a == oaObjPath b) $ filter (\oa -> hasAnnot testAnnot oa || hasAnnot exampleAnnot oa) $ flatObjectMap objMap
  let (noArgTests, propTests) = foldr partitionTest ([], []) testOAs
  return $ map (runSimpleTest prgm) noArgTests ++ map (runPropertyTest prgm) propTests ++ deriveOverlapTests prgm
  where
    isNoArgTest oa = null (exprAppliedArgs (oaObjExpr oa)) && null (exprAppliedOrdVars (oaObjExpr oa))
    partitionTest oa (simple, props)
      | isNoArgTest oa = (oa : simple, props)
      | otherwise      = (simple, oa : props)

    runSimpleTest p oa =
      let testName = oaObjPath oa
          action = asCResT $ do
            let input = eVal testName
            let src = getExprPartialType input
            (expr, env) <- evalBuildPrgm input src intType p
            fst <$> runStateT (evalExpr expr) env
      in (testName, action)

    runPropertyTest p oa =
      let testName = oaObjPath oa
          typeEnv  = mkTypeEnv p
          typeVars = exprAppliedOrdVars (oaObjExpr oa)
          args     = exprAppliedArgs (oaObjExpr oa)

          TypeEnv{teNames} = typeEnv

          -- Resolve a type variable's constraint to a list of concrete PartialTypes
          resolveVar (_varName, varMeta) =
            splitUnionType $ expandTypeWithNames typeEnv H.empty (getMetaType varMeta) teNames

          concreteTypesPerVar = map resolveVar typeVars

          -- The type-parameter bindings a class records for a chosen member instance. A class node
          -- stores each member with the parameters that member fixes (e.g. @FloatOrdering@ under
          -- @Ordering@ carries @$T -> Float@), so this recovers a chosen instance's element type —
          -- which the flattened class expansion drops.
          classNodes = let ClassGraph cg = teClassGraph typeEnv in graphToNodes cg
          instanceVarBindings instName = H.unions
            [ ptVars mp
            | (CGClass (_, _, ts, _), _, _) <- classNodes
            , mt <- ts
            , mp <- case mt of UnionType Nothing ls [] -> splitUnionType ls; _ -> []
            , ptName mp == instName ]

          prop = Hedgehog.property $ do
            -- For each type variable, choose a concrete PartialType
            chosenTypes <- mapM (\concretes ->
              if null concretes
                then Hedgehog.discard
                else Hedgehog.forAll (HG.element concretes)
              ) concreteTypesPerVar

            -- Build a map from type var name to chosen concrete type
            let varTypeMap = H.fromList $ zip (map fst typeVars) chosenTypes
            -- A chosen instance fixes further type variables through its class parameters (e.g.
            -- choosing $E -> Eqv[$T] determines the underlying $T). Expose those nested bindings so
            -- a value arg typed by them (such as @t -> $T@) generates the right concrete value.
            let nestedTypeMap = H.unions (map (instanceVarBindings . ptName) chosenTypes)

            -- For each value argument, determine its concrete type and generate a value. The
            -- resolved type may be a class (e.g. an instance's element type is @Boolean@), so expand
            -- it to its concrete leaves before generating.
            argVals <- mapM (\argOA ->
              let argType = case oaArr argOA of
                    Just (_, arrMeta) -> case getMetaType arrMeta of
                      TypeVar (TVVar vn) _ -> case H.lookup vn nestedTypeMap of
                        Just t  -> t
                        Nothing -> singletonType $ H.lookupDefault intLeaf vn varTypeMap
                      t@(UnionType Nothing _ []) -> t
                      _                          -> intType
                    Nothing -> intType
                  concreteArg = UnionType Nothing (expandTypeWithNames typeEnv H.empty argType teNames) []
              in Hedgehog.forAll (genValForType concreteArg)
              ) args

            -- Build the expression: testName[$T=ConcreteType, ...](arg0=val0, ...)
            -- First apply type variables so the body can resolve class-based calls
            let typeVarInput = foldl (\base (varName, chosenType) -> eAppVar base varName chosenType)
                                     (eVal testName)
                                     (zip (map fst typeVars) chosenTypes)
            let argNames = map oaObjPath args
            let input = foldl (\base (n, v) -> eApply base (n, valToEExpr v))
                               typeVarInput
                               (zip argNames argVals)
            let src = getExprPartialType input

            case evalBuildPrgm input src intType p of
              CErr notes -> Hedgehog.footnote (prettyCNotes notes) >> Hedgehog.failure
              CRes _ (expr, env) -> case runStateT (evalExpr expr) env of
                CErr notes -> Hedgehog.footnote (prettyCNotes notes) >> Hedgehog.failure
                CRes _ _   -> Hedgehog.success

          action = CResT $ do
            ok <- checkPropWithTimeout prop
            return $ if ok
              then CRes [] (TupleVal "()" H.empty)
              else CErr [MkCNote $ GenCErr Nothing $ printf "Property test failed: %s" testName]
      in (testName, action)
