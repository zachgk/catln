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
evalPrgmTargetMode function prgm =
  case maybeGetSingleton $ expandRelPartial typeEnv H.empty (partialVal (mkName function)) of
    Nothing -> NoEval (reachesPartials reachEnv [partialVal (mkName function)]) (reachesPartials reachEnv [partialVal (mkName function)])
    Just f ->
      let function' = ptName f
          funCtxTp = (partialVal (mkName ContextStr)){ptArgs=H.fromList [(partialKey (mkName contextValStr), typeVal function')], ptArgMode=PtArgAny}
          funTp = partialVal function'
          funCtxReaches = reachesPartials reachEnv [funCtxTp]
          funReaches = reachesPartials reachEnv [funTp]
      in case (funCtxReaches, funReaches) of
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuildWithContext (nameStr function')
           (rt, _) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRunWithContext (nameStr function')
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt resultType -> EvalBuild (nameStr function')
           (_, rt) | reachesHasCutSubtypeOf typeEnv H.empty rt ioType -> EvalRun (nameStr function') -- Should require isShowable for run result
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
    targetMode oa = case nameStr (oaObjPath oa) of
      ContextStr -> case H.lookupDefault Nothing (partialKey (mkName contextValStr)) $ exprAppliedArgsMap $ oaObjExpr oa of
        Just (_, Just valE) -> case maybeExprPath valE of
          Just valName -> Just (getMetaID $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (nameStr valName) prgm)
          _ -> Nothing
        _ -> Nothing
      _ -> Just (getMetaID $ getExprMeta $ oaObjExpr oa, evalPrgmTargetMode (nameStr (oaObjPath oa)) prgm)

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
evalObjArr = evalObjArrCaptured H.empty

-- | 'evalObjArr' for a closure ('ObjArrVal'): the captured creation-site args underlay the
-- closure's own parameters, so body references to enclosing args resolve lexically.
evalObjArrCaptured :: Args -> Val -> AnyObjArr -> StateT Env CRes Val
evalObjArrCaptured captured input oa = do
  let newArrArgs = H.union (buildArrArgs oa input) captured
  -- The src partial seeds the arm-body rebuild's var/arg environment, where a function-typed
  -- slot's VALUE is its output type (the input signature lives in the key — the typechecker's
  -- pointer-slot convention). The input-signature form would bind an arm's output var to the
  -- signature (@$M := /fn(/v=Int)@) and leave the body's argument application unable to reach
  -- a concrete destination type.
  (resArrowTree, compAnnots, oldArgs) <- evalStartEArrow (getValPartialTypeOut input) oa newArrArgs
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
evalCallTree  m (TCMatch destType opts) = do
  Env{evTypeEnv} <- get
  -- Normalize higher-order argument keys (bare @/fn@ on the runtime value vs the
  -- function-signature key @/fn(/v=..)@ on the typed arrow) before matching, mirroring
  -- the 'simplifyArgKeys' handling in the 'TCCond' branch below.
  let reKeyP p = p{ptArgs = H.mapKeys (partialKey . pkName) (ptArgs p)}
  -- A closure argument's slot in a typed arm holds either the closure's input signature or the
  -- output contract delivered from the declaration; the value-derived partial only carries the
  -- input signature, so also try the output form ('getValPartialTypeOut') before failing an arm.
  let valP = reKeyP $ getValPartialType m
  let valPOut = reKeyP $ getValPartialTypeOut m
  let optMatches optType = isSubtypePartialOf evTypeEnv valP (singletonType $ reKeyP optType)
        || (valPOut /= valP && isSubtypePartialOf evTypeEnv valPOut (singletonType $ reKeyP optType))
  case H.toList $ H.filterWithKey (\optType _ -> optMatches optType) opts of
    [(_, resArrowTree)] -> do
      withEvalPush ("match with val " ++ show m) $ evalCallTree m resArrowTree
    -- No statically-built arm matches the concrete runtime value. An arm can be dropped at build
    -- time by a COMPILE-TIME-ONLY cyclic dispatch (e.g. an 'Eq' instance whose @==@ arm dispatches
    -- back through the same function — the 'ConsList' 'equal' arm recursing through 'tail.equal').
    -- The runtime value is
    -- concrete, so rebuilding the dispatch from its type cannot cycle. Rebuild from the value and
    -- retry once; if the rebuild is not strictly more resolved, fall back to the original error.
    [] -> do
      Env{evTbEnv} <- get
      case buildPartialCallTree evTbEnv [] S.empty (getValPartialType m) destType of
        CRes _ rebuilt | rebuilt /= TCMatch destType opts ->
          withEvalPush ("rebuild match with val " ++ show m) $ evalCallTree m rebuilt
        _ -> evalError $ printf "Failed match in eval callTree: \n\tVal: %s \n\tVal type: %s\n\tOptions: %s" (show m) (show $ getValPartialType m) (show opts)
    (_:_:_) -> evalError $ printf "Multiple matches in eval callTree: \n\tVal: %s \n\tOptions: %s " (show m) (show opts)
evalCallTree v (TCSeq a b) = do
  v' <- evalCallTree v a
  evalCallTree v' b
evalCallTree _ (TCCond _ []) = evalError "Found no matching conditions in evalTree"
evalCallTree m (TCCond resType ((cond, ifThenTree):restIfTrees)) = do
  Env{evTypeEnv} <- get
  -- The value's partial is tried in both slot conventions, like the 'TCMatch' branch above: a
  -- closure argument's slot in the arm's object holds either the closure's input signature or
  -- the output contract delivered from the declaration.
  let condMatches armOa =
        let armT = simplifyArgKeys $ clearUnionTypePreds $ getExprType $ oaObjExpr armOa
            valT = simplifyArgKeys $ singletonType $ getValPartialType m
            valTOut = simplifyArgKeys $ singletonType $ getValPartialTypeOut m
        in isSubtypeOf evTypeEnv valT armT || (valTOut /= valT && isSubtypeOf evTypeEnv valTOut armT)
  cond' <- case cond of
    -- TODO Remove the use of clearUnionTypePreds below. It is a workaround as type predicates are not supported by subtypeOf
    Just (_, oa) | not (condMatches oa) -> return False
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
    Just (ObjArrVal oa captured) -> evalObjArrCaptured captured input (Right oa)
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
  , Just va <- [H.lookup (nameStr (exprPath ea)) tupleArgs]
  , Just vb <- [H.lookup (nameStr (exprPath eb)) tupleArgs]
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
  return $ TupleVal (nameStr ptName) H.empty
evalExpr (THoleExpr m h) = lift $ CErr [MkCNote $ GenCErr (getMetaPos m) $ printf "Can't evaluate hole %s" (show h)]
evalExpr (TAliasExpr b _) = evalExpr b
evalExpr (TWhere _ b _) = evalExpr b
evalExpr (TTupleApply _ (_, b) (EAppArg oa@ObjArr{oaObj=Just (TValue _ "/io"), oaArr=Just (Nothing, _)})) = do
  Env{evArgs} <- get
  TupleVal n args <- evalExpr b
  case H.lookup "/io" evArgs of
    Just io -> return $ TupleVal n (H.insert (nameStr (oaObjPath oa)) io args)
    Nothing -> error $ printf "evalExpr with no io"
evalExpr (TTupleApply _ (_, b) (EAppVar{})) = evalExpr b
evalExpr (TTupleApply _ (_, b) (EAppArg oa)) = do
  (TupleVal n args) <- evalExpr b
  v <- case oaArr oa of
    Just (Just oaExpr, _) -> case oaObj oa of
      Just TValue{} -> evalExpr oaExpr
      Just TTupleApply{} -> do
        Env{evArgs=creationArgs} <- get
        return $ ObjArrVal oa creationArgs
      _ -> error $ printf "Unsupported eval argument of %s" (show oa)
    Just (Nothing, _) -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
    Nothing -> error $ printf "Missing arrExpr in evalExpr TupleApply with %s - %s" (show b) (show oa)
  return $ TupleVal n (H.insert (nameStr (oaObjPath oa)) v args)
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

-- | Build and evaluate a single annotation expression to the value it renders to through the
-- @DShow@ class (the same path used to display @#print@ values in webdocs). Returns @Nothing@
-- when the annotation cannot reach @DShow@ and so is not renderable.
evalDShowAnnot :: Env -> [(PartialType, EObjArr)] -> EExpr -> CRes (Maybe Val)
evalDShowAnnot env@Env{evTbEnv, evTypeEnv} os annot0
  | isPrint = do
      -- A @#print@ displays a runtime VALUE. Evaluate @p@ to its concrete value (built toward
      -- @DShow@, which certifies displayability and yields the value itself), then render THAT
      -- value as text. Driving the render from the evaluated value — not @p@'s statically-inferred
      -- type — is what lets a computed or interface-typed result print. @sorted([3,1,2])@ has
      -- static type @/Data/List@ (the interface, which is not a @ToString@) and, being a function
      -- call, cannot dispatch @toString@ on its own name; but its value is a concrete @ConsList@ of
      -- @Integer@s, which renders as @[1, 2, 3]@. Building @toString(this= p)@ from @p@'s static
      -- type instead only ever worked for string/scalar/literal @p@ and silently skipped the rest.
      pVal <- evalToValue annot
      -- Render the value as a @String@ through @/Data/toString@ when it is @ToString@ (the only
      -- form a golden can check); a @DShow@ value that is not @ToString@ (a webdocs visualization)
      -- keeps its raw value, so it is displayed but not golden-checked.
      return $ Just $ case renderValString pVal of
        CRes _ str@StrVal{} -> str
        _                   -> pVal
  | reachesTo evTypeEnv H.empty (getExprType $ showAnnot annot) destType = do
      tree <- toTExprDest evTbEnv os (showAnnot annot) destM
      val <- evalStateT (evalExpr tree) env
      return (Just val)
  | otherwise = return Nothing
  where
    isPrint = nameStr (exprPath annot0) == printAnnot
    -- A #print's rendered value is its `p` argument, so render that directly. This is robust to
    -- the print also carrying a `result` golden (the 2-arg #print), which the bare
    -- @dshow(s= #print(p))= p@ rule would not match.
    annot
      | isPrint
      , Just (Just (_, Just p)) <- H.lookup (partialKey (mkName printAnnotText)) (exprAppliedArgsMap annot0)
      = p
      | otherwise = annot0
    showAnnot a = eVal "/Catln/Doc/dshow" `eApply ` ("/s", a)
    -- Render a value as text through @/Data/toString@. The value is reconstructed as a typed
    -- constant expression ('valToEExpr', whose meta carries the concrete runtime type — e.g. a
    -- @ConsList@ of @Integer@s), and @toString@ is built over that; the concrete receiver type
    -- dispatches where @p@'s abstract static type could not. Fails (leaving the caller with the raw
    -- value) for a value 'valToEExpr' cannot reconstruct or one that is not @ToString@.
    renderValString v
      | reconstructableVal v = do
          let e = eVal "/Data/toString" `eApply` ("/this", valToEExpr v)
          tree <- buildRoot evTbEnv e (getExprPartialType e) destType
          evalStateT (evalExpr tree) env
      | otherwise = CErr []
    -- Whether 'valToEExpr' can rebuild this value (it errors on closures, IO, and other runtime-only
    -- values, none of which are @ToString@).
    reconstructableVal IntVal{}         = True
    reconstructableVal FloatVal{}       = True
    reconstructableVal StrVal{}         = True
    reconstructableVal CharVal{}        = True
    reconstructableVal (TupleVal _ as)  = all reconstructableVal (H.elems as)
    reconstructableVal _                = False
    -- Evaluate @p@ to its concrete value by building it toward @DShow@ (which certifies
    -- displayability and yields the value itself). @p@ is declared @-> DShow@, so this resolves for
    -- every valid print.
    evalToValue a = do
      tree <- toTExprDest evTbEnv os a destM
      evalStateT (evalExpr tree) env
    destType = classPartial $ partialVal "/Catln/Doc/DShow"
    destM = emptyMetaT destType

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
    buildAnnots env os annots = do
      results <- forM annots $ \annot -> fmap (annot,) <$> evalDShowAnnot env os annot
      return $ catMaybes results

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
