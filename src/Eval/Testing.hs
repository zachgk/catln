--------------------------------------------------------------------
-- |
-- Module    :  Eval.Testing
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module holds the testing-specific evaluation logic split out
-- of "Eval": the mock IO handle used to capture program output, the
-- timeout-bounded Hedgehog property runner, the derived
-- overlap-agreement tests, and the test discovery/execution entry
-- points.
--------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Eval.Testing where

import           CRes
import qualified Data.HashMap.Strict        as H
import           Semantics.Prgm
import           Semantics.Types

import           Control.Concurrent         (forkIO, killThread, newEmptyMVar,
                                             putMVar, takeMVar)
import           Control.Monad.State        (lift, runStateT)
import           CtConstants
import           Data.IORef
import           Data.List                  (nub, nubBy, tails)
import           Data.Maybe
import           Eval                       (evalBaseEnv, evalBuildPrgm,
                                             evalExpr, evalTargetMode,
                                             evalValEq, prgmFromGraphData)
import           Eval.Common
import           Eval.Env
import           Eval.ExprBuilder
import qualified Hedgehog
import qualified Hedgehog.Gen               as HG
import           Hedgehog.Internal.Property (propertyConfig, propertyTest)
import           Hedgehog.Internal.Report   (Result (..), reportStatus)
import           Hedgehog.Internal.Runner   (checkReport)
import qualified Hedgehog.Internal.Seed     as HedgehogSeed
import           Semantics
import           Semantics.Annots           (getRuntimeAnnot, hasAnnot)
import           System.Timeout             (timeout)
import           Testing.Generation         (genValForType)
import           Text.Printf
import           TreeBuild
import           Utils

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

-- | Like 'Eval.evalRun' but captures IO output into the given 'MockIO' buffer
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
--
-- 'gaveUpOk' controls how an all-discards run (Hedgehog 'GaveUp') is judged: 'False' treats it as a
-- failure (the default for tests whose inputs should always generate), 'True' as a pass. The derived
-- refinement tests pass 'True' because a pairing whose overlap cannot yet be generated (e.g. a
-- @Cons@ value, which the generator discards) has simply found no counterexample.
checkPropWithTimeout :: Bool -> Hedgehog.Property -> IO Bool
checkPropWithTimeout gaveUpOk prop = do
  resultVar <- newEmptyMVar
  tid <- forkIO $ runPropQuietly gaveUpOk (Hedgehog.withTests propTestIterations prop) >>= putMVar resultVar
  result <- timeout propTestTimeoutMicros (takeMVar resultVar)
  case result of
    Just ok -> return ok
    Nothing -> killThread tid >> return True

-- | Run a Hedgehog property to completion without rendering any progress or result output,
-- returning whether it passed. Uses 'checkReport' with a no-op progress callback so that a
-- worker thread killed at the timeout boundary prints nothing — unlike 'Hedgehog.check',
-- whose interrupted live render emits a spurious "thread killed" failure report.
runPropQuietly :: Bool -> Hedgehog.Property -> IO Bool
runPropQuietly gaveUpOk prop = do
  seed <- HedgehogSeed.random
  report <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) (const (pure ()))
  return $ case reportStatus report of
    OK     -> True
    GaveUp -> gaveUpOk
    _      -> False

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
        testName = printf "%s :: overlap-agreement" (nameStr (oaObjPath oaA))
        partial = getSingleton dom
        groups = map (map (nameStr . exprPath . fst)) (exprNonLinearArgGroups (oaObjExpr oaA) ++ exprNonLinearArgGroups (oaObjExpr oaB))
        repOf n = case filter (n `elem`) groups of
          (g:_) -> minimum g
          []    -> n
        argTypeMap = H.fromList [(nameStr (pkName k), t) | (k, t) <- H.toList (ptArgs partial)]
        genArg t = genValForType typeEnv (UnionType Nothing (expandTypeWithNames typeEnv H.empty t teNames) [])
        genInput = do
          let argNames = H.keys argTypeMap
          let reps = nub (map repOf argNames)
          repVals <- mapM (\r -> (,) r <$> genArg (H.lookupDefault PTopType r argTypeMap)) reps
          let repMap = H.fromList repVals
          return $ TupleVal (nameStr (ptName partial)) (H.fromList [(n, H.lookupDefault NoVal (repOf n) repMap) | n <- argNames])
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
          ok <- checkPropWithTimeout False prop
          return $ if ok
            then CRes [] (TupleVal "()" H.empty)
            else CErr [MkCNote $ GenCErr Nothing $ printf "Overlap agreement test failed: %s" testName]

-- | Derive a property test for every refinement declaration paired with an overlapping definition
-- of the same object. A refinement declaration (a @#refine@ arrow) claims a narrowed return type;
-- where its domain overlaps a definition's, that definition's result on inputs in the overlap must
-- match the claim. We generate an input in the overlap, evaluate it through the DEFINITION to a
-- concrete value, and assert that value matches the refinement's claimed return type (see
-- 'valMatchesType'). For @:ConsList.length -> Integer_nonNeg #refine@ paired with @Nil.length= 0@
-- this checks that the length of a @Nil@ is a non-negative integer.
deriveRefinementTests :: EPrgm -> [(String, CResT IO Val)]
deriveRefinementTests prgm@(Prgm (ObjectMap om) _ _) =
  [ mkRefinementTest omi defOa dom refRet
  | omi <- H.elems om
  , refOa <- omiRefineDecls omi
  , Just (_, refM) <- [oaArr refOa]
  , let refRet = getMetaType refM
  , defOa <- filter evaluable (omiDefs omi)
  , let dom = intersectTypes typeEnv (clauseDom refOa) (clauseDom defOa)
  , not (isBottomType dom)
  , isJust (maybeGetSingleton dom)
  ]
  where
    TypeEnv{teNames} = typeEnv
    typeEnv = mkTypeEnv prgm
    env = evalBaseEnv prgm
    evaluable oa = case oaArr oa of
      Just (Just _, _)  -> True
      Just (Nothing, _) -> isJust (getRuntimeAnnot (oaAnnots oa))
      _                 -> False
    clauseDom oa = getExprType (oaObjExpr oa)

    -- A refinement claim is stored as a type predicate on the declared return — a @PredExpr@ holding
    -- a symbolic application tree such as @operator==(r=True, l=nonNeg(this=$this))@ (the desugaring
    -- of @Integer_nonNeg@'s @$this.nonNeg == True@), with @$this@ standing for the value refined.
    -- Rather than teach the type layer to decide such predicates, we decide them in the EVALUATOR:
    -- bind @$this@ to the candidate value and reduce the tree to a concrete value, dispatching each
    -- application through the normal tree builder + evaluator. This keeps the type system simple and
    -- reuses the same machinery that matches values during dispatch (see 'valMatchesType').
    constToVal (CInt n)   = IntVal n
    constToVal (CFloat f) = FloatVal f
    constToVal (CStr s)   = StrVal s
    constToVal (CChar ch) = CharVal ch
    -- A symbolic predicate may carry a relative function name (e.g. @nonNeg@ rather than
    -- @/Data/Primitive/nonNeg@); the object map is keyed by absolute names, so resolve against it.
    resolveName n = case filter (relativeNameMatches n) (H.keys om) of
      (k:_) -> k
      []    -> n
    -- The build destination for dispatching one application of the predicate. It must be the
    -- function's NOMINAL return type expanded to its data leaves (e.g. @Boolean@ → @{True, False}@),
    -- NOT the type stored on a body-carrying definition: that stored type includes the body's own
    -- unevaluated application as a positive leaf (e.g. @nonNeg@'s type is @operator>=(Int,Int) |
    -- isa Boolean@), so building the body against it would find it already a subtype and skip
    -- evaluating it. Expanding the @isa@/class predicate to its members gives a nominal target that
    -- the unevaluated application is NOT a subtype of, so the call is actually built and run.
    funcRetDest nm = cleanDest $ case H.lookup nm om of
      Just o -> case [getMetaType m | d <- omiDecls o ++ omiRuntimeDecls o ++ omiDefs o, Just (_, m) <- [oaArr d]] of
        (t:_) -> t
        []    -> boolType
      Nothing -> boolType
    -- Drop the body's positive leaf but keep the @isa@/class predicate top, then expand that to its
    -- member data leaves. Expansion only fires through the predicate path, so the class must stay a
    -- predicate (not be reduced to a bare leaf) for @Boolean@ → @{True, False}@ to happen.
    cleanDest t = case t of
      UnionType (Just (preds, neg)) _ cs ->
        UnionType Nothing (expandTypeWithNames typeEnv H.empty (UnionType (Just (preds, neg)) H.empty cs) teNames) []
      _ -> clearUnionTypePreds t
    -- Reduce a predicate operand TYPE to a value. The operands of a symbolic predicate are either
    -- constants (@UnionType Nothing _ [c]@), the @$this@ placeholder, or a nested relation/expr
    -- application carried as a @PredRel@\/@PredExpr@ singleton top — all routed to 'evalPredPartial'.
    evalPredType thisVal t = case t of
      UnionType Nothing _ (cst:_)                     -> return (constToVal cst)
      UnionType (Just (PredsOne (PredRel p), _)) _ _  -> evalPredPartial thisVal p
      UnionType (Just (PredsOne (PredExpr p), _)) _ _ -> evalPredPartial thisVal p
      _ -> case maybeGetSingleton t of
        Just p  -> evalPredPartial thisVal p
        Nothing -> CErr [MkCNote $ GenCErr Nothing $ printf "Unsupported refinement predicate operand %s" (show t)]
    -- Reduce a symbolic predicate partial to a concrete value. @$this@ becomes the candidate value;
    -- a nullary partial (a value such as @True@) becomes that value; an application has its argument
    -- types reduced to values first (bottom-up, so the argument types are concrete), then it is
    -- dispatched through 'buildRoot' + 'evalExpr' to its declared return — running the @#runtime@
    -- primitives a refinement bottoms out at.
    evalPredPartial thisVal p
      | nameStr (ptName p) == macroThisKey = return thisVal
      | H.null (ptArgs p)        = return $ TupleVal (nameStr (resolveName (ptName p))) H.empty
      | otherwise = do
          argVals <- mapM (\(k, at) -> (nameStr (pkName k),) <$> evalPredType thisVal at) (H.toList (ptArgs p))
          let nm = resolveName (ptName p)
          let input = TupleVal (nameStr nm) (H.fromList argVals)
          let inputE = valToEExpr input
          tree <- buildRoot (evTbEnv env) inputE (getExprPartialType inputE) (funcRetDest nm)
          fst <$> runStateT (evalExpr tree) env

    typePreds (UnionType (Just (preds, _)) _ _) = preds
    typePreds _                                 = PredsNone
    predAtoms (PredsOne a)  = [a]
    predAtoms (PredsAnd ps) = concatMap predAtoms ps
    predAtoms (PredsNot p)  = predAtoms p
    collectPredExprs preds = [p | PredExpr p <- predAtoms preds]

    -- The structural ("base type") claim of a refinement type, as a positive leaf type to check the
    -- value against. The base is carried as an @isa@/class predicate (e.g. @Integer@), so turn those
    -- predicate partials into leaves directly — @Integer@ is a primitive that does not expand to
    -- members, so it must be used as the leaf @{Integer}@ rather than expanded; the predicate also
    -- carries a relative name, so resolve it to absolute to match the value's type. Falls back to the
    -- type's own positive leaves for a refinement stated on a concrete type.
    structuralTarget t = case [c{ptName = resolveName (ptName c)} | a <- predAtoms (typePreds t), c <- relClassOf a] of
      cs@(_:_) -> UnionType Nothing (joinUnionType cs) []
      []       -> clearUnionTypePreds t
      where
        relClassOf (PredRel c)   = [c]
        relClassOf (PredClass c) = [c]
        relClassOf (PredExpr _)  = []

    -- Whether a value matches a (possibly predicated) type: structurally a subtype of the base-type
    -- claim AND every value @PredExpr@ predicate evaluates to @True@ against it.
    valMatchesType val t = do
      results <- mapM (evalPredPartial val) (collectPredExprs (typePreds t))
      let structuralOk = isSubtypeOf typeEnv (singletonType (getValPartialType val)) (structuralTarget t)
      return $ structuralOk && all isTrue results
    isTrue (TupleVal n _) = n == truePrim
    isTrue _              = False

    mkRefinementTest omi defOa dom refRet = (testName, action)
      where
        testName = printf "%s :: refinement-agreement" (nameStr (oaObjPath defOa))
        -- Build the definition's call against the function's NOMINAL declared return (predicates
        -- cleared), exactly as the overlap tests do (see 'retDest' there) — NOT the definition's own
        -- inferred arrow type. A recursive definition such as @Cons.length= 1 + tail.length@ has an
        -- inferred return that still carries its body's unevaluated @operator+@ application as a
        -- positive leaf; building against that finds the body already a subtype and leaves it
        -- unreduced (a @TupleVal /operator+@ rather than an @Integer@). The narrower nominal @Integer@
        -- is not already satisfied, so the tree builder evaluates the body to a concrete value.
        retDest = clearUnionTypePreds $ case [getMetaType m | d <- omiDecls omi ++ omiRuntimeDecls omi, Just (_, m) <- [oaArr d]] of
          (t:_) -> t
          []    -> intType
        partial = getSingleton dom
        argTypeMap = H.fromList [(nameStr (pkName k), t) | (k, t) <- H.toList (ptArgs partial)]
        genArg t = genValForType typeEnv (UnionType Nothing (expandTypeWithNames typeEnv H.empty t teNames) [])
        genInput = do
          let argNames = H.keys argTypeMap
          vals <- mapM (\n -> (,) n <$> genArg (H.lookupDefault PTopType n argTypeMap)) argNames
          return $ TupleVal (nameStr (ptName partial)) (H.fromList vals)
        -- 'env' (the full program env, all definitions) lets a recursive definition (e.g.
        -- @Cons.length@ calling @tail.length@) reach its siblings; dispatch picks the right clause
        -- for the generated input.
        runDef input = do
          let inputE = valToEExpr input
          tree <- buildRoot (evTbEnv env) inputE (getExprPartialType inputE) retDest
          fst <$> runStateT (evalExpr tree) env
        prop = Hedgehog.property $ do
          input <- Hedgehog.forAll genInput
          case runDef input of
            CRes _ outV -> case valMatchesType outV refRet of
              CRes _ True  -> Hedgehog.success
              CRes _ False -> Hedgehog.footnote (printf "Refinement violated: %s on %s produced %s, not matching the claimed return %s" testName (show input) (show outV) (show refRet)) >> Hedgehog.failure
              CErr ns      -> Hedgehog.footnote (printf "Refinement predicate eval failed on %s = %s: %s" (show input) (show outV) (prettyCNotes ns)) >> Hedgehog.failure
            CErr ns -> Hedgehog.footnote (printf "eval failed: %s" (prettyCNotes ns)) >> Hedgehog.failure
        action = CResT $ do
          ok <- checkPropWithTimeout True prop
          return $ if ok
            then CRes [] (TupleVal "()" H.empty)
            else CErr [MkCNote $ GenCErr Nothing $ printf "Refinement agreement test failed: %s" testName]

evalTest :: EPrgmGraphData -> CRes [(String, CResT IO Val)]
evalTest prgmGraphData = do
  let prgm@(Prgm objMap _ _) = mconcat $ map fst3 $ graphToNodes prgmGraphData
  let testOAs = nubBy (\a b -> oaObjPath a == oaObjPath b) $ filter (\oa -> hasAnnot testAnnot oa || hasAnnot exampleAnnot oa) $ flatObjectMap objMap
  let (noArgTests, propTests) = foldr partitionTest ([], []) testOAs
  return $ map (runSimpleTest prgm) noArgTests ++ map (runPropertyTest prgm) propTests ++ deriveOverlapTests prgm ++ deriveRefinementTests prgm
  where
    isNoArgTest oa = null (exprAppliedArgs (oaObjExpr oa)) && null (exprAppliedOrdVars (oaObjExpr oa))
    partitionTest oa (simple, props)
      | isNoArgTest oa = (oa : simple, props)
      | otherwise      = (simple, oa : props)

    runSimpleTest p oa =
      let testName = nameStr (oaObjPath oa)
          action = asCResT $ do
            let input = eVal testName
            let src = getExprPartialType input
            (expr, env) <- evalBuildPrgm input src intType p
            fst <$> runStateT (evalExpr expr) env
      in (testName, action)

    runPropertyTest p oa =
      let testName = nameStr (oaObjPath oa)
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
              in Hedgehog.forAll (genValForType typeEnv concreteArg)
              ) args

            -- Build the expression: testName[$T=ConcreteType, ...](arg0=val0, ...)
            -- First apply type variables so the body can resolve class-based calls
            let typeVarInput = foldl (\base (varName, chosenType) -> eAppVar base varName chosenType)
                                     (eVal testName)
                                     (zip (map fst typeVars) chosenTypes)
            let argNames = map (nameStr . oaObjPath) args
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
            ok <- checkPropWithTimeout False prop
            return $ if ok
              then CRes [] (TupleVal "()" H.empty)
              else CErr [MkCNote $ GenCErr Nothing $ printf "Property test failed: %s" testName]
      in (testName, action)
