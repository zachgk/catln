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
import qualified Data.HashSet               as S
import           Semantics.Prgm
import           Semantics.Types

import           Control.Concurrent         (forkIO, killThread, newEmptyMVar,
                                             putMVar, takeMVar)
import           Control.Monad              (forM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State        (lift, runStateT)
import           CtConstants
import           Data.IORef                 (IORef, modifyIORef', newIORef,
                                             readIORef)
import           Data.List                  (intercalate, nub, nubBy, sortOn,
                                             tails)
import           Data.Maybe
import           Data.UUID                  (UUID)
import           Eval                       (checkPrintGolden, evalBuildPrgm,
                                             evalDShowAnnot, evalEnv, evalExpr,
                                             evalValEq, printPArg, printRange,
                                             printResultArg, valWithinType)
import           Eval.Common
import           Eval.ExprBuilder
import qualified Hedgehog
import qualified Hedgehog.Gen               as HG
import           Hedgehog.Internal.Property (propertyConfig, propertyTest)
import           Hedgehog.Internal.Report   (Result (..), reportStatus)
import qualified Hedgehog.Internal.Report
import           Hedgehog.Internal.Runner   (checkReport)
import qualified Hedgehog.Internal.Seed     as HedgehogSeed
import           Semantics
import           Semantics.Annots           (getRuntimeAnnot, hasAnnot,
                                             hasElseAnnot, isAnnot)
import           System.Timeout             (timeout)
import           Testing.Generation         (genValForType)
import           Text.Printf
import           TreeBuild
import           Utils

-- | Iteration count for derived property tests (the Hedgehog default).
propTestIterations :: Hedgehog.TestLimit
propTestIterations = 100

-- | Per-property wall-clock timeout, in microseconds (10 seconds).
propTestTimeoutMicros :: Int
propTestTimeoutMicros = 10 * 1000 * 1000

-- | What running a Hedgehog property found: whether it passed, the input of each generated case it
-- got through, and, for a genuine failure, the rendered counterexample.
--
-- The render is carried rather than printed so that whoever asked for the run decides what becomes
-- of it — a command line prints it, a doc server shows it in the page.
data PropOutcome = PropOutcome
  { poPassed :: Bool
  , poCases  :: Int
    -- | The input of each of those cases, in the order they ran. Counted separately from 'poCases',
    -- which is what the engine itself reported running, so that the two can be compared rather than
    -- one being taken on the other's word.
  , poInputs :: [String]
  , poReport :: Maybe String
  }

-- | Records the input one generated case is about to run on. A property is written as a function of
-- this so that a run reports what it covered and not merely how much of it there was.
type RecordCase = String -> Hedgehog.PropertyT IO ()

-- | Run a Hedgehog property capped at both 'propTestIterations' variations and a
-- 'propTestTimeoutMicros' wall-clock timeout. On timeout the property is judged on the variations
-- that completed within the window: the runner returns a failure as soon as it finds a failing case,
-- so if the timeout elapses before a result is produced, no counterexample was found among the
-- completed variations and it counts as a pass.
--
-- The check runs in a worker thread while the timeout is applied to the main thread's wait
-- for its result. On timeout the worker is killed; the 'ThreadKilled' delivered to it is
-- caught inside Hedgehog's 'checkReport', which turns it into a spurious 'Failed' report.
-- The worker therefore only /produces/ the outcome and the main thread decides whether to keep it —
-- so a rendered failure is kept only when the main thread actually received a result, which never
-- happens once the timeout has fired. Nothing may report on the run except through this return
-- value; a worker writing its findings out of band would resurrect exactly that spurious failure.
--
-- 'gaveUpOk' controls how an all-discards run (Hedgehog 'GaveUp') is judged: 'False' treats it as a
-- failure (the default for tests whose inputs should always generate), 'True' as a pass. The derived
-- refinement tests pass 'True' because a pairing whose overlap cannot yet be generated (e.g. a
-- @Cons@ value, which the generator discards) has simply found no counterexample.
checkPropWithTimeout :: Bool -> (RecordCase -> Hedgehog.Property) -> IO PropOutcome
checkPropWithTimeout gaveUpOk mkProp = do
  casesRef <- newIORef []
  let record input = liftIO $ modifyIORef' casesRef (input :)
  resultVar <- newEmptyMVar
  tid <- forkIO $ runPropQuietly gaveUpOk casesRef (Hedgehog.withTests propTestIterations (mkProp record)) >>= putMVar resultVar
  result <- timeout propTestTimeoutMicros (takeMVar resultVar)
  case result of
    Just outcome -> return outcome
    -- The cases the killed worker had got through are not reported: how many of them there were
    -- lives in its report, which is exactly what a timeout kill corrupts, and the inputs it recorded
    -- only mean anything alongside that count.
    Nothing      -> killThread tid >> return (PropOutcome True 0 [] Nothing)

-- | Run a Hedgehog property to completion without rendering any progress or result output,
-- returning what it found. A no-op progress callback suppresses the live progress render.
--
-- The inputs the property recorded are read back from 'casesRef' and cut to the number of cases the
-- report counted. That cut is what separates the cases from the shrinking: Hedgehog forces one tree
-- root per case, so a passing run records exactly one input per case, in order; a failing run then
-- replays the body once per shrink candidate, and every one of those replays comes after the case
-- that failed. Since a failure is reported as @tests + 1@, the counted cases are exactly the recorded
-- inputs up to and including the one it failed on.
runPropQuietly :: Bool -> IORef [String] -> Hedgehog.Property -> IO PropOutcome
runPropQuietly gaveUpOk casesRef prop = do
  seed <- HedgehogSeed.random
  report <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) (const (pure ()))
  recorded <- reverse <$> readIORef casesRef
  let cases = fromIntegral $ Hedgehog.Internal.Report.reportTests report
  let inputs = take cases recorded
  return $ case reportStatus report of
    OK              -> PropOutcome True cases inputs Nothing
    GaveUp          -> PropOutcome gaveUpOk cases inputs Nothing
    Failed failure' -> PropOutcome False cases inputs (Just $ renderFailure failure')

-- | What a failed property found: the inputs the engine generated, and what the failure reported.
--
-- Hedgehog renders a failure by quoting the source around whatever line called it — which is THIS
-- module, the compiler's own Haskell. A hundred lines of 'runPropertyTest' is not what failed; the
-- Catln test is, and a report of it that points at the compiler reads as the compiler being at
-- fault. So the report is assembled from what the engine actually found instead.
renderFailure :: Hedgehog.Internal.Report.FailureReport -> String
renderFailure failure' = intercalate "\n" $ filter (not . null) $ generated ++ footnotes ++ [message]
  where
    generated = case map Hedgehog.Internal.Report.failedValue (Hedgehog.Internal.Report.failureAnnotations failure') of
      []     -> []
      inputs -> ["Generated " ++ intercalate ", " inputs]
    footnotes = Hedgehog.Internal.Report.failureFootnotes failure'
    message = Hedgehog.Internal.Report.failureMessage failure'

-- | A name that names something local — an argument, a type variable — as the program wrote it. The
-- compiler stores every name absolute, so an argument written @a@ is stored @\/a@ and a type
-- variable written @$T@ is stored @\/T@; neither spelling appears in any program, and @\/T@ reads as
-- a module-level name rather than as the variable it is.
showLocalName :: String -> String
showLocalName n = case getPath n of
  NPath True [seg] -> seg
  _                -> n

-- | Render a generated value as the input a case ran on, in the surface form the value was written
-- in: @/Data/Maybe/Some(value= 1)@.
--
-- 'Show' is not reused for this because it names the interpreter's own constructor (@TupleVal@),
-- which tells a reader of the docs nothing about their test. Arguments are ordered by name rather
-- than left in the map's hash order, so the same case reads the same way on every run.
showTestInput :: Val -> String
showTestInput (TupleVal name args)
  | H.null args = name
  | otherwise = printf "%s(%s)" name (intercalate ", " [printf "%s= %s" (showLocalName k) (showTestInput a) :: String | (k, a) <- sortOn fst (H.toList args)])
showTestInput v = show v

-- | How a test decides whether it passed. A test written in the program is an evaluation that must
-- succeed; a test the engine generates inputs for is a property. They are distinguished rather than
-- collapsed into one because only a property has a number of cases to report, and only an evaluation
-- reports its failure as compiler notes.
data TestCheck
  = EvalCheck (CResT IO Val)
  | PropCheck (IO PropOutcome)
  -- | A check decided without running the program, such as a proof by the type checker.
  | StaticCheck (CRes ())

-- | A test derived from a program, before it is run. 'evalTest' derives these; 'runTestDesc' turns
-- one into the 'TestResult' that is cached, served and displayed.
data TestDesc = TestDesc
  { tdName    :: String
  , tdOwner   :: String
  , tdOwnerId :: UUID
  , tdKind    :: TestKind
  , tdCheck   :: TestCheck
  }

-- | Run one test to its result. A failing test is a result rather than an error, so a run of many
-- tests reports all of them instead of stopping at the first.
runTestDesc :: TestDesc -> IO TestResult
runTestDesc TestDesc{tdName, tdOwner, tdOwnerId, tdKind, tdCheck} = finish <$> case tdCheck of
  EvalCheck action -> ofEval <$> runCResT action
  PropCheck run    -> ofProp <$> run
  StaticCheck res  -> return $ ofEval res
  where
    finish (passed, cases, inputs, notes) = TestResult tdName tdOwner tdOwnerId tdKind passed cases inputs notes
    -- An evaluation test is run once on the program as written, so it has no generated case to count
    -- and no input to report.
    ofEval (CRes _ _)   = (True, Nothing, [], Nothing)
    ofEval (CErr notes) = (False, Nothing, [], Just $ prettyCNotes notes)
    ofProp PropOutcome{poPassed, poCases, poInputs, poReport}
      | poPassed = (True, Just poCases, poInputs, Nothing)
      -- A property fails either with a counterexample, which the report renders, or by discarding
      -- every case it generated, which has no counterexample to render but is still a failure.
      | otherwise = (False, Just poCases, poInputs, Just $ fromMaybe gaveUp poReport)
    gaveUp = "The test engine could not generate any usable input for this test"

-- | The program as the derived tests decide which arms overlap: as written, every class closed to
-- its declared members, which is how dispatch reads it ('TreeBuild'). The tests run the program, so
-- the arms that overlap are the arms dispatch could choose between. Read open-world, as inference
-- reads it, an arm for a class (@:Ordering.rel@, @dshow(s -> DShow)@) overlaps every arm whose input
-- might yet join that class, which no run of this program can choose between.
runTypeEnv :: EPrgm -> TypeEnv (ObjArrTypeGraph Expr EvalMetaDat)
runTypeEnv prgm = (mkTypeEnv prgm){tePrgmEnv = True}

-- | A clause the derived tests can evaluate: a definition with a body, or a runtime primitive
-- (evaluated through its @#runtime@ implementation).
evaluableClause :: EObjArr -> Bool
evaluableClause oa = case oaArr oa of
  Just (Just _, _)  -> True
  Just (Nothing, _) -> isJust (getRuntimeAnnot (oaAnnots oa))
  _                 -> False

-- | The program's environment with some clauses of one function removed, so a normal call dispatches
-- among the rest — which is how one clause of several is evaluated in isolation. Removing (rather
-- than keeping only one) preserves a runtime-primitive sibling, which has no body to keep.
envWithoutClauses :: EPrgm -> TypeName -> [EObjArr] -> Env
envWithoutClauses prgm@(Prgm objMap@(ObjectMap om) _ _) name dropOas = evalEnv defaultEvalSession prgm{prgmObjMap = objMap{omItems = H.adjust without name om}}
  where
    without o = o{ omiDefs = filter (`notElem` dropOas) (omiDefs o)
                 , omiRuntimeDecls = filter (`notElem` dropOas) (omiRuntimeDecls o) }

-- | Build and evaluate a call of a value in an environment, against a destination type. Building the
-- full call (rather than a body alone) resolves the body's nested operators and primitives against a
-- concrete result type.
runCallIn :: Env -> Type -> Val -> CResT IO Val
runCallIn env dest input = do
  let inputE = valToEExpr input
  tree <- asCResT $ buildRoot (evTbEnv env) inputE (getExprPartialType inputE) dest
  fst <$> runStateT (evalExpr tree) env

-- | Derive a property test for every pair of evaluable same-name definitions whose domains
-- overlap (their argument types intersect): on the overlap the two definitions must AGREE. A
-- non-linear "diagonal" definition (e.g. @rel(a@x, b@x)= True@) overlaps every full definition it
-- shadows, so this turns the diagonal into a check that each full definition matches it where they
-- meet — for @Eqv.rel@ that overlap is exactly reflexivity. Each definition is evaluated in
-- isolation (bypassing dispatch, which would only ever run one of them) and the results compared
-- through their 'Eq'. Inputs are generated in the overlap, with positions that a non-linear
-- pattern ties together generated equal so the diagonal is actually exercised.
--
-- Derived for the functions @own@ holds an item of, over every arm of them @prgm@ holds; @own@ is
-- @prgm@ itself when the whole program's tests are asked for.
deriveOverlapTests :: EPrgm -> EPrgm -> [TestDesc]
deriveOverlapTests (Prgm (ObjectMap ownOm) _ _) prgm@(Prgm (ObjectMap om) _ _) =
  [ mkOverlapTest omi oaA oaB dom
  | (name, _) <- H.toList ownOm
  , Just omi <- [H.lookup name om]
  -- Ordered by where the arms were written, so which of a pair is @oaA@ — and so which page the
  -- test belongs to — is the same on every run. The object map's own order is a hash order, which
  -- would move a cross-file agreement test between the two files it pairs.
  -- An existential name's arms are alternatives which may give different results, so only a unique
  -- name's arms are tested for agreement.
  , omiQuant omi == QuantUnique
  , let parts = sortOn (getMetaPos . getExprMeta . oaObjExpr) $ filter evaluable (omiDefs omi ++ omiRuntimeDecls omi)
  , (oaA, oaB) <- pairs parts
  , let dom = intersectTypes runEnv (clauseDom oaA) (clauseDom oaB)
  , not (isBottomType dom)
  , isJust (maybeGetSingleton dom)
  -- Only overlapping definitions that can actually agree are testable: their DECLARED result types
  -- must overlap too. Overloads that share an input shape but are declared to produce different
  -- result types are not meant to agree, so no agreement test is derived for them. (Arms whose
  -- results do overlap but are alternatives, such as 'bindap''s 'fmap' and 'mmap' arms, say so with
  -- @#exist@ instead.)
  , not (isBottomType (intersectTypes runEnv (clauseCod omi oaA) (clauseCod omi oaB)))
  -- A clause chosen by a condition rather than by its domain is not an overlapping definition. Two
  -- guarded arms of one function are the branches of a conditional — @greet(…) | formal= …@ beside
  -- @greet(…) else = …@ — and branches are meant to differ, which is the whole of what a branch is
  -- for. Their domains are nevertheless identical, so without this every conditional written with
  -- guards derives a test asserting that it does not branch.
  --
  -- Excluded rather than generated more narrowly. Inputs here are generated from the domain
  -- intersection alone, and a pair of guarded clauses would have to be generated in the region
  -- where BOTH conditions hold — which is not a type and so is not something 'genValForType' can be
  -- asked for. Where that region is empty there is nothing to agree about in the first place.
  , conditionFree oaA && conditionFree oaB
  -- An @else@ arm applies only where no other arm does (as 'TreeBuild' dispatches it), so the input
  -- it is chosen for is its domain less its siblings' and overlaps none of them. Its written domain
  -- still intersects theirs — @llmText(content= Some(Str(s)))@ beside @llmText(content) else@ — so
  -- without this every function with a fallback arm derives a test asserting that the fallback
  -- agrees with the arms it falls back from.
  , not (hasElseAnnot oaA) && not (hasElseAnnot oaB)
  ]
  where
    TypeEnv{teNames} = typeEnv
    typeEnv = mkTypeEnv prgm
    runEnv = runTypeEnv prgm
    -- Pairing a non-linear diagonal against a primitive sibling is how @(_@x - _@x)= 0@ becomes a
    -- derived test of the built-in subtraction (@x - x == 0@).
    evaluable = evaluableClause
    -- A clause with no guard on it, which is the only kind whose domain is the whole of what
    -- selects it. 'TreeBuild.findResArrows' reads the same 'exprWhereConds' to decide which arrows
    -- it may pick without evaluating a condition first.
    conditionFree oa = null (exprWhereConds (oaObjExpr oa))
    clauseDom oa = getExprType (oaObjExpr oa)
    -- The clause's declared result (codomain) type: what the function is declared to return for the
    -- clause's input ('declaredReturns', the clause answering for itself where nothing declares the
    -- function), read over each object the clause accepts. It is NOT the body's type, which is the
    -- body as written: a bare argument's is the reference to that argument, and two constants or two
    -- different calls are disjoint, so reading it hid disagreeing arms of every such shape.
    clauseCod omi oa = unionAllTypes typeEnv
      [ dest
      | UnionType Nothing leafs [] <- [clauseDom oa]
      , src <- splitUnionType leafs
      , dest <- declaredReturns omi [oa] src ]
    pairs xs = [(a, b) | (a:rest) <- tails xs, b <- rest]
    -- What a function is declared to return for an input: the results of its declarations which
    -- accept it, or — when none does — of the given arms, for a function written only as definitions
    -- is declared by them, each giving its own return (@mdbl(v -> Integer) -> Integer= v@). A runtime
    -- primitive is a declaration too, its declared return its own.
    declaredReturns omi arms src = case dests (omiDecls omi ++ omiRuntimeDecls omi) of
      []    -> dests arms
      found -> found
      where
        dests oas = [dest | d@ObjArr{oaArr=Just _} <- oas, dest <- arrowDestType typeEnv H.empty src d]

    mkOverlapTest omi oaA oaB dom = TestDesc testName (nameStr (oaObjPath oaA)) (oaTestId oaA) TestOverlapAgreement (PropCheck (checkPropWithTimeout False prop))
      where
        -- The function's declared return type (predicates cleared) is the destination to build each
        -- call against. It is narrower than a clause's inferred result, so the call's own type is not
        -- already a subtype of it — which forces the tree builder to actually build and evaluate the
        -- body to a concrete value rather than leaving it as an unevaluated application.
        --
        -- The return is read as the declaration gives it FOR THE OVERLAP'S INPUT, not as written: a
        -- generic declaration (@(_ :: $T - _ :: $T)[$T -> Ring] -> $T@) returns its variable, which
        -- admits the unevaluated application itself, so both clauses would come back unbuilt. Only
        -- declarations that accept the overlap answer (a generic one beside the runtime
        -- primitives it covers is not necessarily first).
        --
        -- A function no declaration accepts the overlap of is declared by its arms, so their own
        -- returns are the destination ('declaredReturns').
        retDest = clearUnionTypePreds $ case declaredReturns omi [oaA, oaB] partial of
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
        -- build and evaluate that call.
        envA = envWithoutClauses prgm (oaObjPath oaA) [oaB]
        envB = envWithoutClauses prgm (oaObjPath oaA) [oaA]
        runClause env input = runCResT $ runCallIn env retDest input
        prop :: RecordCase -> Hedgehog.Property
        prop record = Hedgehog.property $ do
          input <- Hedgehog.forAll genInput
          record (showTestInput input)
          resA <- liftIO $ runClause envA input
          resB <- liftIO $ runClause envB input
          case (resA, resB) of
            (CRes _ va, CRes _ vb) -> do
              eqRes <- liftIO $ runCResT $ runStateT (evalValEq va vb) envA
              case eqRes of
                CRes _ (True, _)  -> Hedgehog.success
                CRes _ (False, _) -> Hedgehog.footnote (printf "Overlapping definitions disagree on %s:\n  %s\n  %s" (show input) (show va) (show vb)) >> Hedgehog.failure
                CErr ns           -> Hedgehog.footnote (printf "Eq compare of results failed (%s vs %s): %s" (show va) (show vb) (prettyCNotes ns)) >> Hedgehog.failure
            (ra, rb) -> Hedgehog.footnote (printf "eval failed:\n  %s\n  %s" (errOf ra) (errOf rb)) >> Hedgehog.failure
        errOf (CErr ns) = prettyCNotes ns
        errOf _         = "ok"

-- | Derive a property test for every refinement declaration paired with an overlapping definition
-- of the same object. A refinement declaration (a @#refine@ arrow) claims a narrowed return type;
-- where its domain overlaps a definition's, that definition's result on inputs in the overlap must
-- match the claim. We generate an input in the overlap, evaluate it through the DEFINITION to a
-- concrete value, and assert that value matches the refinement's claimed return type (see
-- 'valMatchesType'). For @:ConsList.length -> Integer_nonNeg #refine@ paired with @Nil.length= 0@
-- this checks that the length of a @Nil@ is a non-negative integer.
--
-- How the claim is checked follows the function's quantification ('omiQuant'). A unique function's
-- arms must each satisfy it, so there is one test per overlapping definition. An existential
-- function's arms are alternatives, so there is one test per refinement, and an input passes when
-- SOME arm accepting it gives a result matching the claim (see 'mkExistRefinementTest').
--
-- Derived for the functions @own@ holds an item of, as 'deriveOverlapTests' is.
deriveRefinementTests :: EPrgm -> EPrgm -> [TestDesc]
deriveRefinementTests (Prgm (ObjectMap ownOm) _ _) prgm@(Prgm (ObjectMap om) _ _) = concat
  [ case omiQuant omi of
      QuantUnique -> [mkRefinementTest omi defOa dom refRet | (defOa, dom) <- arms]
      QuantExist  -> [mkExistRefinementTest name omi refOa (map snd arms) refRet | not (null arms)]
  | (name, _) <- H.toList ownOm
  , Just omi <- [H.lookup name om]
  -- An @#invalid@ refinement claims more than its definitions keep, so testing the claim would
  -- only find that out again.
  , refOa <- filter (not . isInvalidDecl) (omiRefineDecls omi)
  -- A refinement to prove is checked by its proof ('TypeCheck.Proof') instead.
  , not (oaHasAnnotRel proofAnnot refOa)
  , Just (_, refM) <- [oaArr refOa]
  , let refRet = getMetaType refM
  , let arms = [ (defOa, dom)
               | defOa <- filter evaluableClause (omiDefs omi)
               , let dom = intersectTypes runEnv (clauseDom refOa) (clauseDom defOa)
               , not (isBottomType dom)
               , isJust (maybeGetSingleton dom)
               ]
  ]
  where
    TypeEnv{teNames} = typeEnv
    typeEnv = mkTypeEnv prgm
    runEnv = runTypeEnv prgm
    env = evalEnv defaultEvalSession prgm
    clauseDom oa = getExprType (oaObjExpr oa)

    -- Build a definition's call against the function's NOMINAL declared return (predicates cleared),
    -- exactly as the overlap tests do (see 'retDest' there) — NOT the definition's own inferred arrow
    -- type. A recursive definition such as @Cons.length= 1 + tail.length@ has an inferred return that
    -- still carries its body's unevaluated @operator+@ application as a positive leaf; building
    -- against that finds the body already a subtype and leaves it unreduced (a @TupleVal /operator+@
    -- rather than an @Integer@). The narrower nominal @Integer@ is not already satisfied, so the tree
    -- builder evaluates the body to a concrete value.
    refineRetDest omi = clearUnionTypePreds $ case [getMetaType m | d <- omiDecls omi ++ omiRuntimeDecls omi, Just (_, m) <- [oaArr d]] of
      (t:_) -> t
      []    -> intType

    -- A generated input of the (singleton) overlap of a refinement and a definition.
    genRefineInput dom = do
      let partial = getSingleton dom
      let argTypeMap = H.fromList [(nameStr (pkName k), t) | (k, t) <- H.toList (ptArgs partial)]
      let genArg t = genValForType typeEnv (UnionType Nothing (expandTypeWithNames typeEnv H.empty t teNames) [])
      vals <- mapM (\n -> (,) n <$> genArg (H.lookupDefault PTopType n argTypeMap)) (H.keys argTypeMap)
      return $ TupleVal (nameStr (ptName partial)) (H.fromList vals)

    -- A refinement claim is stored as a type predicate on the declared return — a @PredExpr@ holding
    -- a symbolic application tree such as @operator==(r=True, l=nonNeg(this=$this))@ (the desugaring
    -- of @Integer_nonNeg@'s @$this.nonNeg == True@), with @$this@ standing for the value refined.
    -- Whether a value matches it is the question the runtime asks of a value held to a refinement
    -- ('Eval.valWithinType'): within the shape, and every predicate the type layer could not decide
    -- evaluating to @True@ on it.
    valMatchesType val t = fst <$> runStateT (valWithinType val t) env

    mkRefinementTest omi defOa dom refRet = TestDesc testName (nameStr (oaObjPath defOa)) (oaTestId defOa) TestRefinementAgreement (PropCheck (checkPropWithTimeout True prop))
      where
        testName = printf "%s :: refinement-agreement" (nameStr (oaObjPath defOa))
        retDest = refineRetDest omi
        genInput = genRefineInput dom
        -- 'env' (the full program env, all definitions) lets a recursive definition (e.g.
        -- @Cons.length@ calling @tail.length@) reach its siblings; dispatch picks the right clause
        -- for the generated input.
        runDef = runCallIn env retDest
        prop :: RecordCase -> Hedgehog.Property
        prop record = Hedgehog.property $ do
          input <- Hedgehog.forAll genInput
          record (showTestInput input)
          res <- liftIO $ runCResT $ runDef input
          case res of
            CRes _ outV -> do
              matches <- liftIO $ runCResT $ valMatchesType outV refRet
              case matches of
                CRes _ True  -> Hedgehog.success
                CRes _ False -> Hedgehog.footnote (printf "Refinement violated: %s on %s produced %s, not matching the claimed return %s" testName (show input) (show outV) (show refRet)) >> Hedgehog.failure
                CErr ns      -> Hedgehog.footnote (printf "Refinement predicate eval failed on %s = %s: %s" (show input) (show outV) (prettyCNotes ns)) >> Hedgehog.failure
            CErr ns -> Hedgehog.footnote (printf "eval failed: %s" (prettyCNotes ns)) >> Hedgehog.failure

    -- The refinement test of an EXISTENTIAL function: one per refinement, generating inputs from
    -- every overlap of it with a definition. An input passes when some arm accepting it, evaluated
    -- in isolation (the other accepting arms removed, as the overlap tests isolate a clause), gives a
    -- result matching the claim.
    --
    -- The isolation is of the outer call only: a recursive call inside the arm dispatches without the
    -- removed arms too. That can only miss a path some removed arm would have offered, so it may fail
    -- an input whose claim holds but never passes one whose claim does not.
    mkExistRefinementTest name omi refOa doms refRet = TestDesc testName (nameStr name) (oaTestId refOa) TestRefinementAgreement (PropCheck (checkPropWithTimeout True prop))
      where
        testName = printf "%s :: refinement-agreement" (nameStr name)
        retDest = refineRetDest omi
        accepting input = [ oa | oa <- filter evaluableClause (omiDefs omi ++ omiRuntimeDecls omi)
                               , isSubtypeOf runEnv (singletonType (getValPartialType input)) (clauseDom oa) ]
        runArm input arms oa = runCResT $ do
          outV <- runCallIn (envWithoutClauses prgm name (filter (/= oa) arms)) retDest input
          matches <- valMatchesType outV refRet
          return (outV, matches)
        showArm (CRes _ (outV, _)) = show outV
        showArm (CErr ns)          = printf "eval failed: %s" (prettyCNotes ns)
        prop :: RecordCase -> Hedgehog.Property
        prop record = Hedgehog.property $ do
          input <- Hedgehog.forAll (HG.choice (map genRefineInput doms))
          record (showTestInput input)
          let arms = accepting input
          results <- liftIO $ mapM (runArm input arms) arms
          if or [matches | CRes _ (_, matches) <- results]
            then Hedgehog.success
            else Hedgehog.footnote (printf "Refinement violated: no arm of %s on %s produced a result matching the claimed return %s. The arms produced:\n  %s" testName (show input) (show refRet) (intercalate "\n  " (map showArm results))) >> Hedgehog.failure

-- | The tests of a program, evaluated in the session given: every test of it, derived from the
-- whole of it, or with a page named the tests that page wrote. Those are derived from what that page
-- declares, and decided in the whole program the graph holds: the same tests the whole program would
-- attribute to the page, at the cost of that page alone — deriving over the whole program evaluates
-- every print of every page it reaches, which for a page importing the standard library is the
-- library's own tests again.
--
-- The whole program is still the environment, so a law the page states over a class is checked
-- against the instances other pages define, and an agreement test pairs the page's arm with an arm
-- of the same function written elsewhere; such a test belongs to the page whose arm was written
-- first, as 'deriveOverlapTests' places it.
evalTest :: EvalSession -> Maybe FileImport -> EPrgmGraphData -> CResT IO [TestDesc]
evalTest session Nothing prgmGraphData = evalTestWith session prgm prgm
  where prgm = mconcat $ map fst3 $ graphToNodes prgmGraphData
evalTest session (Just name) prgmGraphData = case graphLookup name prgmGraphData of
  Nothing  -> CResT $ return $ CErr [MkCNote $ GenCErr Nothing $ printf "Could not find the page %s to derive the tests of" (show $ impRaw name)]
  Just own -> evalTestWith session own (mconcat $ map fst3 $ graphToNodes prgmGraphData)

-- | The tests derived from the definitions and prints of @own@, evaluated in @prgm@, which holds
-- @own@ and everything it is decided against, in the session given: the pages the program was read
-- from, and what the tests it writes run with.
--
-- The tests derived from the program's own arms -- agreement and refinement tests -- evaluate an arm
-- in isolation ('envWithoutClauses') rather than as the program wrote it, and are not run in it.
evalTestWith :: EvalSession -> EPrgm -> EPrgm -> CResT IO [TestDesc]
evalTestWith session own@(Prgm ownMap _ ownAnnots) prgm = do
  let testOAs = nubBy (\a b -> oaObjPath a == oaObjPath b) $ filter (\oa -> hasAnnot testAnnot oa || hasAnnot exampleAnnot oa) $ flatObjectMap ownMap
  let (noArgTests, propTests) = foldr partitionTest ([], []) testOAs
  printTests <- lift $ derivePrintTests session own prgm
  return $ map (runSimpleTest prgm) noArgTests ++ map (runPropertyTest prgm) propTests ++ filter owned (deriveOverlapTests own prgm ++ deriveRefinementTests own prgm) ++ printTests
  where
    -- A derived test is the page's when the definition it is placed with is. Arms of the page's
    -- functions written in other pages are paired too, and the tests placed with those arms are
    -- those pages' to run.
    ownIds = S.fromList $ map oaTestId (flatObjectMap ownMap) ++ map (getMetaId . getExprMeta) ownAnnots
    owned d = S.member (tdOwnerId d) ownIds
    isNoArgTest oa = null (exprAppliedArgs (oaObjExpr oa)) && null (exprAppliedOrdVars (oaObjExpr oa))
    partitionTest oa (simple, props)
      | isNoArgTest oa = (oa : simple, props)
      | otherwise      = (simple, oa : props)

    runSimpleTest p oa =
      let testName = nameStr (oaObjPath oa)
          action = do
            let input = eVal testName
            let src = getExprPartialType input
            (expr, env) <- asCResT $ evalBuildPrgm session input src intType p
            fst <$> runStateT (evalExpr expr) env
      in TestDesc testName testName (oaTestId oa) TestSimple (EvalCheck action)

    runPropertyTest p oa =
      let testName = nameStr (oaObjPath oa)
          typeEnv  = mkTypeEnv p
          typeVars = exprAppliedOrdVars (oaObjExpr oa)
          args     = exprAppliedArgs (oaObjExpr oa)

          TypeEnv{teNames} = typeEnv

          -- Resolve a type variable's constraint to a list of concrete PartialTypes. An OPEN class
          -- with no in-scope members expands to a single 'classPlaceholder' (a satisfiability
          -- device, not a real instance); drop it so the var reads as having no concretes and the
          -- property discards (a law over a class with no instances is vacuously satisfied — its
          -- instances, if any, are checked in the files that define them).
          resolveVar (_varName, varMeta) =
            filter (not . isClassPlaceholder) $ splitUnionType $ expandTypeWithNames typeEnv H.empty (getMetaType varMeta) teNames
          isClassPlaceholder pt = nameStr (ptName pt) == classPlaceholderStr

          concreteTypesPerVar = map resolveVar typeVars

          -- The type-parameter bindings a class records for a chosen member instance. A class node
          -- stores each member with the parameters that member fixes (e.g. @FloatOrdering@ under
          -- @Ordering@ carries @$T -> Float@), so this recovers a chosen instance's element type —
          -- which the flattened class expansion drops.
          classNodes = let ClassGraph cg = teClassGraph typeEnv in graphToNodes cg
          instanceVarBindings instName = H.unions
            [ ptVars mp
            | (CGClass _ _ ts _, _, _) <- classNodes
            , mt <- ts
            , mp <- case mt of UnionType Nothing ls [] -> splitUnionType ls; _ -> []
            , ptName mp == instName ]

          -- The call a case ran on, written the way it would be called: the chosen type arguments
          -- and the generated value arguments. Rendered from the pieces rather than from the built
          -- expression below, whose 'Show' spells out every arrow's metadata. Value arguments are
          -- ordered by name, like 'showTestInput' orders them: 'exprAppliedArgs' returns them in the
          -- order they were applied, which is the reverse of the order they were written in.
          showCase chosenTypes argNames argVals = printf "%s%s%s" testName typeArgs valArgs
            where
              typeArgs = concat [printf "[$%s= %s]" (showLocalName (nameStr (pkName vn))) (show ct) :: String | (vn, ct) <- zip (map fst typeVars) chosenTypes]
              valArgs | null argVals = ""
                      | otherwise = printf "(%s)" (intercalate ", " [printf "%s= %s" (showLocalName n) (showTestInput v) :: String | (n, v) <- sortOn fst (zip argNames argVals)]) :: String

          prop :: RecordCase -> Hedgehog.Property
          prop record = Hedgehog.property $ do
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
                      TypeVar (TVVar vn) _ i -> case H.lookup vn nestedTypeMap of
                        Just t  -> intersectTypes typeEnv t i
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

            -- After the last generator, so that a case the generators discard records nothing.
            record (showCase chosenTypes argNames argVals)

            case evalBuildPrgm session input src intType p of
              CErr notes -> Hedgehog.footnote (prettyCNotes notes) >> Hedgehog.failure
              CRes _ (expr, env) -> do
                res <- liftIO $ runCResT $ runStateT (evalExpr expr) env
                case res of
                  CErr notes -> Hedgehog.footnote (prettyCNotes notes) >> Hedgehog.failure
                  CRes _ _   -> Hedgehog.success

          -- A type variable with no concrete instances (an open class not yet inhabited in this
          -- file) makes the law vacuously true: there is nothing to quantify over, so pass without
          -- running the property (which would otherwise discard every case and report GaveUp). The
          -- law's real instances are checked in the files that define them. It passes with no cases
          -- run, which is what says it was vacuous rather than checked.
          check
            | any null concreteTypesPerVar = return $ PropOutcome True 0 [] Nothing
            | otherwise = checkPropWithTimeout False prop
      in TestDesc testName testName (oaTestId oa) TestProperty (PropCheck check)

-- | All @#print@ annotations reachable in the program, each paired with the type-context (@os@)
-- under which it must be built (mirrors the per-oa handling in 'Eval.evalAnnots').
collectPrintAnnots :: EPrgm -> [([(PartialType, EObjArr)], EExpr)]
collectPrintAnnots (Prgm objMap _ globalAnnots) =
  [ ([], a) | a <- globalAnnots, isAnnot printAnnot a ]
  ++ [ ([(getExprPartialType $ oaObjExpr oa, oa)], a)
     | oa <- flatObjectMap objMap, a <- oaAnnots oa, isAnnot printAnnot a ]

-- | Only @#print@ values that render (through @DShow@) to a 'StrVal' can have a string-serialized
-- golden. Returns the raw string for those, and 'Nothing' for any other DShow value (e.g. a
-- visualization), which is not golden-checkable.
printGolden :: Val -> Maybe String
printGolden (StrVal s) = Just s
printGolden _          = Nothing

-- | Build and evaluate every @#print@ reachable in the program to its rendered value, keeping
-- only the string-serializable ones, paired with the source range of the print's @p@ argument
-- (used by @catln test --update@ to locate and write goldens back).
evalPrints :: EvalSession -> EPrgm -> CResT IO [(CodeRangeDat, String)]
evalPrints session prgm = do
  let env = evalEnv session prgm
  results <- forM (collectPrintAnnots prgm) $ \(os, annot) -> do
    mval <- evalDShowAnnot env os annot
    return $ (,) <$> printRange annot <*> (mval >>= printGolden)
  return $ catMaybes results

-- | Derive a golden test for every string-serializable @#print@ reachable in the program. A
-- @#print@ whose value renders to a string is checked against its stored @#result@ golden; one
-- without a golden fails with a hint to run @catln test --update@. A @#print@ that renders to a
-- non-string DShow value is skipped entirely (produces no test).
--
-- Each print is evaluated here, as the tests are derived, rather than when its test is run: whether
-- a print has a test at all is decided by what it evaluates to.
--
-- The prints are those of @own@, evaluated in @prgm@ in the session given.
derivePrintTests :: EvalSession -> EPrgm -> EPrgm -> IO [TestDesc]
derivePrintTests session own prgm = catMaybes <$> mapM mkTest (collectPrintAnnots own)
  where
    env = evalEnv session prgm
    mkTest (os, annot) = do
      shown <- runCResT $ evalDShowAnnot env os annot
      return $ case shown of
        CRes _ (Just (StrVal actual)) -> Just (desc $ checkAction actual)
        CRes _ _                      -> Nothing
        CErr notes                    -> Just (desc $ CResT $ return $ CErr notes)
      where
        crange = printRange annot
        -- A print with no position of its own is named by what it prints, which is what tells it
        -- apart from the others.
        testName = printf "%s :: print-golden" (maybe (maybe "<print>" (\p -> "<print " ++ take 120 (show p) ++ ">") (printPArg annot)) showCodeRange crange)
        -- A print inside a definition belongs to that definition; one written at the top of a file
        -- belongs to the annotation itself, which is a statement of that file in its own right.
        (owner, ownerId) = case os of
          ((_, oa):_) -> (nameStr (oaObjPath oa), oaTestId oa)
          []          -> (testName, getMetaId (getExprMeta annot))
        desc = TestDesc testName owner ownerId TestPrintGolden . EvalCheck
        checkAction actual = asCResT $ case printResultArg annot of
          Nothing -> CErr [MkCNote $ GenCErr crange "Missing #result golden for print; run `catln test --update` to populate it"]
          Just _ -> TupleVal "()" H.empty <$ checkPrintGolden annot actual
