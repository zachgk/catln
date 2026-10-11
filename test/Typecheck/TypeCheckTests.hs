--------------------------------------------------------------------
-- |
-- Module    :  TypeCheck.TypeCheckTests
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This is the module for testing typechecking.
--------------------------------------------------------------------
module Typecheck.TypeCheckTests where
import           Common.TestCommon     (findErrCt)
import           Control.Monad.State   (evalStateT, execStateT, lift, runStateT)
import           CRes
import           CtConstants           (macroThisKey, outMacro, thisMacro)
import           CtService             (ctssBaseFiles, getTPrgm, getTPrgmJoined)
import           Data.Graph            (graphFromEdges)
import qualified Data.HashMap.Strict   as H
import qualified Data.HashSet          as S
import           Data.List             (isInfixOf, sort)
import           Hedgehog
import qualified Hedgehog.Gen          as HG
import           MapMeta
import           Semantics             (mkTypeEnv, testCtssConfig)
import           Semantics.Prgm
import           Semantics.Types
import           Test.Tasty
import qualified Test.Tasty.Hedgehog   as THG
import           Test.Tasty.HUnit      (Assertion, assertBool, assertEqual,
                                        assertFailure, testCase)
import           Testing.Generation    (genPrgms)
import           Text.Printf
import           TypeCheck             (typecheckPrgm)
import           TypeCheck.Common      (feCons, tcreToMaybe)
import           TypeCheck.Constrain   (executeConstraint)
import           TypeCheck.Decode      (toPrgms)
import           TypeCheck.Encode      (fromPrgms, makeBaseFEnv)
import           TypeCheck.Instruments (defaultInstruments)
import           Utils

propExprEncodeDecode :: Property
propExprEncodeDecode = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  annotate $ printf "Programs: \n\t%s" (show prgms)
  let prgm = mconcat prgms
  let fenv = makeBaseFEnv defaultInstruments prgm
  (encoded, fenv') <- evalMaybe $ tcreToMaybe $ runStateT (fromPrgms prgms []) fenv
  decoded <- evalMaybe $ tcreToMaybe $ evalStateT (toPrgms encoded) fenv'
  let decodedClear = map (mapMetaPrgm clearMetaDat) decoded
  annotate $ printf "Decoded to: \n\t%s" (show decoded)
  map prgmObjMap prgms === map prgmObjMap decodedClear

propConstraint :: Property
propConstraint = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let prgm = mconcat prgms
  let fenv = makeBaseFEnv defaultInstruments prgm
  fenv' <- evalMaybe $ tcreToMaybe $ execStateT (fromPrgms prgms []) fenv
  let cons = feCons fenv'
  con <- forAll $ HG.element cons
  let _fenv'' = execStateT (executeConstraint 0 con) fenv'
  return ()

propConstraints :: Property
propConstraints = property $ do
  prgmsNodes <- forAll genPrgms
  let prgms = map fst3 prgmsNodes
  let prgm = mconcat prgms
  let fenv = makeBaseFEnv defaultInstruments prgm
  fenv' <- evalMaybe $ tcreToMaybe $ execStateT (fromPrgms prgms []) fenv
  let cons = feCons fenv'
  usedCons <- forAll $ HG.subsequence (concat $ replicate 4 cons)
  let _fenv'' = execStateT (mapM (uncurry executeConstraint) (zip [0..] usedCons)) fenv'
  return ()

propTypeChecks :: Property
propTypeChecks = property $ do
  prgms <- forAll genPrgms
  let tprgm = typecheckPrgm $ graphFromEdges prgms
  case tprgm of
    CRes{}    -> return ()
    CErr errs -> fail $ prettyCNotes errs

-- | A page written to fail ('findErrCt') fails to type-check. What it fails with is not printed
-- ('Semantics.ctssQuietPage'), so a test asserting which errors those are is the place to read them.
testErrPage :: FilePath -> TestTree
testErrPage page = testCase page $ do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgm ctss
  case res of
    CErr _   -> return ()
    CRes _ _ -> assertFailure "the page is written to fail, and it type-checked"

-- | A definition whose body needs more of its input than its declaration promises is reported, and
-- its twin whose declaration states what the body needs is not. @unwrapBad@ returns a @v@ that may be
-- anything as a @Num@; @showBad@ hands what @fn@ returns, which may be anything, to @render@, which
-- needs a @Shows@. Both type-check; only the type checker's stronger question — is every form the
-- value may take guaranteed within the declaration — tells them from @unwrapOk@ and @showOk@.
-- Fixture: unit/resultNotGuaranteed.err.ct.
testResultNotGuaranteed :: TestTree
testResultNotGuaranteed = testCase "unsound arms are reported, their sound twins are not" $ do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig ["test/Typecheck/unit/resultNotGuaranteed.err.ct"]
    getTPrgm ctss
  let notes = case res of
        CRes ns _ -> ns
        CErr ns   -> ns
  let reported = [ n | n <- [show note | note <- notes], "Result Not Guaranteed" `isInfixOf` n ]
  let names = sort [ nm | nm <- ["unwrapBad", "unwrapOk", "showBad", "showOk"], any (("\"/Fx/" ++ nm ++ "\"") `isInfixOf`) reported ]
  assertEqual (unlines reported) ["showBad", "unwrapBad"] names

-- | An arm that binds no parameter of its own is within a declaration whose parameter is tied to
-- its arguments, and a refinement for a class in the parameter's bound is within it too. Both are
-- read from the declaration's spelled input, which the checks find in the solved type graph.
-- Fixture: unit/refineArgsParam.ct.
testRefineArgsParam :: TestTree
testRefineArgsParam = testCase "an arm and a refinement within an args-tied declaration" $ do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig ["test/Typecheck/unit/refineArgsParam.ct"]
    getTPrgm ctss
  let notes = case res of
        CRes ns _ -> ns
        CErr ns   -> ns
  let outside = [ n | n <- [show note | note <- notes], "Outside Declaration" `isInfixOf` n ]
  assertEqual (unlines outside) [] outside

-- | A recursive call passed as an argument is read as its declared result: @textNeg@'s guarded arm
-- passes @textNeg(..)@ to @++@, and inferred from the arms alone that argument is only a
-- @Semigroup@ (the arm's own @++@ is what it reaches), so the arm was reported as not guaranteed
-- to return the @String@ it is declared to. Fixture: unit/recursiveDeclaredArg.ct.
testRecursiveDeclaredArg :: TestTree
testRecursiveDeclaredArg = testCase "a recursive call argument is within its declared result" $ do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig ["test/Typecheck/unit/recursiveDeclaredArg.ct"]
    getTPrgm ctss
  let notes = case res of
        CRes ns _ -> ns
        CErr ns   -> ns
  let reported = [ n | n <- [show note | note <- notes], "Result Not Guaranteed" `isInfixOf` n, "\"textNeg\"" `isInfixOf` n ]
  assertEqual (unlines reported) [] reported

-- | A fold's result is within the declared result of the definition it is the body of: @total@
-- folds with @foldr@ into an @Integer@, and the lambda's parameters are typed from the call.
-- Fixture: unit/foldDeclaredResult.ct.
testFoldDeclaredResult :: TestTree
testFoldDeclaredResult = testCase "a fold into its declared result is guaranteed" $
  assertResultGuaranteed "test/Typecheck/unit/foldDeclaredResult.ct" "total"

-- | A fold into a declared CLASS result: the call's accumulator variable is chosen as the class.
testFoldClassResult :: TestTree
testFoldClassResult = testCase "a fold into its declared class result is guaranteed" $
  assertResultGuaranteed "test/Typecheck/unit/foldClassResult.ct" "anyNeg"

-- | A recursive call inside a @match@ arm: the generated arm's raw call is read with the values
-- its call site binds to the arm's inputs.
testMatchRecursiveCall :: TestTree
testMatchRecursiveCall = testCase "a recursive call in a match arm is guaranteed" $
  assertResultGuaranteed "test/Typecheck/unit/matchRecursiveCall.ct" "lookup"

-- | A higher-order argument's result spelled by a declaration variable the definition does not
-- apply: the variable ranges over the declaration's bound.
testDeclVarBound :: TestTree
testDeclVarBound = testCase "a declaration variable a definition does not apply keeps its bound" $
  assertResultGuaranteed "test/Typecheck/unit/declVarBound.ct" "walk"

-- | An instance written under a declaration's absolute name on another page, then called by a
-- later fragment: the call is still read with the declaration that shares the instance's key.
testSharedKeyDeclaration :: TestTree
testSharedKeyDeclaration = testCase "a declaration shared with a page's instance arms still bounds later calls" $
  assertResultGuaranteed "test/Typecheck/unit/sharedKey/use.ct" "filt"

-- | A type variable bounded by an intersection (@$F -> Box && Eq@) is within each part.
testIntersectionBound :: TestTree
testIntersectionBound = testCase "a type variable bounded by an intersection is within each part" $
  assertResultGuaranteed "test/Typecheck/unit/intersectionBound.ct" "f"

-- | A definition's argument shadows a like-named function in another module: the check reads the
-- argument, not the function's arrows.
testParamShadowsFunction :: TestTree
testParamShadowsFunction = testCase "an argument shadows a like-named function" $
  assertResultGuaranteed "test/Typecheck/unit/paramShadowsFunction.ct" "step"

-- | An arm whose body is a hole produces no value, so it is within its declaration.
testHoleArm :: TestTree
testHoleArm = testCase "an arm ending in a hole is within its declaration" $
  assertResultGuaranteed "test/Typecheck/unit/holeArm.ct" "f"

-- | A fold whose step is an operator declared on its class (@+@ on @Add@) returns what the call
-- chose its accumulator to be, an empty list's fold included. Fixture: unit/foldClassOp.ct.
testFoldClassOp :: TestTree
testFoldClassOp = testCase "a fold through a class-declared operator is within its declaration" $ do
  assertResultGuaranteed "test/Typecheck/unit/foldClassOp.ct" "/Fx/sum"
  assertResultGuaranteed "test/Typecheck/unit/foldClassOp.ct" "/Fx/sumNil"

-- | A call is read at the intersection of every refinement covering it, and a refinement covering
-- only part of a call says nothing about it. @tri@ is a primitive, so only its declaration and
-- refinements say what it returns. @onZ@ calls @tri(n= Z)@, which both refinements cover (@-> AB@
-- and @-> BC@), so what it settles to is within the @B@ they share; @onNat@ calls @tri@ on every
-- @Nat@, which neither covers, so it keeps the declared @Tri@, @A@ and @C@ included.
-- Fixture: unit/refineNarrowsCall.ct.
testRefineNarrowsCall :: TestTree
testRefineNarrowsCall = testCase "a call is within every refinement covering it" $ do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig ["test/Typecheck/unit/refineNarrowsCall.ct"]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let typeEnv = mkTypeEnv prgm
  -- What a definition's result settles to: its type without the unevaluated @tri@ call form.
  let settledResultOf name = [ settled (getMetaType m) | oa@ObjArr{oaArr=Just (_, m)} <- flatObjectMap (prgmObjMap prgm), oaObjPath oa == name ]
      settled (UnionType mTop leafs consts) = UnionType mTop (joinUnionType [l | l <- splitUnionType leafs, nameStr (ptName l) /= "/tri"]) consts
      settled t = t
      within name t = case settledResultOf name of
        [r] -> isSubtypeOf typeEnv r t
        rs  -> error ("one result for " ++ show name ++ ", found " ++ show rs)
      admits name t = case settledResultOf name of
        [r] -> isSubtypeOf typeEnv t r
        rs  -> error ("one result for " ++ show name ++ ", found " ++ show rs)
  assertBool ("onZ is within B: " ++ show (settledResultOf "/onZ")) (within "/onZ" (typeVal "/B"))
  assertBool ("onNat admits A and C: " ++ show (settledResultOf "/onNat")) (admits "/onNat" (typeVal "/A") && admits "/onNat" (typeVal "/C"))

-- | A value predicate is decided against a known value shape through the type graph. @Nat_isZ@ is
-- @Nat | $this.isZ == True@: on @Z@ it reaches only @True@, so @h@'s @Z@ is guaranteed within its
-- declared @Nat_isZ@ and @okZ@'s @f(a= Z)@ settles to the @Z@ it passes; on every @Nat@ it reaches
-- both, so @onNat@'s argument keeps the predicate. The twin error page passes an @S@, on which it
-- reaches only @False@. Fixtures: unit/refineDecide.ct, unit/refineDecideFails.err.ct.
testRefineDecide :: TestTree
testRefineDecide = testCase "a value predicate is decided on a known value" $ do
  let page = "test/Typecheck/unit/refineDecide.ct"
  assertResultGuaranteed page "/h"
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let typeEnv = mkTypeEnv prgm
  let resultOf name = [ getMetaType m | oa@ObjArr{oaArr=Just (_, m)} <- flatObjectMap (prgmObjMap prgm), oaObjPath oa == name ]
      leafsOf (UnionType _ leafs _) = splitUnionType leafs
      leafsOf _                     = []
      -- What a result settles to: its leaves without the unevaluated @f@ call forms.
      settled t = UnionType Nothing (joinUnionType [l | l <- leafsOf t, nameStr (ptName l) /= "/f"]) []
      -- The predicates on the @a@ argument of each @f@ call form in a result.
      argPreds t = [ ptPreds a | l <- leafsOf t, nameStr (ptName l) == "/f", (k, aT) <- H.toList (ptArgs l), nameStr (pkName k) == "/a", a <- leafsOf aT ]
      hasPredExpr preds = maybe False (any isPredExpr) (tryPredsToList preds)
      isPredExpr PredExpr{} = True
      isPredExpr _          = False
      -- Whether the predicate is still stated anywhere in a result: on its top (the arm's generic
      -- @a@, every @Nat@ within the refinement) or on an @f@ call's argument.
      keepsPredicate t = topHas t || any hasPredExpr (argPreds t)
      topHas (UnionType (Just (preds, _, _)) _ _) = hasPredExpr preds
      topHas _                                    = False
  case resultOf "/okZ" of
    [r] -> assertBool ("okZ settles to Z: " ++ show r) (isSubtypeOf typeEnv (settled r) (typeVal "/Z"))
    rs  -> assertFailure ("one result for okZ, found " ++ show rs)
  case resultOf "/onNat" of
    [r] -> assertBool ("onNat keeps the predicate undecided: " ++ show r) (keepsPredicate r)
    rs  -> assertFailure ("one result for onNat, found " ++ show rs)

-- | A value predicate the type checker cannot decide is the runtime's to evaluate, not a failure
-- of the guarantee: @onNat@ passes every @Nat@ where a @Nat_isZ@ is required and @h@ returns one
-- where a @Nat_isZ@ is declared, and neither is reported as not guaranteed. A result the predicate
-- is decided false on is still refused statically (the twin error page). Fixtures:
-- unit/refineRuntime.ct, unit/refineRuntimeFails.err.ct.
testRefineRuntime :: TestTree
testRefineRuntime = testCase "an undecided value predicate is left to the runtime" $ do
  let page = "test/Typecheck/unit/refineRuntime.ct"
  assertResultGuaranteed page "/onNat"
  assertResultGuaranteed page "/h"

-- | The value a condition refining a type is written over is named @$this@, or @$out@ as what the
-- arrow produces. Both desugar to the one name the value is bound under where a predicate is
-- decided ('CtConstants.macroThisKey'), so @h@'s @Nat | $out.isZ == True@ is @Nat_isZ@: decided on
-- the @Z@ it returns (and refused on the @S@ the twin error pages return), while @hRt@'s is left to
-- the runtime. A condition applying an argument by position cannot name it in the predicate and is
-- refused as written. Fixtures: unit/refineOut.ct, unit/refineOut{,Fn,This}Fails.err.ct,
-- unit/refineOutPositional.err.ct.
testRefineOut :: TestTree
testRefineOut = testCase "$out and $this in a type's condition name the refined value" $ do
  let page = "test/Typecheck/unit/refineOut.ct"
  assertResultGuaranteed page "/hRt"
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let resultOf name = [ getMetaType m | oa@ObjArr{oaArr=Just (_, m)} <- flatObjectMap (prgmObjMap prgm), nameStr (oaObjPath oa) == name ]
  let checkResult name = case resultOf name of
        [r] -> do
          let names = map nameStr (S.toList (typeRefNamesDeep r))
          assertBool (name ++ " binds the refined value under " ++ macroThisKey ++ ": " ++ show r) (macroThisKey `elem` names)
          assertBool (name ++ " leaves no written macro name unbound: " ++ show r) (not (any (`elem` names) [thisMacro, outMacro]))
        rs  -> assertFailure ("one result for " ++ name ++ ", found " ++ show rs)
  mapM_ checkResult (["/h", "/hFn", "/hThis", "/hRt"] :: [String])
  assertPageErrorSays "test/Typecheck/unit/refineOutPositional.err.ct" "by position"

-- | A body's condition refines its value by the condition's FORM ('TypeCheck.Encode.condPredForm'):
-- @k@'s @a | a.isZ == True@ types as @a@ within @$this.isZ == True@, the predicate @Nat_isZ@
-- desugars to, so @kDecl@ is guaranteed within its declared @Nat_isZ@ by the predicate and
-- @kZ@'s @k(a= Z)@ settles to @Z@. A condition on another argument is a predicate over that
-- argument's variable, not a value predicate; a bare argument as the condition states none. The
-- twin error pages pass an @S@, on which the predicate is decided false. Fixtures:
-- unit/whereRefine.ct, unit/whereRefineFails.err.ct, unit/whereRefineFnFails.err.ct.
testWhereRefine :: TestTree
testWhereRefine = testCase "a body's condition refines its value by the condition's form" $ do
  let page = "test/Typecheck/unit/whereRefine.ct"
  assertResultGuaranteed page "/kDecl"
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let typeEnv = mkTypeEnv prgm
  let bodyOf name = [ getExprType e | oa@ObjArr{oaArr=Just (Just e, _)} <- flatObjectMap (prgmObjMap prgm), nameStr (oaObjPath oa) == name ]
      resultOf name = [ getMetaType m | oa@ObjArr{oaArr=Just (_, m)} <- flatObjectMap (prgmObjMap prgm), nameStr (oaObjPath oa) == name ]
      leafsOf (UnionType _ leafs _) = splitUnionType leafs
      leafsOf _                     = []
      settled t = UnionType Nothing (joinUnionType [l | l <- leafsOf t, nameStr (ptName l) /= "/k"]) []
      one name xs f = case xs of
        [x] -> f x
        _   -> assertFailure ("one " ++ name ++ ", found " ++ show xs)
  one "body of k" (bodyOf "/k") $ \b ->
    assertBool ("k's condition is a predicate on the refined value: " ++ show b) (typeHasValuePredExpr b && macroThisKey `elem` map nameStr (S.toList (typeRefNamesDeep b)))
  one "body of kFn" (bodyOf "/kFn") $ \b ->
    assertBool ("kFn's condition is a predicate on the refined value: " ++ show b) (typeHasValuePredExpr b)
  one "body of kOther" (bodyOf "/kOther") $ \b ->
    assertBool ("kOther's condition is a predicate over the other argument, not the value: " ++ show b) (typeHasPredExpr b && not (typeHasValuePredExpr b))
  one "body of kArg" (bodyOf "/kArg") $ \b ->
    assertBool ("kArg's bare condition states no predicate: " ++ show b) (not (typeHasPredExpr b))
  one "result of kZ" (resultOf "/kZ") $ \r ->
    assertBool ("kZ settles to Z: " ++ show r) (isSubtypeOf typeEnv (settled r) (typeVal "/Z"))

-- | A definition's declared result refines what its body gives at every call
-- ('Semantics.arrowDestType'): @hGen(a -> Nat) -> Nat_isZ= a@ is guaranteed by shape (undecided on
-- every @Nat@), while @hZ@'s @hGen(a= Z)@ is read within the declaration and is a @Z@ within the
-- refinement, and @onNat@'s @hGen(a= n)@ carries the predicate on. The twin error page calls it
-- with an @S@, on which the predicate is decided false. Fixtures: unit/declResult.ct,
-- unit/declResultFails.err.ct.
testDeclResult :: TestTree
testDeclResult = testCase "a declared result refines every call of the definition" $ do
  let page = "test/Typecheck/unit/declResult.ct"
  assertResultGuaranteed page "/hGen"
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let typeEnv = mkTypeEnv prgm
  let resultOf name = [ getMetaType m | oa@ObjArr{oaArr=Just (_, m)} <- flatObjectMap (prgmObjMap prgm), nameStr (oaObjPath oa) == name ]
      leafsOf (UnionType _ leafs _) = splitUnionType leafs
      leafsOf _                     = []
      settled t = UnionType Nothing (joinUnionType [l | l <- leafsOf t, nameStr (ptName l) /= "/hGen"]) []
      one name xs f = case xs of
        [x] -> f x
        _   -> assertFailure ("one " ++ name ++ ", found " ++ show xs)
  one "result of hZ" (resultOf "/hZ") $ \r -> do
    assertBool ("hZ settles to Z: " ++ show r) (isSubtypeOf typeEnv (settled r) (typeVal "/Z"))
    assertBool ("hZ settles to a Z within the refinement, not an empty result: " ++ show r) (not (isBottomType (settled r)))
  one "result of onNat" (resultOf "/onNat") $ \r ->
    assertBool ("onNat carries the declared predicate on: " ++ show r) (typeHasValuePredExpr r)

-- | The page fails with an error whose text contains @needle@.
assertPageErrorSays :: String -> String -> Assertion
assertPageErrorSays page needle = do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgm ctss
  case res of
    CRes _ _  -> assertFailure "the page is written to fail, and it type-checked"
    CErr notes -> assertBool ("the error says " ++ show needle ++ ":\n" ++ prettyCNotes notes) (needle `isInfixOf` prettyCNotes notes)

-- | No 'ResultNotGuaranteed' is reported for the definition @name@ in the page.
assertResultGuaranteed :: String -> String -> Assertion
assertResultGuaranteed page name = do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig [page]
    getTPrgm ctss
  let notes = case res of
        CRes ns _ -> ns
        CErr ns   -> ns
  let reported = [ n | n <- [show note | note <- notes], "Result Not Guaranteed" `isInfixOf` n, show name `isInfixOf` n ]
  assertEqual (unlines reported) [] reported

typecheckTests :: IO TestTree
typecheckTests = do
  errPages <- findErrCt "test/Typecheck/unit/"
  return $ testGroup "TypeCheckTests" [
    -- THG.testProperty "propExprEncodeDecode" (p propExprEncodeDecode) -- TODO: Debug
    THG.testProperty "propConstraint" (p propConstraint)
    , THG.testProperty "propConstraints" (p propConstraints)
    , THG.testProperty "propTypeChecks" (p propTypeChecks)
    , testResultNotGuaranteed
    , testRefineArgsParam
    , testRecursiveDeclaredArg
    , testFoldDeclaredResult
    , testFoldClassResult
    , testMatchRecursiveCall
    , testDeclVarBound
    , testSharedKeyDeclaration
    , testIntersectionBound
    , testParamShadowsFunction
    , testHoleArm
    , testFoldClassOp
    , testRefineNarrowsCall
    , testRefineDecide
    , testRefineRuntime
    , testRefineOut
    , testWhereRefine
    , testDeclResult
    , testGroup "Error pages" (map testErrPage errPages)
    ]
  where
    p prop = prop
    -- p prop = withTests 10000 prop

main :: IO ()
main = defaultMain =<< typecheckTests
