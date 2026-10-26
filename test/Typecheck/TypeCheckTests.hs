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
import           Common.TestCommon          (findErrCt)
import           Control.Monad.State.Strict (evalStateT, execStateT, lift,
                                             runStateT)
import           CRes
import           CtConstants                (macroThisKey, outMacro, thisMacro)
import           CtService                  (ctssBaseFiles, getTPrgm,
                                             getTPrgmJoined)
import           Data.Graph                 (graphFromEdges)
import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as S
import           Data.List                  (isInfixOf, isPrefixOf, sort)
import           Hedgehog
import qualified Hedgehog.Gen               as HG
import           MapMeta
import           Semantics                  (mkTypeEnv, testCtssConfig)
import           Semantics.Prgm
import           Semantics.Types
import           Test.Tasty
import qualified Test.Tasty.Hedgehog        as THG
import           Test.Tasty.HUnit           (Assertion, assertBool, assertEqual,
                                             assertFailure, testCase)
import           Testing.Generation         (genPrgms)
import           TestService                (MkService)
import           Text.Printf
import           TypeCheck                  (typecheckPrgm)
import           TypeCheck.Common           (feCons, tcreToMaybe)
import           TypeCheck.Constrain        (executeConstraint)
import           TypeCheck.Decode           (toPrgms)
import           TypeCheck.Encode           (fromPrgms, makeBaseFEnv)
import           TypeCheck.Instruments      (defaultInstruments)
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
testErrPage :: MkService -> FilePath -> TestTree
testErrPage mk page = testCase page $ do
  res <- runCResT $ do
    ctss <- lift $ mk [page]
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
testResultNotGuaranteed :: MkService -> TestTree
testResultNotGuaranteed mk = testCase "unsound arms are reported, their sound twins are not" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/resultNotGuaranteed.err.ct"]
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
testRefineArgsParam :: MkService -> TestTree
testRefineArgsParam mk = testCase "an arm and a refinement within an args-tied declaration" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/refineArgsParam.ct"]
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
testRecursiveDeclaredArg :: MkService -> TestTree
testRecursiveDeclaredArg mk = testCase "a recursive call argument is within its declared result" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/recursiveDeclaredArg.ct"]
    getTPrgm ctss
  let notes = case res of
        CRes ns _ -> ns
        CErr ns   -> ns
  let reported = [ n | n <- [show note | note <- notes], "Result Not Guaranteed" `isInfixOf` n, "\"textNeg\"" `isInfixOf` n ]
  assertEqual (unlines reported) [] reported

-- | A fold's result is within the declared result of the definition it is the body of: @total@
-- folds with @foldr@ into an @Integer@, and the lambda's parameters are typed from the call.
-- Fixture: unit/foldDeclaredResult.ct.
testFoldDeclaredResult :: MkService -> TestTree
testFoldDeclaredResult mk = testCase "a fold into its declared result is guaranteed" $
  assertResultGuaranteed mk "test/Typecheck/unit/foldDeclaredResult.ct" "total"

-- | A fold into a declared CLASS result: the call's accumulator variable is chosen as the class.
testFoldClassResult :: MkService -> TestTree
testFoldClassResult mk = testCase "a fold into its declared class result is guaranteed" $
  assertResultGuaranteed mk "test/Typecheck/unit/foldClassResult.ct" "anyNeg"

-- | A recursive call inside a @match@ arm: the generated arm's raw call is read with the values
-- its call site binds to the arm's inputs.
testMatchRecursiveCall :: MkService -> TestTree
testMatchRecursiveCall mk = testCase "a recursive call in a match arm is guaranteed" $
  assertResultGuaranteed mk "test/Typecheck/unit/matchRecursiveCall.ct" "lookup"

-- | A higher-order argument's result spelled by a declaration variable the definition does not
-- apply: the variable ranges over the declaration's bound.
testDeclVarBound :: MkService -> TestTree
testDeclVarBound mk = testCase "a declaration variable a definition does not apply keeps its bound" $
  assertResultGuaranteed mk "test/Typecheck/unit/declVarBound.ct" "walk"

-- | An instance written under a declaration's absolute name on another page, then called by a
-- later fragment: the call is still read with the declaration that shares the instance's key.
testSharedKeyDeclaration :: MkService -> TestTree
testSharedKeyDeclaration mk = testCase "a declaration shared with a page's instance arms still bounds later calls" $
  assertResultGuaranteed mk "test/Typecheck/unit/sharedKey/use.ct" "filt"

-- | A type variable bounded by an intersection (@$F -> Box && Eq@) is within each part.
testIntersectionBound :: MkService -> TestTree
testIntersectionBound mk = testCase "a type variable bounded by an intersection is within each part" $
  assertResultGuaranteed mk "test/Typecheck/unit/intersectionBound.ct" "f"

-- | A definition's argument shadows a like-named function in another module: the check reads the
-- argument, not the function's arrows.
testParamShadowsFunction :: MkService -> TestTree
testParamShadowsFunction mk = testCase "an argument shadows a like-named function" $
  assertResultGuaranteed mk "test/Typecheck/unit/paramShadowsFunction.ct" "step"

-- | An arm whose body is a hole produces no value, so it is within its declaration.
testHoleArm :: MkService -> TestTree
testHoleArm mk = testCase "an arm ending in a hole is within its declaration" $
  assertResultGuaranteed mk "test/Typecheck/unit/holeArm.ct" "f"

-- | A fold whose step is an operator declared on its class (@+@ on @Add@) returns what the call
-- chose its accumulator to be, an empty list's fold included. Fixture: unit/foldClassOp.ct.
testFoldClassOp :: MkService -> TestTree
testFoldClassOp mk = testCase "a fold through a class-declared operator is within its declaration" $ do
  assertResultGuaranteed mk "test/Typecheck/unit/foldClassOp.ct" "/Fx/sum"
  assertResultGuaranteed mk "test/Typecheck/unit/foldClassOp.ct" "/Fx/sumNil"

-- | A call is read at the intersection of every refinement covering it, and a refinement covering
-- only part of a call says nothing about it. @tri@ is a primitive, so only its declaration and
-- refinements say what it returns. @onZ@ calls @tri(n= Z)@, which both refinements cover (@-> AB@
-- and @-> BC@), so what it settles to is within the @B@ they share; @onNat@ calls @tri@ on every
-- @Nat@, which neither covers, so it keeps the declared @Tri@, @A@ and @C@ included.
-- Fixture: unit/refineNarrowsCall.ct.
testRefineNarrowsCall :: MkService -> TestTree
testRefineNarrowsCall mk = testCase "a call is within every refinement covering it" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/refineNarrowsCall.ct"]
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

-- | A class conjunct implied by a sibling through a declared @isa@ edge is dropped wherever it
-- sits in a formula. With @Maybe isa Functor@, @h@'s meet of a @Maybe@ with the @Functor@-bounded
-- @$F@ is the @Maybe@ alone, and @f@'s bind continuations hold no @Functor@ atom, though their
-- @fmap@ arm instantiates @$F[$T -> $_B]@ with the inner continuation's whole result inside the
-- arms' union. Kept, the atom copied that result once more at every level of a nested bind chain,
-- so a chain's types grew threefold per bind. Fixture: unit/impliedClassConjunct.ct.
testImpliedClassConjunct :: MkService -> TestTree
testImpliedClassConjunct mk = testCase "an implied class conjunct is dropped at any depth" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/impliedClassConjunct.ct"]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let results = [ (p, getMetaType m) | oa@ObjArr{oaArr=Just (_, m)} <- flatObjectMap (prgmObjMap prgm)
                                      , let p = nameStr (oaObjPath oa), p == "/h" || "/f." `isPrefixOf` p ]
  assertBool "h and f's continuations are in the program" (any ((== "/h") . fst) results && any (("/f." `isPrefixOf`) . fst) results)
  let withFunctor = [ (p, t) | (p, t) <- results, "/Data/Functor" `elem` classAtomNames t ]
  assertEqual (unlines [p ++ " :: " ++ show t | (p, t) <- withFunctor]) [] (map fst withFunctor)
  where
    classAtomNames :: Type -> [String]
    classAtomNames (UnionType mTop leafs _) = maybe [] (\(ps, negs, _) -> predNames ps ++ leafNames negs) mTop ++ leafNames leafs
    classAtomNames (TypeVar _ _ inner) = classAtomNames inner
    leafNames = concatMap partialNames . splitUnionType
    partialNames p = concatMap classAtomNames (H.elems (ptVars p) ++ H.elems (ptArgs p)) ++ predNames (ptPreds p)
    predNames (PredsAnd ps)            = concatMap predNames ps
    predNames (PredsNot p)             = predNames p
    predNames (PredsOne (PredClass c)) = nameStr (ptName c) : partialNames c
    predNames (PredsOne (PredRel r))   = partialNames r
    predNames (PredsOne (PredExpr e))  = partialNames e
    predNames (PredsOne _)             = []

-- | A value predicate is decided against a known value shape through the type graph. @Nat_isZ@ is
-- @Nat | $this.isZ == True@: on @Z@ it reaches only @True@, so @h@'s @Z@ is guaranteed within its
-- declared @Nat_isZ@ and @okZ@'s @f(a= Z)@ settles to the @Z@ it passes; on every @Nat@ it reaches
-- both, so @onNat@'s argument keeps the predicate. The twin error page passes an @S@, on which it
-- reaches only @False@. Fixtures: unit/refineDecide.ct, unit/refineDecideFails.err.ct.
testRefineDecide :: MkService -> TestTree
testRefineDecide mk = testCase "a value predicate is decided on a known value" $ do
  let page = "test/Typecheck/unit/refineDecide.ct"
  assertResultGuaranteed mk page "/h"
  res <- runCResT $ do
    ctss <- lift $ mk [page]
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
testRefineRuntime :: MkService -> TestTree
testRefineRuntime mk = testCase "an undecided value predicate is left to the runtime" $ do
  let page = "test/Typecheck/unit/refineRuntime.ct"
  assertResultGuaranteed mk page "/onNat"
  assertResultGuaranteed mk page "/h"

-- | The value a condition refining a type is written over is named @$this@, or @$out@ as what the
-- arrow produces. Both desugar to the one name the value is bound under where a predicate is
-- decided ('CtConstants.macroThisKey'), so @h@'s @Nat | $out.isZ == True@ is @Nat_isZ@: decided on
-- the @Z@ it returns (and refused on the @S@ the twin error pages return), while @hRt@'s is left to
-- the runtime. A condition applying an argument by position cannot name it in the predicate and is
-- refused as written. Fixtures: unit/refineOut.ct, unit/refineOut{,Fn,This}Fails.err.ct,
-- unit/refineOutPositional.err.ct.
testRefineOut :: MkService -> TestTree
testRefineOut mk = testCase "$out and $this in a type's condition name the refined value" $ do
  let page = "test/Typecheck/unit/refineOut.ct"
  assertResultGuaranteed mk page "/hRt"
  res <- runCResT $ do
    ctss <- lift $ mk [page]
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
  assertPageErrorSays mk "test/Typecheck/unit/refineOutPositional.err.ct" "by position"

-- | A body's condition refines its value by the condition's FORM ('TypeCheck.Encode.condPredForm'):
-- @k@'s @a | a.isZ == True@ types as @a@ within @$this.isZ == True@, the predicate @Nat_isZ@
-- desugars to, so @kDecl@ is guaranteed within its declared @Nat_isZ@ by the predicate and
-- @kZ@'s @k(a= Z)@ settles to @Z@. A condition on another argument is a predicate over that
-- argument's variable, not a value predicate; a bare argument as the condition states none. A
-- declared refinement beside the same condition on the body is ONE predicate on the result
-- ('Semantics.Types.mergeSameFormPredExprs'), not two spellings of it. The
-- twin error pages pass an @S@, on which the predicate is decided false. Fixtures:
-- unit/whereRefine.ct, unit/whereRefineFails.err.ct, unit/whereRefineFnFails.err.ct.
testWhereRefine :: MkService -> TestTree
testWhereRefine mk = testCase "a body's condition refines its value by the condition's form" $ do
  let page = "test/Typecheck/unit/whereRefine.ct"
  assertResultGuaranteed mk page "/kDecl"
  res <- runCResT $ do
    ctss <- lift $ mk [page]
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
  -- The declared @Nat_isZ@ spells its @True@ absolute (the parser's implicit value) and the
  -- condition spells it as written; met on the result, the two are one form and one atom.
  one "result of kDecl" (resultOf "/kDecl") $ \r ->
    assertEqual ("kDecl's declared refinement and its condition's form are one predicate: " ++ show r) 1 (length (predsExprAtoms (topPredsOf r)))
  where
    topPredsOf (TopType _ ps)                   = ps
    topPredsOf (UnionType (Just (ps, _, _)) _ _) = ps
    topPredsOf (UnionType Nothing leafs [])
      | [l] <- splitUnionType leafs             = ptPreds l
    topPredsOf _                                = PredsNone

-- | A definition's declared result refines what its body gives at every call
-- ('Semantics.arrowDestType'): @hGen(a -> Nat) -> Nat_isZ= a@ is guaranteed by shape (undecided on
-- every @Nat@), while @hZ@'s @hGen(a= Z)@ is read within the declaration and is a @Z@ within the
-- refinement, and @onNat@'s @hGen(a= n)@ carries the predicate on. The twin error page calls it
-- with an @S@, on which the predicate is decided false. Fixtures: unit/declResult.ct,
-- unit/declResultFails.err.ct.
-- | A definition passing a call's result on keeps the link that result holds: @bf@'s lambda
-- @fn(v)= fn(v= v)@ returns @bf@'s own @$_B@, still unbound, and was read as ⊤, so the definition
-- returned @$_F[/T=⊤]@ where @fm@ declares @$F[$T -> $_B]@. bindap's fmap arm is this shape, and
-- read at ⊤ its result could not rule the arm out where a call needs a flat monad.
testPassedOnLink :: MkService -> TestTree
testPassedOnLink mk = testCase "a definition passing a call's result on keeps its link" $ do
  let page = "test/Typecheck/unit/passedOnLink.ct"
  res <- runCResT $ do
    ctss <- lift $ mk [page]
    getTPrgmJoined ctss
  prgm <- case res of
    CRes _ p -> return p
    CErr ns  -> assertFailure (prettyCNotes ns) >> fail "did not type-check"
  let defResults = [ getMetaType m | (name, omi) <- H.toList (omItems (prgmObjMap prgm)), nameStr name == "/R/bf", ObjArr{oaArr=Just (_, m)} <- omiDefs omi ]
  case defResults of
    [r] -> assertBool ("bf's definition returns fm's result with the continuation's $_B: " ++ show r) ("$_/B" `isInfixOf` show r)
    rs  -> assertFailure ("one definition of bf, found " ++ show rs)

testDeclResult :: MkService -> TestTree
testDeclResult mk = testCase "a declared result refines every call of the definition" $ do
  let page = "test/Typecheck/unit/declResult.ct"
  assertResultGuaranteed mk page "/hGen"
  res <- runCResT $ do
    ctss <- lift $ mk [page]
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
-- | A class member written as a bare name that is also another module's type is refused as
-- ambiguous, naming both and the fix, rather than crashing the desugarer. Fixture:
-- unit/ambiguousMember.err.ct.
testAmbiguousMember :: MkService -> TestTree
testAmbiguousMember mk = testCase "an ambiguous class member names what it could be" $ do
  let page = "test/Typecheck/unit/ambiguousMember.err.ct"
  assertPageErrorSays mk page "The name Empty in the members of the class /Mine/Choice is ambiguous"
  assertPageErrorSays mk page "/Mine/Empty or /Other/Empty"

-- | A state in a context is found by the type it is written with ('Syntax.Ct.Builder.contextState'),
-- so one naming no type is refused with an error where it is written rather than crashing the
-- desugarer. Fixture: unit/contextStateNoType.err.ct.
testContextStateNoType :: MkService -> TestTree
testContextStateNoType mk = testCase "a state naming no type is refused where it is written" $
  assertPageErrorSays mk "test/Typecheck/unit/contextStateNoType.err.ct" "i@c is not a state of the context"

assertPageErrorSays :: MkService -> String -> String -> Assertion
assertPageErrorSays mk page needle = do
  res <- runCResT $ do
    ctss <- lift $ mk [page]
    getTPrgm ctss
  case res of
    CRes _ _  -> assertFailure "the page is written to fail, and it type-checked"
    CErr notes -> assertBool ("the error says " ++ show needle ++ ":\n" ++ prettyCNotes notes) (needle `isInfixOf` prettyCNotes notes)
-- | A declaration applying arguments to every member of a class (@(_ -> F)(a -> In) -> YN@) is held
-- to each member: the member given the arguments is a call of it, which its own arrows must answer
-- within the result. Fixtures: unit/classSignatureFails.err.ct, and the members that keep it in
-- Integration/code/classFunction.ct.
testClassSignature :: MkService -> TestTree
testClassSignature mk = testCase "a member of a class of functions gives what the class says" $ do
  let page = "test/Typecheck/unit/classSignatureFails.err.ct"
  assertPageErrorSays mk page "Unfulfilled Class Signature"
  assertPageErrorSays mk page "Member that does not: /bad(/a=/In)"
  assertPageErrorLacks mk page "/good"

-- | A call whose declared result is one of its parameters (@pull(m -> Bag[$V -> $_MV], def -> $_MV)
-- -> $_MV@) gives what an argument's type fixes the parameter as, wherever it is passed: as the left
-- operand of a generic @*@ it is an @Fl@, not the @Ring@ @*@ is declared over. Fixture:
-- unit/paramBoundResult.ct.
testParamBoundResult :: MkService -> TestTree
testParamBoundResult mk = testCase "a call's parameter result is read from its arguments' types" $
  assertResultGuaranteed mk "test/Typecheck/unit/paramBoundResult.ct" "/Rx/scaled"

-- | The page fails, and its errors do not mention @needle@.
assertPageErrorLacks :: MkService -> String -> String -> Assertion
assertPageErrorLacks mk page needle = do
  res <- runCResT $ do
    ctss <- lift $ mk [page]
    getTPrgm ctss
  case res of
    CRes _ _  -> assertFailure "the page is written to fail, and it type-checked"
    CErr notes -> assertBool ("the error does not say " ++ show needle ++ ":\n" ++ prettyCNotes notes) (not (needle `isInfixOf` prettyCNotes notes))

-- | A call of a function's longer shape, where its shorter shape is a call with a class result,
-- is read both as the shorter call given the extra argument and as its result given it; the
-- second is refuted rather than losing both. Fixture: unit/longerShapeCall.ct.
testLongerShapeCall :: MkService -> TestTree
testLongerShapeCall mk = testCase "a call of a longer shape is within its declaration" $
  assertResultGuaranteed mk "test/Typecheck/unit/longerShapeCall.ct" "outer"

-- | An import written after another statement (@annot(#noCore)@ first) is read as an import, and
-- the page it names is imported. Fixture: unit/importAfterAnnot.
testImportAfterAnnot :: MkService -> TestTree
testImportAfterAnnot mk = testCase "an import written after another statement imports its page" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/importAfterAnnot/page.ct"]
    getTPrgm ctss
  case res of
    CRes _ _ -> return ()
    CErr ns  -> assertFailure (prettyCNotes ns)

-- | A sub function capturing a field its parent destructures declares the field's type for it, so
-- a body that narrows the field still fulfils the declaration. Fixture: unit/capturedField.ct.
testCapturedField :: MkService -> TestTree
testCapturedField mk = testCase "a captured destructured field is declared as the field's type" $ do
  res <- runCResT $ do
    ctss <- lift $ mk ["test/Typecheck/unit/capturedField.ct"]
    getTPrgm ctss
  case res of
    CRes _ _ -> return ()
    CErr ns  -> assertFailure (prettyCNotes ns)

-- | No 'ResultNotGuaranteed' is reported for the definition @name@ in the page.
assertResultGuaranteed :: MkService -> String -> String -> Assertion
assertResultGuaranteed mk page name = do
  res <- runCResT $ do
    ctss <- lift $ mk [page]
    getTPrgm ctss
  let notes = case res of
        CRes ns _ -> ns
        CErr ns   -> ns
  let reported = [ n | n <- [show note | note <- notes], "Result Not Guaranteed" `isInfixOf` n, show name `isInfixOf` n ]
  assertEqual (unlines reported) [] reported

typecheckTests :: MkService -> IO TestTree
typecheckTests mk = do
  errPages <- findErrCt "test/Typecheck/unit/"
  return $ testGroup "TypeCheckTests" [
    -- THG.testProperty "propExprEncodeDecode" (p propExprEncodeDecode) -- TODO: Debug
    THG.testProperty "propConstraint" (p propConstraint)
    , THG.testProperty "propConstraints" (p propConstraints)
    , THG.testProperty "propTypeChecks" (p propTypeChecks)
    , testResultNotGuaranteed mk
    , testRefineArgsParam mk
    , testRecursiveDeclaredArg mk
    , testFoldDeclaredResult mk
    , testFoldClassResult mk
    , testMatchRecursiveCall mk
    , testDeclVarBound mk
    , testSharedKeyDeclaration mk
    , testIntersectionBound mk
    , testParamShadowsFunction mk
    , testHoleArm mk
    , testFoldClassOp mk
    , testRefineNarrowsCall mk
    , testImpliedClassConjunct mk
    , testRefineDecide mk
    , testRefineRuntime mk
    , testRefineOut mk
    , testWhereRefine mk
    , testDeclResult mk
    , testPassedOnLink mk
    , testLongerShapeCall mk
    , testClassSignature mk
    , testParamBoundResult mk
    , testImportAfterAnnot mk
    , testCapturedField mk
    , testAmbiguousMember mk
    , testContextStateNoType mk
    , testGroup "Error pages" (map (testErrPage mk) errPages)
    ]
  where
    p prop = prop
    -- p prop = withTests 10000 prop

main :: IO ()
main = defaultMain =<< typecheckTests (ctssBaseFiles testCtssConfig)
