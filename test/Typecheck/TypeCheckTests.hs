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
import           Control.Monad.State   (evalStateT, execStateT, lift, runStateT)
import           CRes
import           CtService             (ctssBaseFiles, getTPrgm)
import           Data.Graph            (graphFromEdges)
import           Data.List             (isInfixOf, sort)
import           Hedgehog
import qualified Hedgehog.Gen          as HG
import           MapMeta
import           Semantics             (testCtssConfig)
import           Semantics.Prgm
import           Test.Tasty
import qualified Test.Tasty.Hedgehog   as THG
import           Test.Tasty.HUnit      (assertEqual, testCase)
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

-- | A definition whose body needs more of its input than its declaration promises is reported, and
-- its twin whose declaration states what the body needs is not. @unwrapBad@ returns a @v@ that may be
-- anything as a @Num@; @showBad@ hands what @fn@ returns, which may be anything, to @render@, which
-- needs a @Shows@. Both type-check; only the type checker's stronger question — is every form the
-- value may take guaranteed within the declaration — tells them from @unwrapOk@ and @showOk@.
-- Fixture: unit/resultNotGuaranteed.ct.
testResultNotGuaranteed :: TestTree
testResultNotGuaranteed = testCase "unsound arms are reported, their sound twins are not" $ do
  res <- runCResT $ do
    ctss <- lift $ ctssBaseFiles testCtssConfig ["test/Typecheck/unit/resultNotGuaranteed.ct"]
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

typecheckTests :: TestTree
typecheckTests = testGroup "TypeCheckTests" [
    -- THG.testProperty "propExprEncodeDecode" (p propExprEncodeDecode) -- TODO: Debug
    THG.testProperty "propConstraint" (p propConstraint)
    , THG.testProperty "propConstraints" (p propConstraints)
    , THG.testProperty "propTypeChecks" (p propTypeChecks)
    , testResultNotGuaranteed
    , testRefineArgsParam
    , testRecursiveDeclaredArg
                                  ]
  where
    p prop = prop
    -- p prop = withTests 10000 prop

main :: IO ()
main = defaultMain typecheckTests
