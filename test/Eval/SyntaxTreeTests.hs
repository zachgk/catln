-- | Tests for a debugger run read by the functions its page writes ('Eval.SyntaxTree').
--
-- Over a real run of @debugNested.ct@ rather than a tree written by hand, because what is being
-- checked is that the arrows desugar writes for a function — a line, a helper, a branch arm, a bind's
-- continuation — are found again as the statements they were written as, and that only holds for the
-- arrows desugar actually writes.
module Eval.SyntaxTreeTests (syntaxTreeTests) where

import           CRes
import           CtService           (CTSS)
import qualified Data.HashMap.Strict as H
import           Data.List           (isSuffixOf, nub)
import           Data.Maybe          (mapMaybe)
import           Data.UUID           (UUID)
import           Eval.Common
import           Semantics           (testCtssConfig)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm
import           Syntax.Parsers      (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | The name each statement of the run is written with: a declaration's object's, a bind's
-- pattern's, and an annotation's own.
stmtNames :: SyntaxRun -> H.HashMap UUID String
stmtNames SyntaxRun{srStatements} = H.fromList $ concatMap (treeNames . ssTree) srStatements
  where
    treeNames (RawStatementTree s children) = mapMaybe stmtName [s] ++ concatMap treeNames children
    stmtName (RawDeclStatement RawObjArr{roaObj=Just o}) = named o
    stmtName (RawBindStatement RawObjArr{roaObj=Just o}) = named o
    stmtName (RawAnnot a)                                = named a
    stmtName _                                           = Nothing
    named e = (\n -> (getMetaId $ getExprMeta e, nameStr n)) <$> maybeExprPath e

-- | The one call of the named function among some calls.
callOf :: H.HashMap UUID String -> String -> [SyntaxCall] -> IO SyntaxCall
callOf names name calls = case [c | c@SyntaxCall{scStmt} <- calls, H.lookup scStmt names == Just name] of
  [c] -> return c
  cs  -> assertFailure $ "Expected one call of " ++ name ++ ", found " ++ show (length cs)

-- | What each run of the named line of a call came to.
lineValues :: H.HashMap UUID String -> String -> SyntaxCall -> [Maybe Val]
lineValues names name SyntaxCall{scLines} = concat [map lrValue slRuns | SyntaxLine{slStmt, slRuns} <- scLines, H.lookup slStmt names == Just name]

-- | The functions of the page a call called, in the order it called them. Those of the library it
-- imports are left out: which of them a call reaches is the library's business.
calledNames :: H.HashMap UUID String -> SyntaxRun -> SyntaxCall -> [String]
calledNames names SyntaxRun{srStatements} SyntaxCall{scCalls} = mapMaybe (\SyntaxCall{scStmt} -> if scStmt `elem` onPage then H.lookup scStmt names else Nothing) scCalls
  where onPage = [ssId | SyntaxStatement{ssId, ssFile} <- srStatements, "debugNested.ct" `isSuffixOf` ssFile]

-- | The lines of the function a call is of: every statement written under it but those of a
-- function nested in it (the helper, and its own lines).
statementLines :: SyntaxRun -> SyntaxCall -> [UUID]
statementLines SyntaxRun{srStatements} SyntaxCall{scStmt} = concat [ssLines | SyntaxStatement{ssId, ssLines} <- srStatements, ssId == scStmt]

syntaxTreeTests :: IO CTSS -> TestTree
syntaxTreeTests getCtss = testCase "a run is read by the functions its page writes" $ do
  ctss <- getCtss
  page <- mkDesCanonicalImportStr testCtssConfig "test/Integration/code/debugNested.ct"
  res <- csRunFun ctss page "/main"
  DebugRun{drSyntax=syntax@SyntaxRun{srCalls}, drResult=EvalResult{erExprs}} <- case res of
    CRes _ r    -> return r
    CErr notes' -> assertFailure $ prettyCNotes notes'
  let names = stmtNames syntax

  -- A call is a step of the one expression it rewrote. The == main's exit checks while dispatching its
  -- operator- is an expression of its own, evaluated inside the operator-'s, and closes at the same
  -- level; counted by both, the operator- read as having come from a call of ==.
  let steps = concatMap exSteps erExprs
  length steps @?= length (nub steps)

  -- main is the call the run makes, declared with a context and found through it.
  mainCall <- callOf names "main" srCalls
  calledNames names syntax mainCall @?= ["bump", "total"]

  total <- callOf names "total" (scCalls mainCall)
  -- What the call came to, rather than the operator+ its body rewrites to.
  lineValues names "total" total @?= [Just (IntVal 63)]
  -- Used by the #assert, the #debug, the helper and twice by the result: five evaluations, each 3.
  lineValues names "sum" total @?= replicate 5 (Just (IntVal 3))
  -- Never used, so never evaluated, though it is a line of total all the same.
  lineValues names "unused" total @?= []
  map (`H.lookup` names) (statementLines syntax total) @?= map Just ["sum", "unused", "#assert", "#debug", "/operator+"]
  -- Each run of sum is of the arrow desugar made of it, which is how the breakdown of whatever used
  -- it leads back to it.
  length [n | SyntaxLine{slStmt, slRuns} <- scLines total, H.lookup slStmt names == Just "sum", LineRun{lrNode=Just n} <- slRuns] @?= 5
  -- The annotations are lines of their own rather than part of the body.
  length (lineValues names "#assert" total) @?= 1
  lineValues names "#debug" total @?= [Just (TupleVal "/Catln/#debug" (H.singleton "/d" (IntVal 3)))]
  -- The helper takes a parameter of its own, so it is a call rather than a line; scale and sign are
  -- other functions of the page.
  calledNames names syntax total @?= ["helper", "scale", "sign"]
  helper <- callOf names "helper" (scCalls total)
  lineValues names "helper" helper @?= [Just (IntVal 6)]

  -- The arm the if took is a line of sign, reached by the caller after sign returned it.
  sign <- callOf names "sign" (scCalls total)
  lineValues names "Then" sign @?= [Just (IntVal 1)]
  lineValues names "Else" sign @?= []

  -- The bind's continuation is the rest of bump: the bound value, and the lines after it.
  bump <- callOf names "bump" (scCalls mainCall)
  lineValues names "x" bump @?= [Just (IntVal 5)]
  lineValues names "doubled" bump @?= [Just (IntVal 10)]
