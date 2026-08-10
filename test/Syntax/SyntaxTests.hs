module Syntax.SyntaxTests where

import           Common.TestCommon   (filesWithExtension)
import           Control.Monad
import           CRes                (CRes (..), CResT (runCResT), fromCRes,
                                      prettyCNotes)
import qualified Data.HashMap.Strict as H
import           Hedgehog
import           Semantics
import           Semantics.Prgm
import           Syntax.Ct.Builder   (rawStr)
import           Syntax.Ct.Formatter (formatRootPrgm)
import           Syntax.Ct.Parser    (contents, pPrgm)
import           Syntax.Ct.Prgm
import           Syntax.Parsers      (parseFile)
import           Test.Tasty
import           Test.Tasty.HUnit    (assertBool, assertEqual, assertFailure,
                                      testCase)
import           Text.Megaparsec     (runParser)
import           Text.Printf

type Prgms = H.HashMap String (RawPrgm ())
type GenPrgm = Gen (Prgm Expr ())

findPrgms :: (String, String) -> IO Prgms
findPrgms (extension, prgmDir) = do
  fileNames <- filesWithExtension extension prgmDir
  prgms <- runCResT $ do
    forM fileNames $ \fileName -> do
      (prgm, _, _) <- parseFile testCtssConfig $ mkRawFileImport $ rawStr fileName
      return (fileName, prgm)
  return $ H.fromList $ fromCRes prgms

testPrgm :: (String, RawPrgm ()) -> TestTree
testPrgm (name, p1) = do
  let testName = printf "testFormat %s" name
  let f1 = formatRootPrgm p1
  case runParser (contents pPrgm) name f1 of
    Left err -> testCase testName $ assertFailure (printf "Fail: %s" (show err))
    Right p2 -> do
      let f2 = formatRootPrgm p2
      testCase testName (assertEqual (printf "Verify Formatting Equals:\n\t%s\n\t%s" f1 f2) f1 f2)

-- | Reading a file which has not changed must give back an equal program.
--
-- 'Meta' compares by id, and 'CtService.ctssRead' decides whether a page needs rebuilding by
-- comparing the program it just read against the one it holds. Ids used to be drawn at random per
-- read, so that comparison never matched and every read rebuilt every page.
--
-- The two programs are not shown on a failure: 'Show' for a 'Meta' prints its type only, so the ids
-- which are the likely difference would not appear in the diff.
testReparse :: (String, RawPrgm ()) -> TestTree
testReparse (name, p1) = testCase (printf "testReparse %s" name) $ do
  reparsed <- runCResT $ parseFile testCtssConfig $ mkRawFileImport $ rawStr name
  case reparsed of
    CErr notes        -> assertFailure $ prettyCNotes notes
    CRes _ (p2, _, _) -> assertBool "reading an unchanged file gave back a different program, so meta ids are not deterministic" (p1 == p2)

syntaxTests :: IO TestTree
syntaxTests = do
  -- TODO: Add hs prgmDirs
  -- let prgmDirs = [(".ct",  "test/Integration/code/"), (".ct", "stack/core/"), (".hs", "src/")]
  let prgmDirs = [(".ct",  "test/Integration/code/"), (".ct", "stack/core/")]
  prgms <- mapM findPrgms prgmDirs
  let allPrgms = concatMap H.toList prgms
  return $ testGroup "SyntaxTests" [
      testGroup "FormatTests" (map testPrgm allPrgms)
    , testGroup "ReparseTests" (map testReparse allPrgms)
    ]

main :: IO ()
main = do
  ts <- syntaxTests
  defaultMain ts
