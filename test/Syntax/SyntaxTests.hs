module Syntax.SyntaxTests where

import           Common.TestCommon   (filesWithExtension)
import           Control.Monad
import           CRes                (CResT (runCResT), fromCRes)
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
import           Test.Tasty.HUnit    (assertEqual, assertFailure, testCase)
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


syntaxTests :: IO TestTree
syntaxTests = do
  -- TODO: Add hs prgmDirs
  -- let prgmDirs = [(".ct",  "test/Integration/code/"), (".ct", "stack/core/"), (".hs", "src/")]
  let prgmDirs = [(".ct",  "test/Integration/code/"), (".ct", "stack/core/")]
  prgms <- mapM findPrgms prgmDirs
  return $ testGroup "FormatTests" (map testPrgm $ concatMap H.toList prgms)

main :: IO ()
main = do
  ts <- syntaxTests
  defaultMain ts
