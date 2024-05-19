module Syntax.SyntaxTests where

import           Common.TestCommon       (findCt)
import           Control.Monad
import           CRes                    (fromCRes)
import qualified Data.HashMap.Strict     as H
import           Hedgehog
import           Semantics.Prgm
import           Syntax.Ct.Formatter     (formatRootPrgm)
import           Syntax.Ct.Parser        (contents, pPrgm)
import           Syntax.Ct.Parser.Syntax (rawStr)
import           Syntax.Ct.Prgm
import           Syntax.Parsers          (parseFile)
import           Test.Tasty
import           Test.Tasty.HUnit        (assertEqual, assertFailure, testCase)
import           Text.Megaparsec         (runParser)
import           Text.Printf

type Prgms = H.HashMap String (RawPrgm ())
type GenPrgm = Gen (Prgm Expr ())

findPrgms :: String -> IO Prgms
findPrgms prgmDir = do
  fileNames <- findCt prgmDir
  prgms <- forM fileNames $ \fileName -> do
    rawPrgm <- parseFile False $ mkRawFileImport $ rawStr fileName
    let (prgm, _, _) = fromCRes rawPrgm
    return (fileName, prgm)
  return $ H.fromList prgms

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
  ctTestPrgms <- findPrgms "test/Integration/code/"
  ctCorePrgms <- findPrgms "stack/core/"
  return $ testGroup "FormatTests" (map testPrgm $ concatMap H.toList [ctTestPrgms, ctCorePrgms])

main :: IO ()
main = do
  ts <- syntaxTests
  defaultMain ts
