-- | Tests for which files of a directory are pages of it.
--
-- A directory's listing is the one place the compiler decides that a file is not worth reading
-- before anything has tried to read it. That decision used to be made from the syntaxes the compiler
-- supplies alone, which was right while those were the only readers there were. An importer declared
-- in a @.ct@ file is a reader the compiler does not know about, so the decision has to ask what the
-- program declares — and until it did, moving the Python importer out of the compiler's table
-- silently stopped @.py@ files being listed.
--
-- Driven against a stub rather than a compilation service, for the reason "Syntax.NativeTests"
-- gives: what is being tested is that the listing asks, not that the service answers.
module Syntax.DirListingTests where

import           Data.List          (isSuffixOf, sort)
import           Semantics.Prgm     (AFileImport (..))
import           Semantics.Types    (Constant (CStr))
import           Syntax.Ct.Prgm     (ImportReader (..), ImportSyntax (..),
                                     ImporterEnv (..), PreloadImport (..),
                                     RawExpr (..), newRecordedReadEnv,
                                     noImporters)
import           Syntax.InferImport (dirSyntax)
import           Syntax.Parsers     (mkDesCanonicalImportStr)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Semantics          (testCtssConfig)

-- | A directory holding one file of each kind that matters: one the compiler reads itself, two an
-- importer reads, and one nothing reads.
listingDir :: FilePath
listingDir = "test/Integration/code/artifacts/listing"

-- | The names of the files a read of that directory walks on to.
--
-- Only the names, because the paths are absolute and the point of each case is which files were
-- picked rather than where they are.
listedNames :: ImporterEnv -> IO [String]
listedNames importers = do
  env <- newRecordedReadEnv Nothing importers
  reader <- isReader dirSyntax env
  imp <- mkDesCanonicalImportStr testCtssConfig listingDir
  preloaded <- irPreload reader (impAbs imp)
  -- Each extra import is `mkRawFileImport (rawStr path)`, so the path is the constant itself rather
  -- than an argument of an application.
  return $ sort [baseName p | AFileImport{impAbs=RawCExpr _ (CStr p)} <- plExtraImports preloaded]
  where
    baseName = reverse . takeWhile (/= '/') . reverse

-- | A stub compiler whose registry declares importers for @.py@ and @.json@.
--
-- The extensions alone, since that is all the listing asks for. Nothing here says which importer
-- reads them — that is settled later, by the registry, when the file is actually read.
declaringImporters :: ImporterEnv
declaringImporters = noImporters{ieExtensions = return ["py", "json"]}

-- | With no compiler behind it, a read lists what the compiler itself can read and nothing else.
--
-- This is the formatter's case and @catln convert@'s, and it must not change: those commands have no
-- service to ask, so a listing that needed one would fail rather than answer.
testListsBuiltinExtensions :: TestTree
testListsBuiltinExtensions = testCase "a listing with no compiler holds only what the compiler reads" $ do
  names <- listedNames noImporters
  assertEqual "only the .ct is listed" ["known.ct"] names

-- | The case the whole change is for: a file an importer reads is a page of the directory holding
-- it.
testListsDeclaredExtensions :: TestTree
testListsDeclaredExtensions = testCase "a listing holds the files a declared importer reads" $ do
  names <- listedNames declaringImporters
  assertEqual "the .ct and both importer files are listed" ["known.ct", "reader.json", "reader.py"] names

-- | An extension nothing claims is still not a page, however many importers there are.
--
-- Listing everything would be the easy way to stop missing files and is the wrong one: every file
-- listed is a file the read goes on to parse, so a directory holding a binary would become a read
-- that fails on it.
testLeavesUnreadableFiles :: TestTree
testLeavesUnreadableFiles = testCase "a file nothing reads is left out of the listing" $ do
  names <- listedNames declaringImporters
  assertBool "the .txt is not listed" (not $ any (".txt" `isSuffixOf`) names)

dirListingTests :: TestTree
dirListingTests = testGroup "DirListing" [
  testListsBuiltinExtensions,
  testListsDeclaredExtensions,
  testLeavesUnreadableFiles
                                         ]
