module Syntax.SyntaxTests where

import           Common.TestCommon   (filesWithExtension)
import           Control.Monad
import           CRes                (CRes (..), CResT (runCResT), fromCRes,
                                      prettyCNotes)
import qualified Data.HashMap.Strict as H
import           Hedgehog
import           Semantics
import           Semantics.Prgm
import           Semantics.Types     (mkName)
import           Syntax.Ct.Builder   (rawStr)
import           Syntax.Ct.Formatter (formatRootPrgm)
import           Syntax.Ct.Parser    (contents, pPrgm)
import           Syntax.Ct.Prgm
import           Syntax.Parsers      (mkRawImportStr, parseFile, rawImportPath)
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

-- | A name given to the compiler means what the same name written after @import@ means, so the two
-- forms an import statement has are the two a command line argument has.
--
-- The path cases are the point of these: a path parses as an expression too — @a\/b.ct@ is a value
-- with a @.ct@ method suffix — so reading a name as an import cannot be a matter of whether it
-- parsed. Each of these is a shape a real program name takes.
testImportStr :: TestTree
testImportStr = testGroup "ImportStrTests" [
    testCase "an import naming its parser is read as that import" $ do
      let imp = impAbs $ mkRawImportStr "obsidian(\"dir\")"
      assertEqual "names the parser" (Just $ mkName "obsidian") (fmap fst $ maybeExprPathM imp)
      assertEqual "names the path" (Just "dir") (rawImportPath imp)

  , testCase "an import of a directory of files is read as that import" $ do
      let imp = impAbs $ mkRawImportStr "fileDirReference(\"dir\")"
      assertEqual "names the parser" (Just $ mkName "fileDirReference") (fmap fst $ maybeExprPathM imp)
      assertEqual "names the path" (Just "dir") (rawImportPath imp)

  , testGroup "a path is read as the file it names" $ map pathCase [
      "test/Integration/code/id.ct"  -- a value with a method suffix
    , "main.ct"                      -- the same, with no directory to it
    , "core"                         -- a bare value, the one import written in every program
    , "artifacts/catln-test"         -- a directory, which does not parse as an expression at all
    , "stack"
    ]
  ]
  where
    pathCase name = testCase name $
      assertEqual "" (rawStr name) (impAbs $ mkRawImportStr name)

-- | A comment is markdown, and markdown says with indentation what a nested list, a continued item
-- or an indented block is. So the writing under a comment keeps how it was laid out within itself,
-- rather than every line of it having to sit at the indentation of the first.
testCommentIndent :: TestTree
testCommentIndent = testGroup "CommentIndentTests" [
    testCase "the writing under a comment keeps how it was indented within itself" $ roundTrips [
      "# A comment."
    , "  Its writing, at the indentation the writing begins at."
    , "    A line indented within that writing, as a nested list item is."
    , "      And a line indented within that one."
    , "  Back where the writing began."
    , "main= 1"
    ]

  , testCase "a comment with nothing under it is the line it was written on" $ roundTrips [
      "# A comment."
    , "main= 1"
    ]

  -- The writing begins where its first line does, so a line to the left of that is out of it. Being
  -- still to the right of the @#@ it is not out of the comment either, which is nowhere to be.
  , testCase "a line dedented out of the writing but not out of the comment is an error" $
      case parsed [
          "#   A comment indented well past its own writing."
        , "   Its writing."
        , "  A line dedented out of that writing."
        , "main= 1"
        ] of
        Left _  -> return ()
        Right f -> assertFailure $ printf "a comment indented out of its own writing was read, as:\n%s" f
  ]
  where
    parsed ls = let src = unlines ls in
      case runParser (contents pPrgm) "<comment>" src of
        Left err -> Left (show err)
        Right p  -> Right (formatRootPrgm p)
    -- Written back out as it was written, which is what @make ctformat@ asks of every file: what the
    -- parser keeps of the writing and what the formatter writes of it have to be the same thing.
    roundTrips ls = case parsed ls of
      Left err -> assertFailure err
      Right f  -> assertEqual "formatting a comment gave back different writing" ls (lines f)

syntaxTests :: IO TestTree
syntaxTests = do
  -- TODO: Add hs prgmDirs
  -- let prgmDirs = [(".ct",  "test/Integration/code/"), (".ct", "stack/core/"), (".hs", "src/")]
  --
  -- The disabled programs are read too. They are not compiled — each is the repro of a gap that
  -- stops it — but they are still written in Catln and are still formatted by @make ctformat@, and
  -- what nothing reads is what quietly stops parsing: four of them had, and were found by the
  -- formatter rather than here.
  let prgmDirs = [(".ct",  "test/Integration/code/"), (".ct", "test/Integration/disabled/"), (".ct", "stack/core/")]
  prgms <- mapM findPrgms prgmDirs
  let allPrgms = concatMap H.toList prgms
  return $ testGroup "SyntaxTests" [
      testGroup "FormatTests" (map testPrgm allPrgms)
    , testGroup "ReparseTests" (map testReparse allPrgms)
    , testImportStr
    , testCommentIndent
    ]

main :: IO ()
main = do
  ts <- syntaxTests
  defaultMain ts
