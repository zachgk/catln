-- | What a directory is read as is user space, held by @test\/Integration\/code\/fileDir.ct@ and
-- @dirEntry.ct@ and their goldens. What is left here is the re-read when a file in it changes, which
-- no test of the Catln suite can make, and the escaping of a path into a name, which is the
-- compiler's.
--
-- | Tests for reading a directory as the page of what is in it.
--
-- @import fileDirReference("../notes")@ walks a directory and writes a page declaring every file
-- under it, which is what an overlay directory's @dir.ctc@ is handed. Three things have to hold for
-- that page to be worth anything, and each is a case below: it must say what is actually there, it
-- must be the same page when nothing has changed, and it must be a different page when something
-- has.
--
-- Read over files written for the test rather than over the ones in the repository, because the
-- cases are about what a directory holds and what happens when that changes.
module Syntax.FileDirTests where

import           CtConstants           (fileDirName, nameInvalidChars)
import           Data.Char             (isPrint, isSpace)
import           Data.List             (isInfixOf)
import           Data.Time.Clock       (addUTCTime, getCurrentTime)
import           Semantics             (testCtssConfig)
import           Semantics.Prgm        (Expr, impAbs)
import           Syntax.Ct.Prgm        (ImportReader (..), ParsedImport (..),
                                        PreloadImport (..), ReadPath (..),
                                        newReadEnv)
import           Syntax.FileDir.Parser (newFileDirReader)
import           Syntax.Parsers        (mkDesCanonicalImportStr)
import           Syntax.RereadTests    (noPages, readAgain, readOnce,
                                        writeLater)
import           System.Directory      (canonicalizePath,
                                        createDirectoryIfMissing,
                                        getTemporaryDirectory,
                                        removePathForcibly)
import           System.FilePath       ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit

-- | A directory to write the files of one case in, emptied first.
caseDir :: String -> IO FilePath
caseDir name = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-filedir" </> name
  removePathForcibly dir
  createDirectoryIfMissing True dir
  canonicalizePath dir

-- | The import expression a program naming this directory writes, as the compiler canonicalizes it.
dirExpr :: FilePath -> IO (Expr ())
dirExpr dir = impAbs <$> mkDesCanonicalImportStr testCtssConfig (concat ["fileDirReference(\"", dir, "\")"])

-- | Read a directory as the page of it.
readDir :: FilePath -> IO ParsedImport
readDir dir = do
  env <- newReadEnv
  reader <- newFileDirReader env
  imp <- dirExpr dir
  irParse reader imp

-- | Where a directory's page sits in the graph, without reading the files.
preloadDir :: FilePath -> IO PreloadImport
preloadDir dir = do
  env <- newReadEnv
  reader <- newFileDirReader env
  imp <- dirExpr dir
  irPreload reader imp

-- | Both kinds of read are recorded, because neither answers for the other.
testRecordsReads :: TestTree
testRecordsReads = testCase "the tree and every file in it are recorded as read" $ do
  dir <- caseDir "records"
  createDirectoryIfMissing True (dir </> "sub")
  writeFile (dir </> "a.md") "first"
  writeFile (dir </> "sub" </> "b.md") "second"
  parsed <- readDir dir
  preloaded <- preloadDir dir

  assertEqual "the tree and its files were not what the read recorded"
    (Just [ReadTree dir, ReadFile (dir </> "a.md"), ReadFile (dir </> "sub" </> "b.md")])
    (piReads parsed)
  -- The two must agree, or a page is timed against a set of paths other than the one it was built
  -- from and comes back stale without having changed.
  assertEqual "the preload recorded something other than the parse"
    (piReads parsed) (plReads preloaded)

-- | The case the 'ReadFile's are there for.
--
-- A tree's recorded time is the newest time any DIRECTORY under it was written, and editing a file
-- does not write to the directory holding it. So a page recorded as a tree alone is handed back
-- unchanged for a directory whose every file has been rewritten — which is exactly the copy an
-- overlay must not be left holding.
testFileEditIsRead :: TestTree
testFileEditIsRead = testCase "a file edited in the directory is read again" $ do
  dir <- caseDir "edited"
  now <- getCurrentTime
  let target = dir </> "target"
  createDirectoryIfMissing True target
  writeLater now (target </> "a.md") "first"
  writeLater now (dir </> "user.ct") $ concat ["import \"core\"\nimport fileDirReference(\"", target, "\")\n"]

  first <- readOnce noPages (dir </> "user.ct")
  unchanged <- readOnce first (dir </> "user.ct")
  assertEqual "a read of an unchanged directory read its page again" [] (readAgain dir first unchanged)

  -- The file's contents change and nothing is added to or removed from any directory, so every
  -- directory's own modification time is as it was.
  writeLater (addUTCTime 60 now) (target </> "a.md") "second"
  changed <- readOnce unchanged (dir </> "user.ct")
  assertBool "the directory's page was not read again after a file in it was edited"
    (any (target `isInfixOf`) (readAgain dir unchanged changed))

-- | The name is built from the path, and has to be a name.
--
-- Every character a name cannot hold is escaped, and the escape is reversible, so two directories
-- are never one name. This is the one piece a reader reproduces by eye, and a name that does not
-- parse fails silently rather than loudly — @.@ is the method operator, @-@ is subtraction — so it
-- is checked here rather than left to a case that would merely not compile.
testName :: TestTree
testName = testCase "a path becomes a name which is a name" $ do
  assertEqual "a path needing no escape was not written as itself"
    "/FileDir/home/zach/notes" (fileDirName "/home/zach/notes")
  assertEqual "the characters a name cannot hold were not escaped"
    "/FileDir/home/zach/myq2Dnotes/siteq5Fv2" (fileDirName "/home/zach/my-notes/site_v2")
  assertEqual "the escape character was not itself escaped"
    "/FileDir/qqueue" (fileDirName "/queue")
  -- Reversible, so that one directory is one name and two are never the same one.
  assertBool "two paths differing only by an escaped character became one name"
    (fileDirName "/a-b" /= fileDirName "/a_b")
  assertBool "a path and the escape of it became one name"
    (fileDirName "/a-b" /= fileDirName "/aq2Db")
  -- Every character of every name it builds is one a name may hold.
  let nameChar c = c == '/' || (isPrint c && not (isSpace c) && c `notElem` nameInvalidChars)
  assertBool "a name was built holding a character a name cannot hold"
    (all nameChar (fileDirName "/a-b/c.d/e_f/g h/i\"j"))

fileDirTests :: TestTree
fileDirTests = testGroup "FileDirTests" [
  testRecordsReads,
  testFileEditIsRead,
  testName
                                        ]
