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

import           CtConstants           (buildFileContents, buildFileFilePath,
                                        catlnResultFiles, catlnResultPath,
                                        fileDirName, nameInvalidChars)
import qualified Data.ByteString       as BS
import           Data.Char             (isPrint, isSpace)
import           Data.List             (isInfixOf)
import           Data.Maybe            (listToMaybe, mapMaybe)
import           Data.Time.Clock       (addUTCTime, getCurrentTime)
import           Semantics             (testCtssConfig)
import           Semantics.Prgm        (Expr, impAbs, impRaw)
import           Semantics.Types       (Constant (CStr), Name, PartialKey (..),
                                        mkName, nameStr, partialKey)
import           Syntax.Ct.Prgm        (ImportReader (..), ParsedImport (..),
                                        PreloadImport (..), RawExpr (..),
                                        RawObjArr (..), RawPrgm (..),
                                        RawStatement (..),
                                        RawStatementTree (..), ReadPath (..),
                                        newReadEnv)
import           Syntax.FileDir.Parser (newFileDirReader)
import           Syntax.InferImport    (dirParse)
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

nameOf :: String -> Name
nameOf = pkName . partialKey . mkName

-- | The name the page declares the directory under.
pageDeclName :: RawPrgm () -> Maybe String
pageDeclName (RawPrgm _ statements) = listToMaybe $ mapMaybe declName statements
  where
    declName (RawStatementTree (RawDeclStatement RawObjArr{roaObj=Just (RawValue _ n)}) _) = Just $ nameStr n
    declName _ = Nothing

-- | The files the page declares, as the paths they are at paired with what is written in them, in
-- the order the page holds them.
pageFiles :: RawPrgm () -> [(String, String)]
pageFiles (RawPrgm _ statements) = concatMap statementFiles statements
  where
    statementFiles (RawStatementTree (RawDeclStatement RawObjArr{roaArr=Just (Just e, _)}) _) = resultFiles e
    statementFiles _ = []
    resultFiles (RawTupleApply _ (_, RawValue _ n) args) | nameStr n == catlnResultPath =
      concatMap listedFiles [v | (_, RawObjArr{roaObj=Just (RawValue _ a), roaArr=Just (Just v, _)}) <- args, a == nameOf catlnResultFiles]
    resultFiles _ = []
    listedFiles (RawList _ entries) = mapMaybe entryFile entries
    listedFiles _                   = []
    entryFile e = (,) <$> strArg e buildFileFilePath <*> strArg e buildFileContents

-- | One argument of a declared file. Named absolutely, as an argument is once it has been built.
strArg :: RawExpr () -> String -> Maybe String
strArg (RawTupleApply _ _ args) want = listToMaybe
  [v | (_, RawObjArr{roaObj=Just (RawValue _ n), roaArr=Just (Just (RawCExpr _ (CStr v)), _)}) <- args, n == nameOf want]
strArg _ _ = Nothing

-- | What the page says of itself, which is the name to write to reach it and what it could not
-- carry.
pageComments :: RawPrgm () -> [String]
pageComments (RawPrgm _ statements) = mapMaybe comment statements
  where
    comment (RawStatementTree (RawAnnot e) _) = strArg e "text"
    comment _                                 = Nothing

-- | The page says what is in the directory, at the paths it is at.
testDeclares :: TestTree
testDeclares = testCase "a directory is declared as the files under it" $ do
  dir <- caseDir "declares"
  createDirectoryIfMissing True (dir </> "sub")
  writeFile (dir </> "a.md") "first"
  writeFile (dir </> "sub" </> "b.md") "second"
  parsed <- readDir dir

  assertEqual "the directory was not declared under its path-derived name"
    (Just $ fileDirName dir) (pageDeclName $ piPrgm parsed)
  -- Sorted, and the path of a file under a subdirectory keeps the subdirectory in it. Sorted
  -- matters rather than being tidy: these are the entries of a declaration, so a listing in
  -- whatever order the filesystem holds it would make every re-read of an unchanged directory
  -- produce a different page.
  assertEqual "the files were not the ones there, at the paths they are at, in order"
    [("a.md", "first"), ("sub/b.md", "second")] (pageFiles $ piPrgm parsed)

-- | The name is the one thing a reader has to write by hand, so the page says it.
testSaysItsName :: TestTree
testSaysItsName = testCase "the page says the name to write to reach it" $ do
  dir <- caseDir "saysq5Fits-name"
  writeFile (dir </> "a.md") "first"
  parsed <- readDir dir
  let said = unwords $ pageComments $ piPrgm parsed
  assertBool ("the page did not say the name to write, only: " ++ said)
    (fileDirName dir `isInfixOf` said)

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

-- | A file which is not text is left out, and the page says which.
testNotText :: TestTree
testNotText = testCase "a file which is not text is left out and said to be" $ do
  dir <- caseDir "nottext"
  writeFile (dir </> "a.md") "first"
  -- Written as the bytes they are rather than as a string, since a string of any characters at all
  -- is written as text a read would decode back. These bytes are not text in any encoding the read
  -- will try: 0xFF and 0xFE never appear in UTF-8.
  BS.writeFile (dir </> "logo.png") (BS.pack [0x89, 0x50, 0x4E, 0x47, 0xFF, 0xFE, 0x00])
  parsed <- readDir dir

  assertEqual "the file which is not text was carried anyway"
    [("a.md", "first")] (pageFiles $ piPrgm parsed)
  assertBool "the page did not say which file it could not carry"
    (any ("logo.png" `isInfixOf`) (pageComments $ piPrgm parsed))

-- | A dotted entry is walked past, as everywhere else a tree is walked.
testDotted :: TestTree
testDotted = testCase "a dotted entry is passed over" $ do
  dir <- caseDir "dotted"
  createDirectoryIfMissing True (dir </> ".git")
  writeFile (dir </> ".git" </> "HEAD") "ref: refs/heads/main"
  writeFile (dir </> ".hidden") "not a file of the directory"
  writeFile (dir </> "a.md") "first"
  parsed <- readDir dir
  assertEqual "a dotted entry was read as a file of the directory"
    [("a.md", "first")] (pageFiles $ piPrgm parsed)

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

-- | A directory holding both is entered through the one that says what it is.
testDirCtcWins :: TestTree
testDirCtcWins = testCase "dir.ctc is the page a directory is entered through, over main.ct" $ do
  dir <- caseDir "entry"
  writeFile (dir </> "main.ct") "import \"core\"\nfromMain = 1\n"
  onlyMain <- dirParse dir
  assertEqual "a directory holding only a main.ct was not entered through it"
    [dir </> "main.ct"] (importPaths $ piPrgm onlyMain)

  writeFile (dir </> "dir.ctc") "import \"core\"\nfromCtc = 1\n"
  both <- dirParse dir
  assertEqual "a directory holding both was not entered through its dir.ctc"
    [dir </> "dir.ctc"] (importPaths $ piPrgm both)
  where
    importPaths (RawPrgm imports _) = mapMaybe (rawPath . impRaw) imports
    rawPath (RawCExpr _ (CStr p)) = Just p
    rawPath _                     = Nothing

fileDirTests :: TestTree
fileDirTests = testGroup "FileDirTests" [
  testDeclares,
  testSaysItsName,
  testRecordsReads,
  testFileEditIsRead,
  testNotText,
  testDotted,
  testName,
  testDirCtcWins
                                        ]
