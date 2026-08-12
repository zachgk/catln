-- | Tests for reading files a second time: what a read reads again, and what it gives back.
--
-- A read gives back every page whose files are as it left them ('Syntax.Parsers.unchangedPages'),
-- which is what lets a doc server read on every request rather than choosing between answering from
-- a read that may be stale and reading the whole program to answer at all. What it must never do is
-- give back a page something it was read from has changed under, so each case here changes one
-- thing and asks which pages the next read read.
--
-- Read over files written for the test rather than over the ones in the repository, because the
-- cases are changes: a file written to, a file added to a directory, a note added to a vault.
module Syntax.RereadTests where

import           Data.Graph       (graphFromEdges)
import           Data.List        (isPrefixOf, sort)
import           Data.Time.Clock  (UTCTime, addUTCTime, getCurrentTime)
import           Semantics        (testCtssConfig)
import           Semantics.Prgm   (FileImport, impDisp)
import           Syntax.Parsers   (ReadPage (..), mkRawImportStr, readFiles,
                                   unchangedPages)
import           System.Directory (createDirectoryIfMissing,
                                   getTemporaryDirectory, removeFile,
                                   removePathForcibly, setModificationTime)
import           System.FilePath  ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils            (GraphData, graphToNodes)

-- | The pages of a read written under one directory, by the path they are shown at.
--
-- Everything else a read reaches is left out, which is the standard library: every file imports it,
-- and what these cases are about is the handful of files written for them.
pagesUnder :: FilePath -> GraphData ReadPage FileImport -> [FilePath]
pagesUnder dir pages = sort [path | (_, name, _) <- graphToNodes pages
                                  , Just path <- [impDisp name]
                                  , dir `isPrefixOf` path]

-- | Which of the pages under a directory the second of two reads read again.
--
-- Read times identify the read rather than the files: a page read again is timed afresh before it
-- is parsed, so times which came back unchanged are the ones the page was handed back with.
readAgain :: FilePath -> GraphData ReadPage FileImport -> GraphData ReadPage FileImport -> [FilePath]
readAgain dir before now = sort [path | (page, name, _) <- graphToNodes now
                                      , Just path <- [impDisp name]
                                      , dir `isPrefixOf` path
                                      , maybe True (\was -> rpReads was /= rpReads page) (lookup name held)]
  where
    held = [(name, page) | (page, name, _) <- graphToNodes before]

-- | Read what a name reaches, giving back every page still as the last read left it.
readOnce :: GraphData ReadPage FileImport -> String -> IO (GraphData ReadPage FileImport)
readOnce prev name = do
  unchanged <- unchangedPages [(prgmName, page) | (page, prgmName, _) <- graphToNodes prev]
  readFiles testCtssConfig unchanged [mkRawImportStr name]

-- | No pages at all: a first read has nothing to give back.
noPages :: GraphData ReadPage FileImport
noPages = graphFromEdges []

-- | Write a file, dated definitely later than anything read before it.
--
-- The date is set rather than left to the clock because a filesystem which keeps modification times
-- to the second has nothing to show for a file written in the same second it was read, which would
-- have these cases pass or fail on how fast the machine is.
writeLater :: UTCTime -> FilePath -> String -> IO ()
writeLater later path src = do
  writeFile path src
  setModificationTime path later

-- | A directory to write the files of one case in, emptied first.
caseDir :: String -> IO FilePath
caseDir name = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-reread" </> name
  removePathForcibly dir
  createDirectoryIfMissing True dir
  return dir

-- | A file written to is read again, and nothing else is.
testWritten :: TestTree
testWritten = testCase "a file written to is the only page read again" $ do
  dir <- caseDir "written"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"core\"\nimport \"b.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "import \"core\"\nb = 2\n"
  first <- readOnce noPages (dir </> "a.ct")
  assertEqual "the read did not read both files" [dir </> "a.ct", dir </> "b.ct"] (pagesUnder dir first)

  unchanged <- readOnce first (dir </> "a.ct")
  assertEqual "a read of unchanged files read a page again" [] (readAgain dir first unchanged)

  writeLater (addUTCTime 60 now) (dir </> "b.ct") "import \"core\"\nb = 3\n"
  changed <- readOnce unchanged (dir </> "a.ct")
  assertEqual "the written file was not the one page read again" [dir </> "b.ct"] (readAgain dir unchanged changed)

-- | A directory is read from its listing, so a file appearing in one is a page and a file leaving
-- one is not.
testListed :: TestTree
testListed = testCase "a file added to a directory is found, and one removed is dropped" $ do
  dir <- caseDir "listed"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"core\"\na = 1\n"
  setModificationTime dir now
  first <- readOnce noPages dir
  assertEqual "the read did not read the directory and its file" [dir, dir </> "a.ct"] (pagesUnder dir first)

  let later = addUTCTime 60 now
  writeLater later (dir </> "b.ct") "import \"core\"\nb = 2\n"
  setModificationTime dir later
  added <- readOnce first dir
  assertEqual "the file added to the directory is not a page" [dir, dir </> "a.ct", dir </> "b.ct"] (pagesUnder dir added)
  assertEqual "the directory the file was added to was not read again" [dir, dir </> "b.ct"] (readAgain dir first added)

  let laterStill = addUTCTime 120 now
  removeFile (dir </> "b.ct")
  setModificationTime dir laterStill
  removed <- readOnce added dir
  assertEqual "the file removed from the directory is still a page" [dir, dir </> "a.ct"] (pagesUnder dir removed)

-- | A vault is read from every directory under it, which is the case a page timed by its own file
-- alone could not see: a note added in a subdirectory does not touch the vault directory at all.
testVault :: TestTree
testVault = testCase "a note added under a vault is found from a subdirectory of it" $ do
  dir <- caseDir "vault"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  createDirectoryIfMissing True (dir </> "sub")
  writeLater now (dir </> "One.md") "Links to [[Two]].\n"
  setModificationTime (dir </> "sub") now
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  first <- readOnce noPages vault
  assertEqual "the read did not read the vault and its note" [dir, dir </> "One.md"] (pagesUnder dir first)

  let later = addUTCTime 60 now
  writeLater later (dir </> "sub" </> "Two.md") "A note.\n"
  setModificationTime (dir </> "sub") later
  added <- readOnce first vault
  assertEqual "the note added under the vault is not a page"
    [dir, dir </> "One.md", dir </> "sub" </> "Two.md"] (pagesUnder dir added)
  -- Every note is read again, not only the new one: a note's links resolve against the whole vault
  -- listing, which is what lets One.md's link to Two find it now that Two is written.
  assertEqual "the notes of the vault were not read again"
    [dir, dir </> "One.md", dir </> "sub" </> "Two.md"] (readAgain dir first added)

rereadTests :: TestTree
rereadTests = testGroup "Reread" [testWritten, testListed, testVault]
