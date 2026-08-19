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

import           Data.Graph          (graphFromEdges)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.IORef          (modifyIORef', newIORef, readIORef)
import           Data.List           (isPrefixOf, sort)
import           Data.Time.Clock     (UTCTime, addUTCTime, getCurrentTime)
import           Semantics           (ctssCachePath, testCtssConfig)
import           Semantics.Prgm      (FileImport, impDisp)
import           Syntax.Ct.Prgm      (ReadPage (..), newReadEnv, scanTree,
                                      tsFiles)
import           Syntax.Parsers      (canonicalImport, finishReading,
                                      mkRawImportStr, newReadQueue, newReading,
                                      readFiles, readPathWrittenIn, readPaths,
                                      readQueued, rqRestart, unchangedPages)
import           System.Directory    (createDirectoryIfMissing,
                                      getTemporaryDirectory, removeFile,
                                      removePathForcibly, renameFile,
                                      setModificationTime)
import           System.FilePath     ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils               (GraphData, graphToNodes)

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

-- | What the page shown at a path imports, by the paths those pages are shown at.
--
-- Which imports a note has is what resolving its wiki links produced, so this is what the pages of
-- a vault say about each other. Confined to one directory as 'pagesUnder' is, to leave the standard
-- library out.
importsUnder :: FilePath -> FilePath -> GraphData ReadPage FileImport -> [FilePath]
importsUnder dir path pages = sort [imp | (page, name, _) <- graphToNodes pages
                                        , impDisp name == Just path
                                        , Just imp <- map impDisp (rpImports page)
                                        , dir `isPrefixOf` imp]

-- | The pages under a directory a write to these paths reaches.
--
-- A read which is TOLD what has been written to looks at the pages the write reached and gives back
-- every other one untouched ('CtService.ctssUnchangedOf'), so this is the half of that decision the
-- cases above cannot show: they run over a read which looks at everything, and 'readAgain' is the
-- other half, what looking then decides. Reaching a page is not reading it again — a note written
-- to reaches the notes linking it, and looking says none of them changed.
--
-- Worth asserting on its own because being told is strictly less than looking. A path written to is
-- a path created, changed, deleted or renamed all the same, and a directory renamed is reported as
-- the directory rather than as the notes under it.
writtenTo :: FilePath -> [FilePath] -> GraphData ReadPage FileImport -> [FilePath]
writtenTo dir written pages = sort [path | (page, name, _) <- graphToNodes pages
                                         , Just path <- [impDisp name]
                                         , dir `isPrefixOf` path
                                         , any (readPathWrittenIn (S.fromList written)) (readPaths page)]

-- | Read what a name reaches, giving back every page still as the last read left it.
--
-- Reads every page it reads in full: 'testCtssConfig' keeps no record for a page to be answered
-- out of, so there is none for a read to be told to pass over. What a record does to these cases is
-- "Syntax.ObsidianCacheTests".
readOnce :: GraphData ReadPage FileImport -> String -> IO (GraphData ReadPage FileImport)
readOnce prev name = do
  unchanged <- unchangedPages [(prgmName, page) | (page, prgmName, _) <- graphToNodes prev]
  readFiles testCtssConfig S.empty unchanged [mkRawImportStr name]

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
--
-- The vault is the only page read from all of that. A note is read from its own text and from the
-- notes it links to, so a note added is the vault's business and not every note's — which is what
-- makes a change to a vault cost the change rather than the vault.
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
  assertEqual "a note the added one is nothing to was read again"
    [dir, dir </> "sub" </> "Two.md"] (readAgain dir first added)

-- | The limitation the above buys: a note added does not resolve a link that already resolved to
-- nothing, until the note writing that link is read again for a reason of its own.
--
-- Noticing it would be a note depending on every name it links to and cannot find, which can only
-- be answered against the whole vault — the cost the reads are shaped to avoid. So it is taken as a
-- limitation rather than paid for, and it is asserted here so that it stays a decision.
testDanglingLink :: TestTree
testDanglingLink = testCase "a note added does not resolve a link which already resolved to nothing" $ do
  dir <- caseDir "dangling"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  writeLater now (dir </> "One.md") "Links to [[Two]].\n"
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  first <- readOnce noPages vault
  assertEqual "the link was resolved before the note it names was written" [] (importsUnder dir (dir </> "One.md") first)

  let later = addUTCTime 60 now
  writeLater later (dir </> "Two.md") "A note.\n"
  setModificationTime dir later
  added <- readOnce first vault
  assertEqual "the note added is not a page" [dir, dir </> "One.md", dir </> "Two.md"] (pagesUnder dir added)
  assertEqual "the dangling link was resolved without the note writing it being read again"
    [] (importsUnder dir (dir </> "One.md") added)

  writeLater (addUTCTime 120 now) (dir </> "One.md") "Links to [[Two]], still.\n"
  written <- readOnce added vault
  assertEqual "the note read again did not resolve its link"
    [dir </> "Two.md"] (importsUnder dir (dir </> "One.md") written)

-- | A note deleted is read again by the notes which link to it, which they must be: a page's
-- imports are walked whether or not the page itself was read again, so a note left importing a note
-- that is no longer there is a read which fails rather than a link which dangles.
testNoteRemoved :: TestTree
testNoteRemoved = testCase "a note deleted is read again by the notes which link to it" $ do
  dir <- caseDir "removed-note"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  writeLater now (dir </> "One.md") "Links to [[Two]].\n"
  writeLater now (dir </> "Three.md") "A note which links to nothing.\n"
  writeLater now (dir </> "Two.md") "A note.\n"
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  first <- readOnce noPages vault
  assertEqual "the read did not read the vault and its notes"
    [dir, dir </> "One.md", dir </> "Three.md", dir </> "Two.md"] (pagesUnder dir first)
  assertEqual "the note's link was not resolved" [dir </> "Two.md"] (importsUnder dir (dir </> "One.md") first)

  let later = addUTCTime 60 now
  removeFile (dir </> "Two.md")
  setModificationTime dir later
  removed <- readOnce first vault
  assertEqual "the note deleted is still a page"
    [dir, dir </> "One.md", dir </> "Three.md"] (pagesUnder dir removed)
  assertEqual "the note linking to the deleted one was not read again, or another note was"
    [dir, dir </> "One.md"] (readAgain dir first removed)
  assertEqual "the note linking to the deleted one still imports it"
    [] (importsUnder dir (dir </> "One.md") removed)

-- | A note moved is read again by the notes which link to it, as the path they import it by is no
-- longer the path it is at. A note which links to neither is not.
testNoteMoved :: TestTree
testNoteMoved = testCase "a note moved is read again by the notes which link to it" $ do
  dir <- caseDir "moved"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  createDirectoryIfMissing True (dir </> "sub")
  writeLater now (dir </> "One.md") "Links to [[Two]].\n"
  writeLater now (dir </> "Three.md") "A note which links to nothing.\n"
  writeLater now (dir </> "Two.md") "A note.\n"
  setModificationTime (dir </> "sub") now
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  first <- readOnce noPages vault
  assertEqual "the note's link was not resolved" [dir </> "Two.md"] (importsUnder dir (dir </> "One.md") first)

  let later = addUTCTime 60 now
  renameFile (dir </> "Two.md") (dir </> "sub" </> "Two.md")
  setModificationTime (dir </> "sub" </> "Two.md") later
  setModificationTime (dir </> "sub") later
  setModificationTime dir later
  moved <- readOnce first vault
  assertEqual "the note moved is not a page where it was moved to"
    [dir, dir </> "One.md", dir </> "Three.md", dir </> "sub" </> "Two.md"] (pagesUnder dir moved)
  assertEqual "the note linking to the moved one was not read again, or another note was"
    [dir, dir </> "One.md", dir </> "sub" </> "Two.md"] (readAgain dir first moved)
  assertEqual "the note linking to the moved one does not import it where it now is"
    [dir </> "sub" </> "Two.md"] (importsUnder dir (dir </> "One.md") moved)

-- | One read walks a tree once, however many pages are read out of it.
--
-- Asserted by changing the tree under the read, which is the only way sharing the walk can be seen
-- from outside — and which is the same thing that makes it safe. What a page's parse resolved
-- against and what its read records as having found are then the one walk, so a change the read did
-- not see is one the NEXT read finds, rather than one recorded as though the read had seen it.
testScanTreeOnce :: TestTree
testScanTreeOnce = testCase "a read walks a tree once, however many pages are read out of it" $ do
  dir <- caseDir "scan"
  writeFile (dir </> "One.md") "A note.\n"
  env <- newReadEnv
  assertEqual "the walk did not find the note" [dir </> "One.md"] . tsFiles =<< scanTree env dir

  writeFile (dir </> "Two.md") "Another note.\n"
  assertEqual "the read walked the tree a second time" [dir </> "One.md"] . tsFiles =<< scanTree env dir
  fresh <- newReadEnv
  assertEqual "the next read did not walk the tree"
    [dir </> "One.md", dir </> "Two.md"] . tsFiles =<< scanTree fresh dir

-- | Which pages a write reaches, for a read which is told what was written to rather than left to
-- look. The case only this one can be given is the last, a directory renamed — the notes in it are
-- written to as far as any reader of them is concerned, and yet nothing says so but the directory.
testWrittenTo :: TestTree
testWrittenTo = testCase "a write reaches the notes read from what was written to" $ do
  dir <- caseDir "written-to"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  createDirectoryIfMissing True (dir </> "sub")
  writeLater now (dir </> "One.md") "Links to [[Two]].\n"
  writeLater now (dir </> "Three.md") "A note which links to nothing.\n"
  writeLater now (dir </> "sub" </> "Two.md") "A note.\n"
  setModificationTime (dir </> "sub") now
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  pages <- readOnce noPages vault
  assertEqual "the note's link was not resolved"
    [dir </> "sub" </> "Two.md"] (importsUnder dir (dir </> "One.md") pages)

  assertEqual "a write to a note does not reach the note which links it"
    [dir, dir </> "One.md", dir </> "sub" </> "Two.md"] (writtenTo dir [dir </> "sub" </> "Two.md"] pages)
  assertEqual "a write to a note reaches a note which does not link it"
    [dir, dir </> "Three.md"] (writtenTo dir [dir </> "Three.md"] pages)
  assertEqual "renaming the directory a note is in does not reach it or the note which links it"
    [dir, dir </> "One.md", dir </> "sub" </> "Two.md"] (writtenTo dir [dir </> "sub"] pages)

-- | A note is read from its own text and from which notes the vault holds, and writing to a note
-- changes neither for any other note: a file being written to does not touch the directory holding
-- it. So the note written to is read again and no other is.
testNoteWritten :: TestTree
testNoteWritten = testCase "a note written to is the only page read again" $ do
  dir <- caseDir "note"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  writeLater now (dir </> "One.md") "Links to [[Two]].\n"
  writeLater now (dir </> "Two.md") "A note.\n"
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  first <- readOnce noPages vault
  assertEqual "the read did not read the vault and both notes"
    [dir, dir </> "One.md", dir </> "Two.md"] (pagesUnder dir first)

  writeLater (addUTCTime 60 now) (dir </> "Two.md") "A note, written to.\n"
  written <- readOnce first vault
  assertEqual "the note written to was not the one page read again"
    [dir </> "Two.md"] (readAgain dir first written)

-- | A vault's own configuration is not notes of it, and Obsidian writes it as the vault is used. A
-- page read from a tree is read from what a reading of that tree walks, so writing the
-- configuration must read nothing again.
testVaultConfig :: TestTree
testVaultConfig = testCase "a vault's own configuration is not read from" $ do
  dir <- caseDir "config"
  now <- getCurrentTime
  createDirectoryIfMissing True (dir </> ".obsidian")
  writeLater now (dir </> "One.md") "A note.\n"
  setModificationTime (dir </> ".obsidian") now
  setModificationTime dir now
  let vault = "obsidian(\"" ++ dir ++ "\")"
  first <- readOnce noPages vault
  assertEqual "the read did not read the vault and its note" [dir, dir </> "One.md"] (pagesUnder dir first)

  let later = addUTCTime 60 now
  writeLater later (dir </> ".obsidian" </> "workspace.json") "{}\n"
  setModificationTime (dir </> ".obsidian") later
  used <- readOnce first vault
  assertEqual "writing the vault's configuration read a page again" [] (readAgain dir first used)

-- | A read hands each page over as it reads it, and reads the pages it has reached oldest first.
--
-- The order is what makes the difference visible. Reading from a queue reads @a@, then both of what
-- @a@ imports, then what those import; reading from a stack reads @a@, then the whole of its first
-- import before the second — so @d@ comes before @c@ rather than after it. It is a queue so that a
-- page reached first is read first, which is an order whoever is watching the read can be told
-- about, and it is what a service takes the pages up in ('CtService.ctssReadOf').
--
-- @d@ is imported by both @b@ and @c@ and read once, which is the queue keeping a page it already
-- holds out rather than reading it twice.
--
-- The pages of the standard library are read too and left out here, as everywhere else in this
-- module: every file imports it, and what this case is about is the four files written for it.
testQueuedOrder :: TestTree
testQueuedOrder = testCase "a read hands each page over as it reads it, oldest reached first" $ do
  dir <- caseDir "queued"
  now <- getCurrentTime
  writeLater now (dir </> "a.ct") "import \"b.ct\"\nimport \"c.ct\"\na = 1\n"
  writeLater now (dir </> "b.ct") "import \"d.ct\"\nb = 2\n"
  writeLater now (dir </> "c.ct") "import \"d.ct\"\nc = 3\n"
  writeLater now (dir </> "d.ct") "d = 4\n"
  handed <- readHandingOver (dir </> "a.ct") (\imp _ -> return imp)
  assertEqual "the pages were not read in the order they were reached"
    [dir </> "a.ct", dir </> "b.ct", dir </> "c.ct", dir </> "d.ct"]
    [path | imp <- handed, Just path <- [impDisp imp], dir `isPrefixOf` path]

-- | Read what a name reaches, keeping what the given action makes of each page as it is handed over.
--
-- The read a service makes, without the service: 'Syntax.Parsers.readQueued' with a queue of its
-- own and each page acted on as it arrives, which is 'CtService.ctssReadOf' but for what it does
-- with them. Reads every page it reads in full, as 'readOnce' does and for the same reason.
readHandingOver :: String -> (FileImport -> ReadPage -> IO a) -> IO [a]
readHandingOver name found = do
  reading <- newReading (ctssCachePath testCtssConfig)
  queue <- newReadQueue
  rqRestart queue =<< mapM (canonicalImport testCtssConfig Nothing) [mkRawImportStr name]
  acc <- newIORef []
  readQueued testCtssConfig reading S.empty H.empty queue $ \imp page ->
    found imp page >>= \x -> modifyIORef' acc (x :)
  finishReading reading
  reverse <$> readIORef acc

rereadTests :: TestTree
rereadTests = testGroup "Reread" [testWritten, testListed, testVault, testDanglingLink, testNoteRemoved, testNoteMoved, testNoteWritten, testVaultConfig, testScanTreeOnce, testWrittenTo, testQueuedOrder]
