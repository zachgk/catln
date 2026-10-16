-- | User space held in Haskell for want of a surface: the vault cache is the Obsidian importer's,
-- but every case here reads a vault twice with a change to the files between, which no test of
-- the Catln suite can do. Moves with the runtime.
--
-- | Tests for reading an Obsidian vault out of what an earlier run recorded of it.
--
-- A run records which notes each note links to, so the run after it is answered out of that rather
-- than by reading every note again ('Syntax.Obsidian.Cache'). A page answered that way is in the
-- graph with its imports and without its program, and is read when something asks what is in it.
--
-- So there are two things to hold it to. The graph a warm run reads must be the graph a cold run
-- reads, or a record makes the compiler read a different program from the one on disk; and a record
-- must stop being answered with the moment it stops describing what is on disk, which the notes'
-- own files are not enough to say. Each case here changes one thing and asks what the next run
-- read.
module Syntax.ObsidianCacheTests where

import           CRes                  (CRes (..), runCResT)
import           CtService             (CTSS, ctssBaseFiles, ctssCached,
                                        ctssKeys, ctssLoad, ctssRead,
                                        ctssRootPage, ctssStale, getSource,
                                        setSource)
import           Data.Graph            (graphFromEdges)
import qualified Data.HashSet          as S
import           Data.List             (isInfixOf, isPrefixOf, isSuffixOf, sort)
import           Data.Maybe            (isJust, isNothing)
import           Data.Time.Clock       (UTCTime, addUTCTime, getCurrentTime)
import           Eval.Common           (SSF (..))
import           Semantics             (CTSSConfig (..), testCtssConfig)
import           Semantics.Prgm        (FileImport, impDisp)
import           Syntax.Ct.Prgm        (ReadPage (..))
import           Syntax.Obsidian.Cache (vaultCacheFile)
import           Syntax.Parsers        (mkDesCanonicalImportStr, mkRawImportStr,
                                        readFiles, unchangedPages)
import           System.Directory      (createDirectoryIfMissing, doesPathExist,
                                        getTemporaryDirectory, removeFile,
                                        removePathForcibly, renameFile,
                                        setModificationTime)
import           System.FilePath       (takeDirectory, (</>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Utils                 (GraphData, graphToNodes)

-- | A vault written for one case, with a cache root of its own, both emptied first.
--
-- A root per case because a record is a thing shared between runs: two cases sharing one would be
-- two cases whose answers depended on which ran first.
caseVault :: String -> IO (FilePath, CTSSConfig)
caseVault name = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-obsidian-cache" </> name
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "vault" </> ".obsidian")
  createDirectoryIfMissing True (dir </> "cache")
  return (dir </> "vault", testCtssConfig{ctssCachePath = Just (dir </> "cache")})

-- | Write a note, dated definitely earlier than the run which reads it.
--
-- Dated rather than left to the clock for the reason 'Syntax.RereadTests.writeLater' is, and
-- earlier rather than later because a record does not hold for a note whose time is not strictly
-- before the reading which recorded it — a note written in the same instant as the run is one the
-- run cannot vouch for.
writeNote :: UTCTime -> FilePath -> String -> IO ()
writeNote at path src = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path src
  setModificationTime path at

-- | Read what a vault reaches, reading in full the notes named and answering for the rest out of
-- whatever the last run recorded.
--
-- The vault is named as one outright, rather than left to be guessed from what is on disk, so that
-- what these cases do to a vault does not also rest on it being recognised as one. That it is
-- recognised is 'testVaultNamedByPath' below.
readVault :: CTSSConfig -> [FilePath] -> GraphData ReadPage FileImport -> FilePath -> IO (GraphData ReadPage FileImport)
readVault config needed prev vault = do
  unchanged <- unchangedPages [(prgmName, page) | (page, prgmName, _) <- graphToNodes prev]
  -- The notes are named to the read as the pages they are, which is what a caller asking for a page
  -- holds ('CtService.ctssReadOf'), rather than as the paths they are shown at: a page is the import
  -- which names it, and two pages can be shown at the one path.
  inFull <- mapM (mkDesCanonicalImportStr config) needed
  readFiles config (S.fromList inFull) unchanged [mkRawImportStr $ "obsidian(\"" ++ vault ++ "\")"]

-- | A first run has nothing an earlier one left to give back.
noPages :: GraphData ReadPage FileImport
noPages = graphFromEdges []

-- | The pages under a directory, by the path they are shown at.
pagesUnder :: FilePath -> GraphData ReadPage FileImport -> [FilePath]
pagesUnder dir pages = sort [path | (_, name, _) <- graphToNodes pages
                                  , Just path <- [impDisp name]
                                  , dir `isPrefixOf` path]

-- | The notes of a vault whose program was read, by the path they are shown at.
--
-- The notes rather than the pages: the vault's own page is read every time, since which notes it
-- holds is read out of the listings under it and that walk is what says whether anything has been
-- added or taken away. What a record saves is the notes.
notesRead :: FilePath -> GraphData ReadPage FileImport -> [FilePath]
notesRead dir pages = sort [path | (page, name, _) <- graphToNodes pages
                                 , Just path <- [impDisp name]
                                 , dir `isPrefixOf` path
                                 , ".md" `isSuffixOf` path
                                 , isJust (rpPrgm page)]

-- | What the page shown at a path imports, by the paths those pages are shown at.
importsUnder :: FilePath -> FilePath -> GraphData ReadPage FileImport -> [FilePath]
importsUnder dir path pages = sort [imp | (page, name, _) <- graphToNodes pages
                                        , impDisp name == Just path
                                        , Just imp <- map impDisp (rpImports page)
                                        , dir `isPrefixOf` imp]

-- | A vault of three notes: one linking to both others, one linking back, one linking to nothing.
threeNotes :: UTCTime -> FilePath -> IO ()
threeNotes at vault = do
  writeNote at (vault </> "Welcome.md") "Start at [[Notes/Nested]] and then [[Tasks]].\n"
  writeNote at (vault </> "Notes" </> "Nested.md") "Back to [[Welcome]].\n"
  writeNote at (vault </> "Tasks.md") "- [ ] a task\n"

-- | A run answered out of a record reads the same graph, and reads none of the notes.
testWarmGraphMatches :: TestTree
testWarmGraphMatches = testCase "a recorded vault reads the same graph without reading a note" $ do
  (vault, config) <- caseVault "warm"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault

  cold <- readVault config [] noPages vault
  assertEqual "the first run did not read every note"
    [vault </> "Notes/Nested.md", vault </> "Tasks.md", vault </> "Welcome.md"] (notesRead vault cold)
  recorded <- doesPathExist (vaultCacheFile (takeDirectory vault </> "cache") vault)
  assertBool "the first run recorded nothing of the vault" recorded

  -- A second run from nothing, which is what starting the compiler again is.
  warm <- readVault config [] noPages vault
  assertEqual "the pages of the two runs differ" (pagesUnder vault cold) (pagesUnder vault warm)
  assertEqual "a note was read though the record still held" [] (notesRead vault warm)
  -- The imports are what the record is for: a page held without its program still says what it
  -- imports, or the graph would be the vault with its notes cut off from each other.
  assertEqual "the recorded imports are not the ones the read found"
    (importsUnder vault (vault </> "Welcome.md") cold)
    (importsUnder vault (vault </> "Welcome.md") warm)
  assertEqual "the note's imports were not recorded"
    [vault </> "Notes/Nested.md", vault </> "Tasks.md"]
    (importsUnder vault (vault </> "Welcome.md") warm)

-- | Asking for a note reads that note and leaves the rest of the vault as it was.
testReadsWhatIsAskedFor :: TestTree
testReadsWhatIsAskedFor = testCase "a note asked for is read, and no other note is" $ do
  (vault, config) <- caseVault "asked"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  warm <- readVault config [vault </> "Welcome.md"] noPages vault
  assertEqual "the note asked for was not the one note read" [vault </> "Welcome.md"] (notesRead vault warm)

-- | A note edited between two runs is read again, whatever was recorded of it.
testWrittenNoteIsReadAgain :: TestTree
testWrittenNoteIsReadAgain = testCase "a note written to is read again and its record replaced" $ do
  (vault, config) <- caseVault "written"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  writeNote (addUTCTime (-30) now) (vault </> "Welcome.md") "Now only [[Tasks]].\n"
  warm <- readVault config [] noPages vault
  assertEqual "the written note was not the one note read" [vault </> "Welcome.md"] (notesRead vault warm)
  assertEqual "the note kept the links it used to have"
    [vault </> "Tasks.md"] (importsUnder vault (vault </> "Welcome.md") warm)

-- | A note rewritten to the same length in the same instant is read again.
--
-- The case a modification time alone cannot answer: a filesystem which keeps times to the second
-- has nothing to show for two writes inside one. The length is what catches this one; two writes
-- agreeing on both are what the record cannot catch, and are rare enough to be worth living with.
testSameTimeDifferentLength :: TestTree
testSameTimeDifferentLength = testCase "a note rewritten at the same time to a different length is read again" $ do
  (vault, config) <- caseVault "samestamp"
  now <- getCurrentTime
  let at = addUTCTime (-60) now
  threeNotes at vault
  _ <- readVault config [] noPages vault

  writeNote at (vault </> "Welcome.md") "Only [[Tasks]] now, and at the very same moment.\n"
  warm <- readVault config [] noPages vault
  assertEqual "the note was answered for out of a record of what it used to be"
    [vault </> "Welcome.md"] (notesRead vault warm)

-- | A note deleted between two runs does not leave a note linking to it importing what is gone.
--
-- The case the notes' own files cannot answer. Welcome.md is untouched, so what it was read from
-- says nothing has changed — and yet what its links resolved to is decided by what the vault holds.
-- A record answered with here would import a file which is not there, and a read walks on to every
-- import, so one deleted note would fail the whole read rather than dangle.
testLinkedNoteDeleted :: TestTree
testLinkedNoteDeleted = testCase "a note deleted is dropped by the notes which linked to it" $ do
  (vault, config) <- caseVault "deleted"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  removeFile (vault </> "Tasks.md")
  warm <- readVault config [] noPages vault
  assertEqual "the vault still holds the deleted note"
    [vault, vault </> "Notes/Nested.md", vault </> "Welcome.md"] (pagesUnder vault warm)
  assertEqual "a note was left importing the note that was deleted"
    [vault </> "Notes/Nested.md"] (importsUnder vault (vault </> "Welcome.md") warm)

-- | A note moved is the same case as one deleted, since what a link resolved to is a path.
testLinkedNoteMoved :: TestTree
testLinkedNoteMoved = testCase "a note moved is resolved to where it now is" $ do
  (vault, config) <- caseVault "moved"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  createDirectoryIfMissing True (vault </> "Notes")
  renameFile (vault </> "Tasks.md") (vault </> "Notes" </> "Tasks.md")
  warm <- readVault config [] noPages vault
  assertEqual "the note was left importing where the note used to be"
    [vault </> "Notes/Nested.md", vault </> "Notes/Tasks.md"]
    (importsUnder vault (vault </> "Welcome.md") warm)

-- | A note the vault no longer holds is forgotten, so a record does not grow forever.
testRemovedNoteIsForgotten :: TestTree
testRemovedNoteIsForgotten = testCase "a note the vault no longer holds is dropped from its record" $ do
  (vault, config) <- caseVault "forgotten"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  removeFile (vault </> "Tasks.md")
  -- Welcome.md linked to it, so the run above reads Welcome.md again and rewrites the record.
  _ <- readVault config [] noPages vault
  recorded <- readFile (vaultCacheFile (takeDirectory vault </> "cache") vault)
  assertBool "the record still holds the note the vault lost" (not $ "Tasks.md" `isInfixOf` recorded)

-- | A read with no cache path records nothing and reads every page, which is every test but these.
testNoCacheReadsEverything :: TestTree
testNoCacheReadsEverything = testCase "a compiler with nowhere to record reads every note, every run" $ do
  (vault, config) <- caseVault "nocache"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault

  let uncached = config{ctssCachePath = Nothing}
  _ <- readVault uncached [] noPages vault
  warm <- readVault uncached [] noPages vault
  assertEqual "a note was answered for by a compiler which records nothing"
    [vault </> "Notes/Nested.md", vault </> "Tasks.md", vault </> "Welcome.md"] (notesRead vault warm)
  written <- doesPathExist (vaultCacheFile (takeDirectory vault </> "cache") vault)
  assertBool "a compiler with nowhere to record wrote a record" (not written)

-- | A page held as its imports alone is a page with no source to show.
testDeferredHasNoSource :: TestTree
testDeferredHasNoSource = testCase "a note answered for out of a record has no source until it is read" $ do
  (vault, config) <- caseVault "source"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  warm <- readVault config [] noPages vault
  let sourceOf pages = [rpSource page | (page, name, _) <- graphToNodes pages, impDisp name == Just (vault </> "Welcome.md")]
  assertBool "a note nothing read showed a source" (all isNothing (sourceOf warm))

  read' <- readVault config [vault </> "Welcome.md"] noPages vault
  assertBool "a note that was read showed no source" (all isJust (sourceOf read'))

-- | A vault is the same page whether it is named as one or named by its path.
--
-- A page is identified by the import which names it, and what parser a path is read with is guessed
-- from what is on disk. Guessing a plain directory for a vault makes the vault a second page at the
-- one display path — so the page a doc server says it opens at ('CtService.ctssRootPage', which
-- answers with a path) is not the page it holds, and neither that page nor an annotation scoped to
-- it can be asked for.
testVaultNamedByPath :: TestTree
testVaultNamedByPath = testCase "a vault named by its path is the vault the service holds" $ do
  (vault, config) <- caseVault "named"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault

  ctss <- vaultService config vault
  root <- ctssRootPage ctss
  assertEqual "the page the service opens at is not the vault" (Just vault) root

  byPath <- mkDesCanonicalImportStr config vault
  keys <- ctssKeys ctss
  assertBool "the vault named by its path is not a page the service holds" (byPath `elem` keys)

-- | The notes of a vault a service holds the program of.
notesHeld :: FilePath -> CTSS -> IO [FilePath]
notesHeld dir ctss = do
  names <- ctssKeys ctss
  held <- mapM (\name -> (name,) <$> ctssCached ssfRaw name ctss) names
  return $ sort [path | (name, Just _) <- held
                      , Just path <- [impDisp name]
                      , dir `isPrefixOf` path
                      , ".md" `isSuffixOf` path]

-- | A service reading a recorded vault.
vaultService :: CTSSConfig -> FilePath -> IO CTSS
vaultService config vault = ctssBaseFiles config ["obsidian(\"" ++ vault ++ "\")"]

-- | A note whose program has been read is never given back unread by a later read.
--
-- The invariant everything else here rests on. A read decides for itself which pages to answer for
-- out of a record, and a page it answers for that way has no program — so a read which did that to
-- a page whose program the service already held would take that program away, and with it
-- everything built from it. Every read therefore reads in full whatever it already holds, whoever
-- asked ('CtService.ctssReadOf').
testLoadedStaysLoaded :: TestTree
testLoadedStaysLoaded = testCase "a note read stays read through the reads after it" $ do
  (vault, config) <- caseVault "loaded"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  ctss <- vaultService config vault
  note <- mkDesCanonicalImportStr config (vault </> "Welcome.md")
  held <- notesHeld vault ctss
  assertEqual "a note was read before anything asked what was in it" [] held

  ctssLoad ctss [note]
  loaded <- notesHeld vault ctss
  assertEqual "the note asked for was not read" [vault </> "Welcome.md"] loaded

  -- Forgotten so the next read reads it again rather than giving it back untouched. A read which
  -- gives a page back never asks a record about it, so this is the only way round to the case: the
  -- note is read afresh while its record still describes it exactly, which is when a read deciding
  -- for itself would answer out of that record and drop the program the service holds.
  ctssStale ctss note
  ctssRead ctss
  reread <- notesHeld vault ctss
  assertEqual "a note read afresh was answered for out of a record, losing the program held for it"
    [vault </> "Welcome.md"] reread

-- | Saving a note takes effect, however recently the note was read.
--
-- The record is answered for by the same look at the same modification time that
-- 'CtService.ctssStale' exists to work around: a filesystem keeping times to the second has nothing
-- to show for a page written and read inside one. So a save reads the page in full rather than
-- leaving the read to decide, or a save would be answered with what the note said before it.
testSaveReadsInFull :: TestTree
testSaveReadsInFull = testCase "a note saved takes effect rather than reverting to what was recorded" $ do
  (vault, config) <- caseVault "saved"
  now <- getCurrentTime
  threeNotes (addUTCTime (-60) now) vault
  _ <- readVault config [] noPages vault

  ctss <- vaultService config vault
  note <- mkDesCanonicalImportStr config (vault </> "Welcome.md")
  saved <- runCResT $ setSource ctss note "Now only [[Tasks]].\n"
  case saved of
    CErr notes -> assertFailure $ "saving the note failed: " ++ show notes
    CRes _ ()  -> return ()

  src <- runCResT $ getSource ctss note
  case src of
    CErr notes -> assertFailure $ "reading the note back failed: " ++ show notes
    CRes _ back -> assertEqual "the note did not come back as what was saved" "Now only [[Tasks]].\n" back

obsidianCacheTests :: TestTree
obsidianCacheTests = testGroup "ObsidianCache" [
  testWarmGraphMatches,
  testReadsWhatIsAskedFor,
  testWrittenNoteIsReadAgain,
  testSameTimeDifferentLength,
  testLinkedNoteDeleted,
  testLinkedNoteMoved,
  testRemovedNoteIsForgotten,
  testNoCacheReadsEverything,
  testDeferredHasNoSource,
  testVaultNamedByPath,
  testLoadedStaysLoaded,
  testSaveReadsInFull
                                               ]
