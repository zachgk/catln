-- | Tests for which directories get watched, and what the watches report.
--
-- Which watches are in place is worth asserting on its own because nothing else can see it. A watch
-- that need not have been put on costs only descriptors, which no other test measures, and one
-- wrongly left off is hidden by 'FileWatch.answeredBy' being wrong in the same way — the pages under
-- it quietly go back to being looked at on every read, which is slower and still correct. So the
-- cases here ask 'FileWatch.watching' and 'FileWatch.covered' directly rather than through a read.
--
-- The two cases that write files are the ones nothing else could stand in for: they are what says a
-- watch reaching under a directory really does report a write deeper in it, which is the whole
-- reason the watches under it are left off.
module Syntax.FileWatchTests where

import           Control.Concurrent (threadDelay)
import qualified Data.HashSet       as S
import           FileWatch          (Watch, Watcher, covered, startWatcher,
                                     watchReads, watching, withPending)
import           Syntax.Ct.Prgm     (readsUnder)
import           System.Directory   (createDirectoryIfMissing,
                                     getTemporaryDirectory, removePathForcibly)
import           System.FilePath    ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit

-- | A directory to hold one case's directories, emptied first, with the given ones made under it.
caseDirs :: String -> [FilePath] -> IO (FilePath, Watcher)
caseDirs name dirs = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-filewatch" </> name
  removePathForcibly dir
  createDirectoryIfMissing True dir
  mapM_ (createDirectoryIfMissing True . (dir </>)) dirs
  w <- startWatcher readsUnder
  return (dir, w)

-- | Ask for these watches, then say which are in place and which are answered for.
asked :: Watcher -> [Watch] -> IO (S.HashSet Watch, S.HashSet Watch)
asked w want = do
  watchReads w want
  (,) <$> watching w <*> covered w

-- | A watch reaching under a directory answers for every directory under it, so no watch goes on
-- any of them. They are still answered for: a directory left unwatched but reported as unwatchable
-- would have every page under it looked at on every read, which is the cost the watcher exists to
-- avoid and which nothing would say had come back.
testSubsumed :: TestTree
testSubsumed = testCase "a watch reaching under a directory is the only watch under it" $ do
  (dir, w) <- caseDirs "subsumed" ["sub" </> "deep"]
  let want = [(dir, True), (dir </> "sub", False), (dir </> "sub" </> "deep", False)]
  (dirs, seen) <- asked w want
  assertEqual "a directory under the tree was watched as well" (S.fromList [(dir, True)]) dirs
  assertEqual "a directory under the tree was left unanswered for" (S.fromList want) seen

-- | A directory asked for both ways is watched once, reaching under it, since that watch reports
-- everything the other would. Asked for whenever a tree is read from and something directly in it is
-- read from as well — an Obsidian vault and a note written at its top level.
testBothWays :: TestTree
testBothWays = testCase "a directory asked for both ways is watched once, reaching under it" $ do
  (dir, w) <- caseDirs "bothways" []
  (dirs, seen) <- asked w [(dir, True), (dir, False)]
  assertEqual "the directory was not watched once reaching under it" (S.fromList [(dir, True)]) dirs
  assertEqual "the directory was not answered for both ways" (S.fromList [(dir, True), (dir, False)]) seen

-- | A watch on a directory alone says nothing about what is under it, so a directory under one is
-- watched itself. The mirror of 'testSubsumed', and what says subsumption follows from the watch
-- reaching under rather than from one directory holding another.
testPlainDoesNotSubsume :: TestTree
testPlainDoesNotSubsume = testCase "a watch on a directory alone does not answer for what is under it" $ do
  (dir, w) <- caseDirs "plain" ["sub"]
  let want = [(dir, False), (dir </> "sub", False)]
  (dirs, seen) <- asked w want
  assertEqual "a directory under a plainly watched one was left unwatched" (S.fromList want) dirs
  assertEqual "a watched directory was left unanswered for" (S.fromList want) seen

-- | A directory beside a tree is watched itself, and a tree under a tree is not.
testBesideAndNested :: TestTree
testBesideAndNested = testCase "a tree beside a directory keeps it, and a tree under a tree does not" $ do
  (dir, w) <- caseDirs "nested" ["vault" </> "sub", "beside"]
  let vault = dir </> "vault"
  let want = [(vault, True), (vault </> "sub", True), (dir </> "beside", False)]
  (dirs, seen) <- asked w want
  assertEqual "the nested tree was watched, or the directory beside it was not"
    (S.fromList [(vault, True), (dir </> "beside", False)]) dirs
  assertEqual "something asked for was left unanswered for" (S.fromList want) seen

-- | A watch no longer asked for is stopped, and one asked for in its place is put on. What the
-- watches are for is what the reads read, so they change whenever a read finds a directory it had
-- not read before or stops reading one it had.
testReplaced :: TestTree
testReplaced = testCase "a watch no longer asked for is stopped" $ do
  (dir, w) <- caseDirs "replaced" ["one", "two"]
  (dirs, _) <- asked w [(dir </> "one", False)]
  assertEqual "the watch asked for was not put on" (S.fromList [(dir </> "one", False)]) dirs
  (dirs', seen') <- asked w [(dir </> "two", False)]
  assertEqual "the watch no longer asked for was not stopped" (S.fromList [(dir </> "two", False)]) dirs'
  assertEqual "the watch no longer asked for is still answered for" (S.fromList [(dir </> "two", False)]) seen'

-- | What a reading of a tree reads, which is the rule the watcher is handed and the same rule a
-- read asks of the paths it is told were written ('Syntax.Parsers.readPathWrittenIn'). Asserted
-- directly because the watcher only ever hands it paths genuinely under the tree, so the case that
-- matters — a directory whose name merely begins with the tree's — reaches it only from a read.
testReadsUnder :: TestTree
testReadsUnder = testCase "a tree is read from what is under it, and not from what merely begins with it" $ do
  assertBool "a note under the tree is not read" $ readsUnder "/a/notes" "/a/notes/sub/One.md"
  assertBool "the tree itself is not read" $ readsUnder "/a/notes" "/a/notes"
  assertBool "a directory beside the tree is read" $ not $ readsUnder "/a/notes" "/a/notes-archive/One.md"
  assertBool "a hidden entry of the tree is read" $ not $ readsUnder "/a/notes" "/a/notes/.obsidian/workspace.json"
  assertBool "something outside the tree is read" $ not $ readsUnder "/a/notes" "/b/One.md"

-- | The paths reported within a second, or whatever was reported by then.
--
-- Waited for rather than slept on, so that a machine which reports quickly is not waited on and one
-- which reports slowly is not failed. The cases which use it wait for something they expect, so the
-- wait ends when the report arrives.
reported :: Watcher -> (S.HashSet FilePath -> Bool) -> IO (S.HashSet FilePath)
reported w done = aux (100 :: Int) S.empty
  where
    aux 0 acc = return acc
    aux n acc = do
      acc' <- S.union acc <$> withPending w return
      if done acc' then return acc' else threadDelay 10000 >> aux (n - 1) acc'

-- | A write under a directory watched reaching under it is reported, at whatever depth. This is what
-- lets 'testSubsumed' leave the directories under a tree unwatched: if a tree watch did not report
-- this, every page under it would go stale rather than merely be looked at.
testReportsUnder :: TestTree
testReportsUnder = testCase "a write deep under a watched tree is reported" $ do
  (dir, w) <- caseDirs "under" ["sub" </> "deep"]
  let note = dir </> "sub" </> "deep" </> "One.md"
  _ <- asked w [(dir, True)]
  writeFile note "A note.\n"
  pending <- reported w (S.member note)
  assertBool ("the write was not reported: " ++ show (S.toList pending)) (S.member note pending)

-- | A write to what a reading of the tree would not have read is not reported. Obsidian rewrites a
-- vault's own configuration as the vault is used, and a watcher which reported that would have every
-- read of an open vault go looking through its pages to find that none of them had changed.
--
-- The write that is expected is made as well, so the wait ends on it rather than on a fixed delay: a
-- test which slept could pass by not having waited long enough.
testReportsWhatIsRead :: TestTree
testReportsWhatIsRead = testCase "a write a reading of the tree would pass over is not reported" $ do
  (dir, w) <- caseDirs "config" [".obsidian"]
  let config = dir </> ".obsidian" </> "workspace.json"
  let note = dir </> "One.md"
  _ <- asked w [(dir, True)]
  writeFile config "{}\n"
  writeFile note "A note.\n"
  pending <- reported w (S.member note)
  assertBool ("the note's own write was not reported: " ++ show (S.toList pending)) (S.member note pending)
  assertBool ("the configuration write was reported: " ++ show (S.toList pending)) (not $ S.member config pending)

fileWatchTests :: TestTree
fileWatchTests = testGroup "FileWatch" [
  testSubsumed,
  testBothWays,
  testPlainDoesNotSubsume,
  testBesideAndNested,
  testReplaced,
  testReadsUnder,
  testReportsUnder,
  testReportsWhatIsRead
                                       ]
