--------------------------------------------------------------------
-- |
-- Module    :  FileWatch
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module watches the files a program was read from, so that reading it again costs what the
-- change costs rather than what the program costs.
--
-- What it answers is which paths have been written to. Deciding what that means for a page is
-- 'CtService.ctssRead's, which knows which paths each page was read from.
--------------------------------------------------------------------

module FileWatch where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar',
                                         newTVarIO, readTVar, writeTVar)
import           Control.Exception      (SomeException, onException, try)
import           Control.Monad          (forM, forM_, unless)
import qualified Data.HashMap.Strict    as H
import qualified Data.HashSet           as S
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           System.Directory       (doesDirectoryExist)
import           System.FilePath        (takeDirectory)
import           System.FSNotify        (Event, WatchManager, eventPath,
                                         startManager, watchDir, watchTree)

-- | Something asked to be watched: a path, and whether everything under it was asked for too.
type Watch = (FilePath, Bool)

-- | The directories being watched, and what they were watched for.
data Watching = Watching {
  -- | What was last asked for, which is what the watches are for.
  wAsked   :: S.HashSet Watch,
  -- | The directories being watched, and how to stop watching each, by whether the watch reaches
  -- under the directory. A directory watched both ways is watched once, reaching under, since that
  -- reports everything the other would.
  wDirs    :: H.HashMap FilePath (Bool, IO ()),
  -- | Everything asked for whose every watch is in place. Only these can the watcher answer for; one
  -- whose directory could not be watched — the watch limit reached, a filesystem which reports
  -- nothing — is left out, so that whoever asked looks at it instead of waiting for an event which
  -- will never come.
  wCovered :: S.HashSet Watch
                         }

data Watcher = Watcher {
  wManager  :: WatchManager,
  -- | The paths written to since they were last asked for.
  --
  -- Held in STM so that a caller wanting to act on a change as it happens, rather than the next
  -- time it asks, can wait for this to fill instead of asking over and over.
  wPending  :: TVar (S.HashSet FilePath),
  wWatching :: IORef Watching
                       }

startWatcher :: IO Watcher
startWatcher = do
  manager <- startManager
  Watcher manager <$> newTVarIO S.empty <*> newIORef (Watching S.empty H.empty S.empty)

-- | The directories it takes to notice a change to something asked for.
--
-- A path needs the directory holding it, so that it appearing, going or being written to is seen,
-- and itself where it is a directory, so that an entry appearing in it is seen — a listing is
-- changed by what appears in it, which the directory holding it says nothing about. Asked for with
-- everything under it, one watch reaching under it says all of that at once.
watchedFor :: Watch -> IO [(FilePath, Bool)]
watchedFor (path, True) = return [(path, True), (takeDirectory path, False)]
watchedFor (path, False) = do
  isDir <- doesDirectoryExist path
  return $ (takeDirectory path, False) : [(path, False) | isDir]

-- | Watch what it takes to notice a change to any of these, and nothing else.
--
-- Asking for the same things again does nothing at all, which is the usual case: what a program is
-- read from only changes when the program does.
watchReads :: Watcher -> [Watch] -> IO ()
watchReads w asked = do
  watching <- readIORef (wWatching w)
  let wanted = S.fromList asked
  unless (wanted == wAsked watching) $ do
    needs <- forM (S.toList wanted) $ \want -> (want,) <$> watchedFor want
    -- A directory wanted both ways is watched once, reaching under it.
    let dirs = H.fromListWith (||) $ concatMap snd needs
    forM_ (H.toList $ wDirs watching) $ \(dir, (under, stop)) ->
      unless (H.lookup dir dirs == Just under) stop
    let kept = H.filterWithKey (\dir (under, _) -> H.lookup dir dirs == Just under) (wDirs watching)
    started <- forM (H.toList $ H.difference dirs kept) $ \(dir, under) -> do
      -- A directory which cannot be watched is not an error to report: it leaves what was asked for
      -- out of 'wCovered', which has it looked at on every read instead. Slower, never stale.
      let watch = if under then watchTree else watchDir
      stop <- try (watch (wManager w) dir (const True) (report w)) :: IO (Either SomeException (IO ()))
      return $ either (const Nothing) (Just . (dir,) . (under,)) stop
    let dirs' = H.union kept (H.fromList [s | Just s <- started])
    writeIORef (wWatching w) Watching {
      wAsked = wanted,
      wDirs = dirs',
      wCovered = S.fromList [want | (want, needed) <- needs, all (watchedBy dirs') needed]
                                                    }
  where
    -- A watch reaching under a directory reports everything a watch on the directory alone reports,
    -- so it answers for both; a watch on the directory alone answers only for itself. The two are
    -- asked for together whenever a tree is read from and something in it is read from as well --
    -- an Obsidian vault and a note of it -- and a note left uncovered by the tree watch its own
    -- vault put on would be looked at on every read, for no reason and with nothing to say so.
    watchedBy dirs' (dir, under) = case H.lookup dir dirs' of
      Just (under', _) -> under' || not under
      Nothing          -> False
    report :: Watcher -> Event -> IO ()
    report w' event = atomically $ modifyTVar' (wPending w') $ S.insert (eventPath event)

-- | What the watcher can answer for.
covered :: Watcher -> IO (S.HashSet Watch)
covered w = wCovered <$> readIORef (wWatching w)

-- | Whether everything asked about is something the watcher can answer for, which is the usual case:
-- a watch is refused only where the filesystem cannot report or the watch limit has been reached.
--
-- Worth asking on its own because it answers for everything at once, where asking whether some
-- particular paths are covered is work for each of them. A caller which groups its paths — many
-- groups, each holding many paths, the same path in many groups — would otherwise pay for every
-- path of every group to learn what this says in one.
allCovered :: Watcher -> IO Bool
allCovered w = (\watching -> S.size (wCovered watching) == S.size (wAsked watching)) <$> readIORef (wWatching w)

-- | Act on the paths written to since this was last asked, taking them out of the watcher.
--
-- Taken rather than read, so a change is answered once. A change which arrives while the acting is
-- going on is therefore left for the next asker, which reads a page one more time than it had to
-- and never one time less.
--
-- Put back where the acting does not finish. A read is what finds out that a file has been saved
-- with a syntax error, and it finds out by failing; the changes it was told about are still changes
-- when it does, including the ones to files it never got as far as reading.
withPending :: Watcher -> (S.HashSet FilePath -> IO a) -> IO a
withPending w act = do
  pending <- atomically $ do
    pending <- readTVar (wPending w)
    writeTVar (wPending w) S.empty
    return pending
  act pending `onException` atomically (modifyTVar' (wPending w) (S.union pending))
