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
import           System.FilePath        (takeDirectory)
import           System.FSNotify        (Event, WatchManager, eventPath,
                                         startManager, watchDir, watchTree)

-- | A directory to watch, and whether the watch reaches under it.
--
-- Directories only, because a directory is the smallest thing a watch can be put on that reports
-- anything: a change to a file is reported by a watch on the directory holding it, and a change to
-- a directory — an entry appearing in it, going from it, being renamed within it — by a watch on
-- the directory itself. Whoever asks says which directories it needs
-- ('CtService.readPathWatches'), because only the reading knows whether what it read was a file, a
-- listing or a tree, and asking the disk instead would be a question per path on every read.
type Watch = (FilePath, Bool)

-- | The watches asked for, and the ones it took to answer them.
data Watching = Watching {
  -- | What was last asked for.
  wAsked   :: S.HashSet Watch,
  -- | The watches in place, and how to stop each. Fewer than were asked for, wherever one answers
  -- for another ('answeredBy').
  wDirs    :: H.HashMap FilePath (Bool, IO ()),
  -- | Everything asked for that the watches in place answer for. Only these can the watcher answer
  -- for; one whose watch could not be put on — the watch limit reached, a filesystem which reports
  -- nothing — is left out, so that whoever asked looks at it instead of waiting for an event which
  -- will never come.
  wCovered :: S.HashSet Watch
                         }

data Watcher = Watcher {
  wManager  :: WatchManager,
  -- | Whether a watch reaching under a directory reports a path under it: the directory, then the
  -- path. Given at the start rather than fixed here, because a watch stands for a reading and only
  -- the reading knows what it walked into. A path it would not have read is a path that must not
  -- wake anything: an Obsidian vault rewrites its own configuration constantly while it is open,
  -- and every one of those writes would otherwise be a change to answer for.
  wReports  :: FilePath -> FilePath -> Bool,
  -- | The paths written to since they were last asked for.
  --
  -- Held in STM so that a caller wanting to act on a change as it happens, rather than the next
  -- time it asks, can wait for this to fill instead of asking over and over.
  wPending  :: TVar (S.HashSet FilePath),
  wWatching :: IORef Watching
                       }

startWatcher :: (FilePath -> FilePath -> Bool) -> IO Watcher
startWatcher reports = do
  manager <- startManager
  Watcher manager reports <$> newTVarIO S.empty <*> newIORef (Watching S.empty H.empty S.empty)

-- | Whether some directory above this one is watched reaching under it.
--
-- Such a watch reports everything that happens under it at any depth, so it already reports
-- everything a watch here would, and a watch here would be a second report of the same writes.
-- Strictly above: a directory does not answer for itself here, as whether it does depends on what
-- is being asked.
recursiveAbove :: (FilePath -> Maybe Bool) -> FilePath -> Bool
recursiveAbove look = go . takeDirectory
  where
    go dir | look dir == Just True    = True
           | takeDirectory dir /= dir = go (takeDirectory dir)
           | otherwise                = False

-- | Whether the watches in place answer for what was asked.
--
-- A watch reaching under a directory answers for that directory and for everything under it; a
-- watch on the directory alone answers only for that directory. So a directory left unwatched
-- because a tree above it is watched is still answered for, which it must be — 'wCovered' is what
-- decides whether a caller trusts the watcher or looks at the files itself, so a directory dropped
-- from the watches but not answered for here would quietly put every page under that tree back to
-- being looked at, with nothing to say so.
answeredBy :: H.HashMap FilePath (Bool, IO ()) -> Watch -> Bool
answeredBy dirs (dir, under) = here || recursiveAbove (fmap fst . (`H.lookup` dirs)) dir
  where
    here = case H.lookup dir dirs of
      Just (under', _) -> under' || not under
      Nothing          -> False

-- | Watch what it takes to notice a change to any of these, and nothing else.
--
-- Asking for the same things again does nothing at all, which is the usual case: what a program is
-- read from only changes when the program does.
watchReads :: Watcher -> [Watch] -> IO ()
watchReads w asked = do
  before <- readIORef (wWatching w)
  let wanted = S.fromList asked
  unless (wanted == wAsked before) $ do
    -- A directory asked for both ways is watched once, reaching under it, and one asked for under a
    -- tree that is also asked for is not watched at all.
    let needed = H.fromListWith (||) (S.toList wanted)
    let dirs = H.filterWithKey (\dir _ -> not (recursiveAbove (`H.lookup` needed) dir)) needed
    forM_ (H.toList $ wDirs before) $ \(dir, (under, stop)) ->
      unless (H.lookup dir dirs == Just under) stop
    let kept = H.filterWithKey (\dir (under, _) -> H.lookup dir dirs == Just under) (wDirs before)
    started <- forM (H.toList $ H.difference dirs kept) $ \(dir, under) -> do
      -- A directory which cannot be watched is not an error to report: it leaves what was asked for
      -- out of 'wCovered', which has it looked at on every read instead. Slower, never stale.
      stop <- try (start dir under) :: IO (Either SomeException (IO ()))
      return $ either (const Nothing) (Just . (dir,) . (under,)) stop
    let dirs' = H.union kept (H.fromList [s | Just s <- started])
    writeIORef (wWatching w) Watching {
      wAsked = wanted,
      wDirs = dirs',
      wCovered = S.filter (answeredBy dirs') wanted
                                                    }
  where
    -- Only a watch reaching under a directory is asked what it reports, because that is the only
    -- watch that reports paths the asker did not name. A watch on a directory alone reports that
    -- directory, which is exactly what was asked for.
    start dir True  = watchTree (wManager w) dir (wReports w dir . eventPath) (report w)
    start dir False = watchDir (wManager w) dir (const True) (report w)
    report :: Watcher -> Event -> IO ()
    report w' event = atomically $ modifyTVar' (wPending w') $ S.insert (eventPath event)

-- | What the watcher can answer for.
covered :: Watcher -> IO (S.HashSet Watch)
covered w = wCovered <$> readIORef (wWatching w)

-- | The watches in place: what it took to answer for everything 'covered' holds.
--
-- Nothing acts on this — it is what 'watchReads' did, not something to decide by. It is here to be
-- asserted against, because a watch that need not have been put on costs nothing that anything else
-- can see, and one wrongly left off is answered for by 'answeredBy' being wrong in the same way.
watching :: Watcher -> IO (S.HashSet Watch)
watching w = do
  Watching{wDirs} <- readIORef (wWatching w)
  return $ S.fromList [(dir, under) | (dir, (under, _)) <- H.toList wDirs]

-- | Whether everything asked about is something the watcher can answer for, which is the usual case:
-- a watch is refused only where the filesystem cannot report or the watch limit has been reached.
--
-- Worth asking on its own because it answers for everything at once, where asking whether some
-- particular paths are covered is work for each of them. A caller which groups its paths — many
-- groups, each holding many paths, the same path in many groups — would otherwise pay for every
-- path of every group to learn what this says in one.
allCovered :: Watcher -> IO Bool
allCovered w = (\seen -> S.size (wCovered seen) == S.size (wAsked seen)) <$> readIORef (wWatching w)

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
