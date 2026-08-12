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
                                         startManager, watchDir)

-- | The directories being watched, and what they were watched for.
data Watching = Watching {
  -- | The paths last asked about, which is what the watches are for.
  wPaths   :: S.HashSet FilePath,
  -- | The directories being watched, and how to stop watching each.
  wDirs    :: H.HashMap FilePath (IO ()),
  -- | The paths every watch of which is in place. Only these can the watcher answer for; a path
  -- whose directory could not be watched — the watch limit reached, a filesystem which reports
  -- nothing — is left out, so that whoever asked looks at it instead of waiting for an event which
  -- will never come.
  wCovered :: S.HashSet FilePath
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

-- | The directories it takes to notice a change to a path: the one holding it, so that the path
-- appearing, going or being written to is seen, and the path itself where it is a directory, so
-- that an entry appearing in it is seen.
--
-- A directory needs both. A page read from a directory listing is a page an entry added to it
-- changes, which the directory holding it says nothing about.
watchedFor :: FilePath -> IO [FilePath]
watchedFor path = do
  isDir <- doesDirectoryExist path
  return $ takeDirectory path : [path | isDir]

-- | Watch what it takes to notice a change to any of these paths, and nothing else.
--
-- Asking for the same paths again does nothing at all, which is the usual case: the paths a program
-- is read from only change when the program does.
watchReads :: Watcher -> [FilePath] -> IO ()
watchReads w paths = do
  watching <- readIORef (wWatching w)
  let wanted = S.fromList paths
  unless (wanted == wPaths watching) $ do
    needs <- forM (S.toList wanted) $ \path -> (path,) <$> watchedFor path
    let dirs = S.fromList $ concatMap snd needs
    forM_ (H.toList $ wDirs watching) $ \(dir, stop) -> unless (S.member dir dirs) stop
    let kept = H.filterWithKey (\dir _ -> S.member dir dirs) (wDirs watching)
    started <- forM (S.toList $ S.difference dirs (S.fromList $ H.keys kept)) $ \dir -> do
      -- A directory which cannot be watched is not an error to report: it leaves its paths out of
      -- 'wCovered', which has them looked at on every read instead. Slower, never stale.
      stop <- try (watchDir (wManager w) dir (const True) (report w)) :: IO (Either SomeException (IO ()))
      return $ either (const Nothing) (Just . (dir,)) stop
    let dirs' = H.union kept (H.fromList [s | Just s <- started])
    writeIORef (wWatching w) Watching {
      wPaths = wanted,
      wDirs = dirs',
      wCovered = S.fromList [path | (path, needed) <- needs, all (`H.member` dirs') needed]
                                                    }
  where
    report :: Watcher -> Event -> IO ()
    report w' event = atomically $ modifyTVar' (wPending w') $ S.insert (eventPath event)

-- | The paths the watcher can answer for.
covered :: Watcher -> IO (S.HashSet FilePath)
covered w = wCovered <$> readIORef (wWatching w)

-- | Whether every path asked about is one the watcher can answer for, which is the usual case: a
-- watch is refused only where the filesystem cannot report or the watch limit has been reached.
--
-- Worth asking on its own because it answers for everything at once, where asking whether some
-- particular paths are covered is work for each of them. A caller which groups its paths — many
-- groups, each holding many paths, the same path in many groups — would otherwise pay for every
-- path of every group to learn what this says in one.
allCovered :: Watcher -> IO Bool
allCovered w = (\watching -> S.size (wCovered watching) == S.size (wPaths watching)) <$> readIORef (wWatching w)

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
