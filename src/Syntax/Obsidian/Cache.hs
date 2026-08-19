--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Cache
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module records what reading the notes of an Obsidian vault found, so that a later run is
-- answered out of it rather than by reading them all again.
--
-- What a note costs to read is its text: it is read off disk and parsed as markdown, to find the
-- links written in its prose and not in its code. All that a reader of one note needs out of all
-- that is which notes it links to, which is what this keeps — so a vault of a thousand notes opened
-- again is a thousand files looked at rather than a thousand files read and parsed.
--
-- Resolving those links against the vault is not what this saves any more. That once scanned every
-- note of the vault for every link, so a vault of a thousand notes cost a million comparisons of a
-- path before any note had been parsed; it is now a lookup in an index the read builds once
-- ('Syntax.Obsidian.Parser.vaultIndex'). Which is worth saying because it was the reason this was
-- written, and a record kept for a cost that has gone would be a record worth dropping.
--
-- This is deliberately the Obsidian importer's own cache, and NOT a general mechanism for import
-- caching. A vault is a directory of peers whose members are found by walking it, and whether a
-- note is still the note that was read is answered by looking at that one file. Neither is true of
-- a Haskell or a Python import, which is resolved against a project rather than against a
-- directory.
--
-- There is now a second record, 'Syntax.Rss.Cache', and it is a second record rather than this one
-- generalised. What the two have in common is where they are kept and how they are written: one
-- directory per syntax under the cache root, one JSON file per source named by a hash of it with
-- the source written inside, and a flush which moves a finished file into place. What they do not
-- have in common is the question, which is the whole of what a cache is. This one asks whether a
-- file is still the file that was read, out of that file's own time and length, and forgets what
-- the vault no longer holds. That one asks whether six hours have passed, out of nothing but a
-- clock, and forgets nothing until there is too much to keep. A mechanism holding both would be a
-- mechanism holding neither question, which is a place to put a file and not a cache.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Obsidian.Cache where

import           Control.Exception    (IOException, try)
import           Control.Monad        (unless, void, when)
import           Data.Aeson           (FromJSON, ToJSON, eitherDecodeFileStrict,
                                       encodeFile)
import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.IORef
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import qualified Data.UUID            as UUID
import qualified Data.UUID.V5         as UUID5
import           GHC.Generics         (Generic)
import           System.Directory     (createDirectoryIfMissing, doesPathExist,
                                       getFileSize, getModificationTime,
                                       removeFile, renameFile)
import           System.FilePath      (joinPath, (<.>), (</>))

-- | What one reading of one note found: which notes it linked to, and what the note was when it
-- said so.
data NoteEntry = NoteEntry {
  neNote     :: FilePath,
  -- | When the note was last written, as the reading found it.
  neWritten  :: UTCTime,
  -- | How long the note was. Kept beside the time because a time on its own cannot say a file is
  -- the file that was read: a filesystem which keeps times to the second has nothing to show for
  -- two writes inside one, and @cp -p@, @rsync -a@ and @git stash@ all put an old time back over
  -- new contents. Two writes agreeing on both are rare enough to be worth the one extra look.
  neSize     :: Integer,
  -- | When the reading recorded this. A note whose time is not strictly before this is one the
  -- record cannot vouch for, in the same way and for the same reason as
  -- 'Syntax.Parsers.readPathCurrent' — the reading may have read it before the write which gave it
  -- that time. Within one run the times a read records decide that; across runs there is nothing
  -- else to decide it with, so it is written down.
  neRecorded :: UTCTime,
  -- | The notes the note links to, as the paths those links were resolved to.
  neLinks    :: [FilePath]
                           }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Everything recorded of one vault.
data VaultCache = VaultCache {
  -- | The vault this is of. Written down so that a file found under the name a vault hashes to is
  -- checked rather than trusted: one which says it is of another vault is read past, not answered
  -- with.
  vcVault :: FilePath,
  vcNotes :: [NoteEntry]
                             }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The vaults one read has looked at, and whether it has anything to write back to them.
--
-- Held for a whole read because a read reads many notes out of one vault, and each of them would
-- otherwise read that vault's record from disk for itself.
data ObsidianCache = ObsidianCache {
  ocRoot   :: FilePath,
  ocVaults :: IORef (H.HashMap FilePath (H.HashMap FilePath NoteEntry, Bool))
                                   }

newObsidianCache :: FilePath -> IO ObsidianCache
newObsidianCache root = ObsidianCache root <$> newIORef H.empty

-- | The directory a cache root keeps Obsidian's records in. Named here rather than at the root so
-- that a root shared with whatever else comes to keep records is still one directory per syntax.
obsidianCacheDir :: FilePath -> FilePath
obsidianCacheDir root = joinPath [root, "obsidian"]

-- | The file a vault's record is kept in.
--
-- Named by a hash of the vault's path rather than by the path, since a path holds the separators a
-- name cannot. Which vault it is of is written inside it, so a name two vaults happen to share is
-- read past rather than answered with.
vaultCacheFile :: FilePath -> FilePath -> FilePath
vaultCacheFile root vault = obsidianCacheDir root </> UUID.toString named <.> "json"
  where named = UUID5.generateNamed UUID5.namespaceURL $ B.unpack $ BSU.fromString vault

-- | The record of a vault this read has, reading it from disk if this read has not.
--
-- A record which cannot be read, or which is of another vault, is no record: a cache is a thing to
-- be answered faster by and never a thing to fail on, so anything the matter with it reads as
-- nothing having been recorded.
vaultNotesCache :: ObsidianCache -> FilePath -> IO (H.HashMap FilePath NoteEntry)
vaultNotesCache ObsidianCache{ocRoot, ocVaults} vault = do
  held <- readIORef ocVaults
  case H.lookup vault held of
    Just (notes, _) -> return notes
    Nothing -> do
      read' <- try (eitherDecodeFileStrict (vaultCacheFile ocRoot vault)) :: IO (Either IOException (Either String VaultCache))
      let notes = case read' of
            Right (Right VaultCache{vcVault, vcNotes}) | vcVault == vault -> H.fromList [(neNote n, n) | n <- vcNotes]
            _ -> H.empty
      modifyIORef' ocVaults $ H.insert vault (notes, False)
      return notes

-- | What a note is now: when it was last written and how long it is, or nothing where there is no
-- note there.
noteStamp :: FilePath -> IO (Maybe (UTCTime, Integer))
noteStamp note = either (const Nothing) Just <$> (try stamp :: IO (Either IOException (UTCTime, Integer)))
  where stamp = (,) <$> getModificationTime note <*> getFileSize note

-- | The notes a note links to, as an earlier read found them, where that reading still holds.
--
-- It holds while the note is the note that was read — same time, same length, and that time older
-- than the reading which recorded it — AND while every note it was found to link to is still
-- there. The second does not follow from the first: what a note's links resolve to is decided by
-- what the vault holds, which the note's own file says nothing about. Without it a note linking to
-- a note deleted between two runs would be answered with an import of a file which is not there,
-- and 'Syntax.Parsers.readFiles' walks on to every import — so one deleted note would fail the
-- whole read rather than dangle.
cachedNoteLinks :: ObsidianCache -> FilePath -> FilePath -> IO (Maybe [FilePath])
cachedNoteLinks cache vault note = do
  notes <- vaultNotesCache cache vault
  case H.lookup note notes of
    Nothing -> return Nothing
    Just NoteEntry{neWritten, neSize, neRecorded, neLinks} -> do
      now <- noteStamp note
      if now /= Just (neWritten, neSize) || neWritten >= neRecorded
        then return Nothing
        else do
          there <- mapM doesPathExist neLinks
          return $ if and there then Just neLinks else Nothing

-- | Record what reading a note found, for a later run to be answered out of.
--
-- The note is stamped after it was read rather than before, so that a note written while it was
-- being read is recorded as what it is now rather than as what the read found. Read again next run
-- is the cost of that; recorded as something it never was would be the cost of the other way
-- round.
recordNoteLinks :: ObsidianCache -> FilePath -> FilePath -> [FilePath] -> IO ()
recordNoteLinks cache@ObsidianCache{ocVaults} vault note links = do
  notes <- vaultNotesCache cache vault
  stamp <- noteStamp note
  recorded <- getCurrentTime
  case stamp of
    -- A note which cannot be looked at is one nothing can be said of later, so nothing is said of
    -- it now.
    Nothing -> return ()
    Just (written, size) ->
      modifyIORef' ocVaults $ H.insert vault (H.insert note (NoteEntry note written size recorded links) notes, True)

-- | Forget whatever was recorded of notes a vault no longer holds.
--
-- Given the notes a reading of the vault found, which is the one place the whole set of them is
-- known. Without it a record grows by every note ever renamed and never shrinks.
keepOnlyNotes :: ObsidianCache -> FilePath -> [FilePath] -> IO ()
keepOnlyNotes cache@ObsidianCache{ocVaults} vault notes = do
  held <- vaultNotesCache cache vault
  let kept = H.filterWithKey (\note _ -> S.member note there) held
  unless (H.size kept == H.size held) $
    modifyIORef' ocVaults $ H.insert vault (kept, True)
  where there = S.fromList notes

-- | Write back whatever this read has to say, and nothing where it has nothing.
--
-- A read answered out of the records leaves them as they were, so a doc server reading on every
-- request rewrites nothing until something it read has changed.
--
-- Written to a file of its own and moved into place, so that a run stopping halfway leaves the
-- record it had rather than half of a new one. Two runs writing at once leaves whichever moved
-- last, which is one of the two reads' record rather than a mixture of both — and either is a
-- record, which is all this has to be.
flushObsidianCache :: ObsidianCache -> IO ()
flushObsidianCache ObsidianCache{ocRoot, ocVaults} = do
  held <- readIORef ocVaults
  mapM_ write [(vault, notes) | (vault, (notes, True)) <- H.toList held]
  where
    write (vault, notes) = do
      let path = vaultCacheFile ocRoot vault
      let tmp = path <.> "writing"
      -- A record which cannot be written is a run which reads its notes again next time, which is
      -- what a run with no record at all does. Nothing to report, and nothing to fail.
      done <- passingOver $ do
        createDirectoryIfMissing True (obsidianCacheDir ocRoot)
        encodeFile tmp $ VaultCache vault (H.elems notes)
        renameFile tmp path
      unless done $ do
        left <- doesPathExist tmp
        when left $ void $ passingOver $ removeFile tmp

-- | Do it, answering whether it was done, and passing over its failing either way.
passingOver :: IO () -> IO Bool
passingOver act = either (const False) (const True) <$> (try act :: IO (Either IOException ()))
