--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Rss.Cache
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module records what a feed has said, so that a later run is answered out of it rather than
-- by asking the feed again, and so that what the feed has stopped saying is still there.
--
-- It keeps a record for two reasons where 'Syntax.Obsidian.Cache' keeps one for one. Asking a feed
-- means asking the network, which is slow, may fail, and is a request made of somebody else's
-- machine — so it is made at most once every 'rssCacheBuffer' and the record answers in between.
-- And a feed is a window rather than a document: xkcd carries its last four comics and nothing
-- before them, so a page built out of what the feed says now would forget an entry the moment a
-- fifth arrived. The record is therefore what the page is built from, and a fetch adds to it.
--
-- The two records share a root, a file naming and a way of being written, and share no question.
-- Obsidian's asks whether a file is still the file that was read, out of that file's own time and
-- length, and forgets what the vault no longer holds. This one asks whether six hours have passed,
-- out of nothing but a clock, and forgets nothing until there is too much to keep. Neither answer
-- is the other's, so they are two records rather than one mechanism with two settings — see the
-- head of 'Syntax.Obsidian.Cache' for what a shared one would have had to be.
--------------------------------------------------------------------

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Rss.Cache where

import           Control.Exception    (IOException, try)
import           Control.Monad        (unless, void, when)
import           Data.Aeson           (FromJSON, ToJSON, eitherDecodeFileStrict,
                                       encodeFile)
import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict  as H
import           Data.IORef
import           Data.List            (foldl')
import           Data.Time.Clock      (NominalDiffTime, UTCTime, diffUTCTime)
import qualified Data.UUID            as UUID
import qualified Data.UUID.V5         as UUID5
import           GHC.Generics         (Generic)
import           System.Directory     (createDirectoryIfMissing, doesPathExist,
                                       removeFile, renameFile)
import           System.FilePath      (joinPath, (<.>), (</>))

-- | How long a feed is answered out of the record before it is asked again.
--
-- A feed is somebody else's machine, and a doc server reads on every request it answers, so what
-- this bounds is how often that machine is asked rather than how fresh a page is. Six hours is
-- longer than a reader is likely to wait for an entry and far shorter than a feed's own patience.
rssCacheBuffer :: NominalDiffTime
rssCacheBuffer = 6 * 60 * 60

-- | How many entries of one feed are kept.
--
-- The record accumulates, so without a bound a feed followed for years is an ever longer file and
-- an ever longer page. Two hundred is several years of a feed which writes three times a week, and
-- the entries dropped are the ones nothing has seen for longest.
rssCacheKeep :: Int
rssCacheKeep = 200

-- | One entry of a feed, as some read of it found the entry.
data ItemEntry = ItemEntry {
  -- | What the feed calls the entry: its id, or its address, or its title. The name the record
  -- merges on, so an entry seen by two reads is one entry rather than two.
  ieGuid        :: String,
  ieTitle       :: String,
  ieLink        :: String,
  -- | The date as the feed wrote it, or empty where the feed dated nothing.
  iePublished   :: String,
  -- | The entry's writing as the feed wrote it, which is markup.
  ieDescription :: String,
  -- | When a read of this feed first saw the entry. Not shown anywhere; kept because it is the one
  -- thing that can be said of an entry the feed has stopped carrying, and so is what a record too
  -- long to keep whole is trimmed by.
  ieFirstSeen   :: UTCTime
                           }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Everything recorded of one feed.
data FeedCache = FeedCache {
  -- | The feed this is of. Written down so that a file found under the name a feed hashes to is
  -- checked rather than trusted: one which says it is of another feed is read past, not answered
  -- with.
  fcFeed    :: String,
  -- | What the feed calls itself, which the page is headed by.
  --
  -- Recorded rather than taken from the answer, because the page is built out of the record and
  -- must be the same page whether the record was just written or was written six hours ago. A
  -- heading which appeared only on the read that fetched would be a page that changed shape every
  -- time the feed was asked, and so a page every reader of it had to be told about again.
  fcTitle   :: String,
  -- | When the feed was last asked. Stamped only by a fetch which answered, so a feed which could
  -- not be reached is asked again by the next read rather than left for six hours.
  fcFetched :: UTCTime,
  -- | The entries, in the order the page writes them: what the feed says now, in the order it says
  -- it, and then what it used to say.
  fcItems   :: [ItemEntry]
                           }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The feeds one read has looked at, and whether it has anything to write back to them.
--
-- Held for a whole read because a read may reach one feed from several pages, and each of them
-- would otherwise read that feed's record from disk for itself.
data RssCache = RssCache {
  rcRoot  :: FilePath,
  rcFeeds :: IORef (H.HashMap String (Maybe FeedCache, Bool))
                         }

newRssCache :: FilePath -> IO RssCache
newRssCache root = RssCache root <$> newIORef H.empty

-- | The directory a cache root keeps RSS's records in. Named here rather than at the root so that a
-- root shared with whatever else comes to keep records is still one directory per syntax.
rssCacheDir :: FilePath -> FilePath
rssCacheDir root = joinPath [root, "rss"]

-- | The file a feed's record is kept in.
--
-- Named by a hash of the feed's address rather than by the address, since an address holds the
-- separators a name cannot. Which feed it is of is written inside it, so a name two feeds happen to
-- share is read past rather than answered with.
feedCacheFile :: FilePath -> String -> FilePath
feedCacheFile root feed = rssCacheDir root </> UUID.toString named <.> "json"
  where named = UUID5.generateNamed UUID5.namespaceURL $ B.unpack $ BSU.fromString feed

-- | The record of a feed this read has, reading it from disk if this read has not.
--
-- A record which cannot be read, or which is of another feed, is no record: a cache is a thing to be
-- answered faster by and never a thing to fail on, so anything the matter with it reads as nothing
-- having been recorded.
cachedFeed :: RssCache -> String -> IO (Maybe FeedCache)
cachedFeed RssCache{rcRoot, rcFeeds} feed = do
  held <- readIORef rcFeeds
  case H.lookup feed held of
    Just (record, _) -> return record
    Nothing -> do
      read' <- try (eitherDecodeFileStrict (feedCacheFile rcRoot feed)) :: IO (Either IOException (Either String FeedCache))
      let record = case read' of
            Right (Right cache) | fcFeed cache == feed -> Just cache
            _                                          -> Nothing
      modifyIORef' rcFeeds $ H.insert feed (record, False)
      return record

-- | Whether a record still answers for its feed, as of now.
--
-- A record stamped later than now is one a clock moved backwards has left behind, and answers for
-- nothing: taking it as fresh would leave a feed unasked until the clock caught up again.
feedIsFresh :: UTCTime -> FeedCache -> Bool
feedIsFresh now FeedCache{fcFetched} = elapsed >= 0 && elapsed < rssCacheBuffer
  where elapsed = diffUTCTime now fcFetched

-- | Record what asking a feed found, and answer with everything known of it.
--
-- The entries given are the ones the feed carries now, in the order it wrote them. What comes back
-- is those and the ones only the record holds, which is what the page is built from.
recordFeed :: RssCache -> String -> UTCTime -> String -> [ItemEntry] -> IO [ItemEntry]
recordFeed cache@RssCache{rcFeeds} feed fetched title found = do
  held <- cachedFeed cache feed
  let merged = take rssCacheKeep $ mergeItems (maybe [] fcItems held) found
  modifyIORef' rcFeeds $ H.insert feed (Just (FeedCache feed title fetched merged), True)
  return merged

-- | The entries of a feed and the entries recorded of it, as one run of entries.
--
-- An entry the fetch carries takes the fetch's words, since the feed has said them more recently
-- than the record did, and keeps the time it was first seen, since that is the one thing the fetch
-- cannot say. An entry the fetch carries and the record does not is new, and goes where the feed
-- put it. An entry only the record holds has fallen off the end of the feed, and is kept where it
-- was, below the ones which have not.
--
-- Order is decided by where an entry sits rather than by the times on it, so a feed which dates
-- nothing still reads top-down as what it says now and then what it used to say.
mergeItems :: [ItemEntry] -> [ItemEntry] -> [ItemEntry]
mergeItems recorded found = map refreshed found ++ filter fallenOff recorded
  where
    was = H.fromList [(ieGuid item, item) | item <- recorded]
    still = foldl' (\acc item -> H.insert (ieGuid item) () acc) H.empty found
    refreshed item = case H.lookup (ieGuid item) was of
      Just old -> item{ieFirstSeen = ieFirstSeen old}
      Nothing  -> item
    fallenOff item = not $ H.member (ieGuid item) still

-- | Write back whatever this read has to say, and nothing where it has nothing.
--
-- A read answered out of the records leaves them as they were, so a doc server reading on every
-- request rewrites nothing until a feed has been asked again.
--
-- Written to a file of its own and moved into place, so that a run stopping halfway leaves the
-- record it had rather than half of a new one. Two runs writing at once leaves whichever moved
-- last, which is one of the two reads' record rather than a mixture of both — and either is a
-- record, which is all this has to be.
flushRssCache :: RssCache -> IO ()
flushRssCache RssCache{rcRoot, rcFeeds} = do
  held <- readIORef rcFeeds
  mapM_ write [record | (Just record, True) <- H.elems held]
  where
    write record = do
      let path = feedCacheFile rcRoot (fcFeed record)
      let tmp = path <.> "writing"
      -- A record which cannot be written is a run which asks the feed again next time, which is what
      -- a run with no record at all does. Nothing to report, and nothing to fail.
      done <- passingOver $ do
        createDirectoryIfMissing True (rssCacheDir rcRoot)
        encodeFile tmp record
        renameFile tmp path
      unless done $ do
        left <- doesPathExist tmp
        when left $ void $ passingOver $ removeFile tmp

-- | Do it, answering whether it was done, and passing over its failing either way.
passingOver :: IO () -> IO Bool
passingOver act = either (const False) (const True) <$> (try act :: IO (Either IOException ()))
