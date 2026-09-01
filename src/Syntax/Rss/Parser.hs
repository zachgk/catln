--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Rss.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads an RSS feed as a page: @import rss("https:\/\/xkcd.com\/rss.xml")@, whose page
-- holds one @\/Data\/RSS\/Item@ declaration per entry of the feed.
--
-- It is the first syntax which reads something that is not on disk. Two things follow from that.
-- What it reads it reads at most once every 'Syntax.Rss.Cache.rssCacheBuffer' and answers out of a
-- record in between, and what it reads is a window rather than a document — so the page is built
-- from the record, which is every entry any read has seen, and a fetch adds to it. The record is
-- 'Syntax.Rss.Cache'.
--
-- An address is not the only thing a feed can be named by. A path to a file holding one is read as
-- that file, with no record kept and nothing asked of the network, which is what lets a feed be
-- tested against a fixture rather than against somebody's server.
--------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Rss.Parser where

import           Control.Exception    (SomeException, try)
import           CtConstants
import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as BSU
import           Data.List            (isPrefixOf)
import           Data.Maybe           (listToMaybe, mapMaybe)
import qualified Data.Text            as T
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import           Network.HTTP.Simple  (Response, getResponseBody, httpBS,
                                       parseRequest)
import           Semantics.Prgm
import           Semantics.Types      (Constant (CStr), mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Rss.Cache
import           System.Directory     (doesFileExist)
import           Text.Feed.Import     (parseFeedSource)
import           Text.Feed.Query      (getFeedItems, getFeedTitle, getItemDate,
                                       getItemDescription, getItemId,
                                       getItemLink, getItemTitle)
import qualified Text.Feed.Types      as Feed
import           Text.Printf

-- | The name an import reads a feed with.
rss :: String
rss = "rss"

-- | What one read reads feeds with: where a record of them is kept, and how a feed is asked.
--
-- How a feed is asked is a field rather than a call, so that a test can hold the reader to what it
-- does with an answer without opening a socket to get one.
data RssReader = RssReader {
  rrCache :: Maybe RssCache,
  rrFetch :: String -> IO (Maybe String)
                           }

-- | Reading a feed as a page, and writing back what the read found of it.
--
-- No reversion: a feed is somebody else's writing and there is nothing to write a page of it back
-- to, so 'Syntax.Parsers.unparseImport' reports that rather than saving it as something else.
rssSyntax :: ImportSyntax
rssSyntax = ImportSyntax (newRssReaderWith fetchFeed) (const Nothing) rssLinkDir Nothing

-- | The reader one read reads feeds with, asking them however it is told to.
newRssReaderWith :: (String -> IO (Maybe String)) -> ReadEnv -> IO ImportReader
newRssReaderWith fetch env = do
  cache <- traverse newRssCache (reCacheRoot env)
  let reader = RssReader cache fetch
  return $ readyReader (rssPreload reader) (rssParse reader) (mapM_ flushRssCache cache)

-- | Where a relative link written on a feed's page resolves against.
--
-- Nothing, for a feed named by an address. 'Syntax.Parsers.canonicalImport' works out a directory
-- for every import by taking the directory of what it names, and the directory of an address is not
-- one — @https:\/\/xkcd.com\/rss.xml@ gives @https:\/xkcd.com@, which names nothing and would have
-- a page's links resolve against it. A feed named by a path has a directory like any other page.
rssLinkDir :: FileImport -> IO (Maybe FilePath)
rssLinkDir imp
  | maybe False snd (rssImport' (impAbs imp)) = return Nothing
  | otherwise = importedLinkDir imp

-- | Whether a feed is named by an address rather than by a path.
isWebAddress :: String -> Bool
isWebAddress name = "http://" `isPrefixOf` name || "https://" `isPrefixOf` name

-- | The feed an import names, and whether it is asked of the network.
rssImport :: MonadFail m => Expr () -> m (String, Bool)
rssImport imp = case rssImport' imp of
  Just found -> return found
  Nothing    -> fail $ printf "An rss import must name a feed, in %s" (show imp)

rssImport' :: Expr () -> Maybe (String, Bool)
rssImport' imp = fmap (\name -> (name, isWebAddress name)) $ listToMaybe $ mapMaybe argStr $ exprAppliedArgs imp
  where
    argStr ObjArr{oaArr=Just (Just (CExpr _ (CStr s)), _)} = Just s
    argStr _                                               = Nothing

-- | The imports a feed's page writes: the module declaring what an entry is.
--
-- The type is not in the standard library, so a page of entries says where the type is rather than
-- relying on the core it is given. Named as it is written in a program, for
-- 'Syntax.InferImport.importPathCandidates' to find in the standard library the way any other
-- import of it is found.
rssImports :: [RawFileImport]
rssImports = [mkRawFileImport $ rawStr rssStackModule]

-- | Where a feed's page sits in the graph, which is known without asking the feed.
--
-- Every feed's page writes the one import and brings in nothing else, so there is nothing here that
-- reading the feed could say. Answering without it is what keeps a doc server from asking every
-- feed a program mentions on every request it serves: the feed is asked when a reader opens it.
rssPreload :: RssReader -> Expr () -> ImportPreloadResult
rssPreload _ imp = do
  (name, web) <- rssImport imp
  return $ PreloadImport rssImports [] Nothing (rssReads name web)

-- | What a reading of a feed read on disk.
--
-- A feed read off the network read nothing on disk, so its page is read again on every read, which
-- is the only way six hours passing can ever be noticed — see 'Syntax.Ct.Prgm.ReadPaths'. Reading
-- it again costs the record and no network. A feed read from a file is read again when that file is
-- written to, like any other page.
rssReads :: String -> Bool -> ReadPaths
rssReads _ True     = Nothing
rssReads name False = Just [ReadFile name]

-- | A feed as the page it holds.
rssParse :: RssReader -> Expr () -> ImportParseResult
rssParse reader imp = do
  (name, web) <- rssImport imp
  now <- getCurrentTime
  (title, items) <- if web then webFeed reader name now else fileFeed name
  return $ ParsedImport (RawPrgm rssImports (feedStatements title items)) Nothing (rssReads name web)

-- | A feed asked of the network, or answered out of the record where it has been asked recently
-- enough, or where asking it failed.
--
-- Asking fails for as many reasons as there are between here and somebody else's machine, and none
-- of them is a reason to fail the read of a program which merely imports the feed. So a failure
-- with a record behind it is the record, and a failure with nothing behind it is a page saying so —
-- which a reader can see, where a read that stopped is a compiler that will not run.
--
-- 'Syntax.Rss.Cache.fcFetched' is stamped by an answer and not by an attempt, so a feed which could
-- not be reached is asked again by the next read rather than left for six hours.
--
-- The page is built out of the record whichever way the record was reached, so a read which asked
-- the feed, a read which did not, and a read which asked and got nothing all build the same page
-- from the same entries under the same heading. A page which changed shape according to which of
-- those a read was would be a page rewritten on every fetch, and this page is read again on every
-- read.
webFeed :: RssReader -> String -> UTCTime -> IO (Maybe String, [ItemEntry])
webFeed RssReader{rrCache=Nothing, rrFetch} name now = maybe (Nothing, []) (\(title, items) -> (Just title, items)) <$> askFeed rrFetch name now
webFeed RssReader{rrCache=Just cache, rrFetch} name now = do
  recorded <- cachedFeed cache name
  case recorded of
    Just record | feedIsFresh now record -> return $ recordPage record
    _ -> do
      answered <- askFeed rrFetch name now
      case answered of
        Nothing -> return $ maybe (Nothing, []) recordPage recorded
        Just (title, entries) -> do
          merged <- recordFeed cache name now title entries
          return (Just title, merged)
  where recordPage record = (Just (fcTitle record), fcItems record)

-- | Ask a feed, answering with what it called itself and what it carried, where it answered at all.
--
-- A feed which carries no entries is taken as not having answered. An answer which is not a feed at
-- all still parses — a server which is unwell answers with a web page saying so, and that page
-- parses as a feed of nothing — so an empty answer and a wrong answer cannot be told apart. Of the
-- two ways of being wrong about that, asking again next read costs one request and taking a wrong
-- answer for a right one costs six hours of a blank page and a record which says a live feed is
-- empty.
askFeed :: (String -> IO (Maybe String)) -> String -> UTCTime -> IO (Maybe (String, [ItemEntry]))
askFeed fetch name now = do
  fetched <- fetch name
  return $ case fetched >>= parseFeedSource of
    Just feed | entries@(_:_) <- feedEntries now feed -> Just (T.unpack $ getFeedTitle feed, entries)
    _ -> Nothing

-- | A feed read from a file, which is read every time and recorded nowhere.
--
-- Nothing is saved by a record of a file: a file is read as fast as a record of it, and a record
-- would leave an edit to it unseen for six hours. What the file is for is a feed which can be read
-- without asking anybody, so that reading feeds can be tested.
fileFeed :: String -> IO (Maybe String, [ItemEntry])
fileFeed name = do
  there <- doesFileExist name
  if not there
    then return (Nothing, [])
    else do
      contents <- readFile name
      now <- getCurrentTime
      -- Held to what an answer is by the same rule the network is held to, so that a file which is
      -- not a feed reads as no feed rather than as an empty one.
      return $ case parseFeedSource contents of
        Just feed | entries@(_:_) <- feedEntries now feed -> (Just (T.unpack $ getFeedTitle feed), entries)
        _ -> (Nothing, [])

-- | Ask a feed, answering with nothing where asking it did not work.
--
-- Every way of not working is one answer, because there is one thing to do about all of them. A
-- refused connection, a name which does not resolve, a certificate which does not check out and a
-- page which answers 404 are alike in that the feed was not read.
fetchFeed :: String -> IO (Maybe String)
fetchFeed url = do
  asked <- try (httpBS =<< parseRequest url) :: IO (Either SomeException (Response B.ByteString))
  return $ case asked of
    Left _         -> Nothing
    Right response -> Just $ BSU.toString $ getResponseBody response

-- | The entries of a feed as this read found them.
--
-- An entry with no name of its own is named by its address, and one with neither by its title. A
-- name is what says an entry read today is the entry read last week, so an entry which cannot be
-- named at all is dropped: kept, it would arrive again as a new entry on every read and the page
-- would grow without the feed having said anything.
feedEntries :: UTCTime -> Feed.Feed -> [ItemEntry]
feedEntries now feed = mapMaybe entry (getFeedItems feed)
  where
    entry item = case itemGuid item of
      Just guid -> Just $ ItemEntry {
        ieGuid = guid,
        ieTitle = text (getItemTitle item),
        ieLink = text (getItemLink item),
        iePublished = text (getItemDate item),
        ieDescription = text (getItemDescription item),
        ieFirstSeen = now
                                    }
      Nothing -> Nothing
    itemGuid item = case snd <$> getItemId item of
      Just guid | not (T.null guid) -> Just $ T.unpack guid
      _ -> case filter (not . null) [text (getItemLink item), text (getItemTitle item)] of
        found:_ -> Just found
        []      -> Nothing
    text = maybe "" T.unpack

-- | The statements a feed's page holds: what the feed calls itself, and then its entries.
feedStatements :: Maybe String -> [ItemEntry] -> [RawStatementTree RawExpr ()]
feedStatements title items = heading ++ map itemStatement items
  where
    heading = case title of
      Just title' -> [RawStatementTree (RawAnnot (rawComment title')) []]
      -- A feed which could not be read says so, rather than being a page which is simply empty. An
      -- empty page and a feed nobody could reach look the same, and only one of them is worth
      -- looking into.
      Nothing | null items -> [RawStatementTree (RawAnnot (rawComment couldNotRead)) []]
      Nothing -> []
    couldNotRead = "This feed could not be read. What is written below is what an earlier read of it recorded, if anything was."

-- | One entry, declared as the value it is.
itemStatement :: ItemEntry -> RawStatementTree RawExpr ()
itemStatement item = RawStatementTree (RawDeclStatement decl) []
  where
    decl = RawObjArr (Just (itemExpr item)) TypeObj Nothing [] Nothing Nothing

itemExpr :: ItemEntry -> RawExpr ()
itemExpr ItemEntry{ieGuid, ieTitle, ieLink, iePublished, ieDescription} = rawVal rssItemPath `applyRawArgs` [
  (Just $ partialKey (mkName rssItemTitle), rawStr ieTitle),
  (Just $ partialKey (mkName rssItemLink), rawStr ieLink),
  (Just $ partialKey (mkName rssItemGuid), rawStr ieGuid),
  (Just $ partialKey (mkName rssItemPublished), rawStr iePublished),
  (Just $ partialKey (mkName rssItemDescription), rawStr ieDescription)
                                                                                                           ]
