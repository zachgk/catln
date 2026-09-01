-- | Tests for reading an RSS feed out of what an earlier run recorded of it.
--
-- A feed is asked of somebody else's machine, so it is asked at most once every six hours and the
-- record answers in between; and a feed is a window rather than a document, so the page is built
-- out of every entry any read has seen rather than out of the last answer alone
-- ('Syntax.Rss.Cache').
--
-- So there are three things to hold it to. The feed must be asked when it is due to be asked and
-- not otherwise, since a record that is never used is no record and one that is never refreshed is
-- a page that stops moving. What the feed has stopped carrying must still be on the page, since
-- that is the whole reason the record is what the page is built from. And a feed which cannot be
-- reached must leave a program that imports it still readable, since the alternative is a compiler
-- which will not run because somebody else's server is down.
--
-- Nothing here opens a socket. The reader is asked how to ask a feed
-- ('Syntax.Rss.Parser.newRssReaderWith'), and what it is given here is a stub which counts what it
-- was asked and answers out of a script — so each case says what the feed said and reads what the
-- page became, with no third party in it.
module Syntax.RssCacheTests where

import           Control.Exception (IOException, try)
import           CtConstants       (mdAnnotText, rssItemPath, rssItemTitle)
import           Data.Aeson        (eitherDecodeFileStrict, encodeFile)
import           Data.IORef
import           Data.List         (isInfixOf)
import           Data.Maybe        (isNothing, listToMaybe, mapMaybe)
import           Data.Time.Clock   (UTCTime, addUTCTime, getCurrentTime)
import           Semantics         (testCtssConfig)
import           Semantics.Prgm    (Expr, impAbs)
import           Semantics.Types   (Constant (CStr), Name, PartialKey (..),
                                    mkName, nameStr, partialKey)
import           Syntax.Ct.Prgm    (ImportReader (..), ParsedImport (..),
                                    PreloadImport (..), RawExpr (..),
                                    RawObjArr (..), RawPrgm (..),
                                    RawStatement (..), RawStatementTree (..),
                                    ReadPath (..), newRecordedReadEnv,
                                    noImporters)
import           Syntax.Parsers    (mkDesCanonicalImportStr)
import           Syntax.Rss.Cache  (FeedCache (..), ItemEntry (..),
                                    feedCacheFile, rssCacheBuffer)
import           Syntax.Rss.Parser (newRssReaderWith)
import           System.Directory  (createDirectoryIfMissing,
                                    getTemporaryDirectory, removePathForcibly)
import           System.FilePath   (takeDirectory, (</>))
import           Test.Tasty
import           Test.Tasty.HUnit

-- | The feed every case here reads. Named as an address, because reading a feed out of a record is
-- what an address does and a path does not.
feedUrl :: String
feedUrl = "https://example.com/rss.xml"

-- | A cache root of its own for each case.
--
-- A root per case because a record is a thing shared between runs: two cases sharing one would be
-- two cases whose answers depended on which ran first.
caseRoot :: String -> IO FilePath
caseRoot name = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-rss-cache" </> name
  removePathForcibly dir
  createDirectoryIfMissing True dir
  return dir

-- | A feed saying these entries, as the XML a feed is written as.
feedSaying :: [(String, String)] -> String
feedSaying entries = concat [
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
  "<rss version=\"2.0\"><channel><title>Example</title><link>https://example.com/</link>",
  "<description>An example feed</description>",
  concatMap entry entries,
  "</channel></rss>"
                            ]
  where
    entry (guid, title) = concat [
      "<item><title>", title, "</title>",
      "<link>https://example.com/", guid, "</link>",
      "<description>&lt;p&gt;", title, "&lt;/p&gt;</description>",
      "<guid>", guid, "</guid></item>"
                                 ]

-- | What one read of the feed asked for, and what the page it built holds.
data Read' = Read' {
  rAsked :: Int,
  rPage  :: RawPrgm ()
                   }

-- | Read the feed once, with a reader of its own, answering out of the script and writing back
-- whatever it recorded.
--
-- A reader of its own because a reader holds a read's records open, and two reads sharing one would
-- be one read: what is being tested is what a later run makes of what an earlier one left on disk.
readFeed :: FilePath -> [Maybe String] -> IO Read'
readFeed root answers = do
  script <- newIORef answers
  asked <- newIORef (0 :: Int)
  env <- newRecordedReadEnv (Just root) noImporters
  reader <- newRssReaderWith (stub script asked) env
  imp <- feedExpr
  parsed <- irParse reader imp
  irFinish reader
  Read' <$> readIORef asked <*> return (piPrgm parsed)
  where
    stub script asked _ = do
      modifyIORef' asked (+ 1)
      answers' <- readIORef script
      case answers' of
        answer:rest -> writeIORef script rest >> return answer
        []          -> return Nothing

-- | The import expression a program naming this feed writes, as the compiler canonicalizes it.
feedExpr :: IO (Expr ())
feedExpr = impAbs <$> mkDesCanonicalImportStr testCtssConfig (concat ["rss(\"", feedUrl, "\")"])

-- | The entries a page holds, in the order it holds them.
pageTitles :: RawPrgm () -> [String]
pageTitles (RawPrgm _ statements) = mapMaybe title statements
  where
    title (RawStatementTree (RawDeclStatement RawObjArr{roaObj=Just e}) _) | isItem e = itemArg e rssItemTitle
    title _ = Nothing
    isItem e = case e of
      RawTupleApply _ (_, RawValue _ n) _ -> nameStr n == rssItemPath
      _                                   -> False

-- | One argument of a declared entry.
--
-- Named as an argument is named once it has been built, which is absolutely: 'Semantics.Types.partialKey'
-- makes the name absolute, so an argument written @title@ is @\/title@ by the time it is an argument.
itemArg :: RawExpr () -> String -> Maybe String
itemArg (RawTupleApply _ _ args) want = listToMaybe $ mapMaybe named args
  where
    named (_, RawObjArr{roaObj=Just (RawValue _ n), roaArr=Just (Just (RawCExpr _ (CStr v)), _)})
      | n == nameOf want = Just v
    named _ = Nothing
itemArg _ _ = Nothing

nameOf :: String -> Name
nameOf = pkName . partialKey . mkName

-- | What the page says of itself, which is what the feed calls itself or what went wrong reading it.
pageComment :: RawPrgm () -> String
pageComment (RawPrgm _ statements) = concat $ mapMaybe said statements
  where
    said (RawStatementTree (RawAnnot e) _) = itemArg e mdAnnotText
    said _                                 = Nothing

-- | The record on disk, as it stands, and nothing where there is none to stand.
recordOf :: FilePath -> IO (Maybe FeedCache)
recordOf root = do
  read' <- try (eitherDecodeFileStrict (feedCacheFile root feedUrl)) :: IO (Either IOException (Either String FeedCache))
  return $ case read' of
    Right (Right record) -> Just record
    _                    -> Nothing

-- | Put a record on disk, as a run which finished this long ago would have left it.
writeRecord :: FilePath -> UTCTime -> [(String, String)] -> IO ()
writeRecord root fetched entries = do
  createDirectoryIfMissing True (takeDirectory (feedCacheFile root feedUrl))
  encodeFile (feedCacheFile root feedUrl) $ FeedCache feedUrl "Example" fetched (map entry entries)
  where
    entry (guid, title) = ItemEntry guid title ("https://example.com/" ++ guid) "" ("<p>" ++ title ++ "</p>") fetched

rssCacheTests :: TestTree
rssCacheTests = testGroup "RssCacheTests" [
  testCase "a cold read asks the feed, and records what it was told" $ do
    root <- caseRoot "cold"
    read' <- readFeed root [Just $ feedSaying [("a", "First"), ("b", "Second")]]
    rAsked read' @?= 1
    pageTitles (rPage read') @?= ["First", "Second"]
    record <- recordOf root
    fmap (map ieTitle . fcItems) record @?= Just ["First", "Second"],

  testCase "a read within the buffer asks nothing, and answers the same" $ do
    root <- caseRoot "warm"
    _ <- readFeed root [Just $ feedSaying [("a", "First"), ("b", "Second")]]
    -- The second read is given a script which would answer differently if it were asked. It must
    -- not be asked, so the page must be the first read's.
    read' <- readFeed root [Just $ feedSaying [("c", "Third")]]
    rAsked read' @?= 0
    pageTitles (rPage read') @?= ["First", "Second"],

  testCase "a read past the buffer asks the feed again" $ do
    root <- caseRoot "expired"
    now <- getCurrentTime
    writeRecord root (addUTCTime (negate rssCacheBuffer - 60) now) [("a", "First")]
    read' <- readFeed root [Just $ feedSaying [("a", "First"), ("b", "Second")]]
    rAsked read' @?= 1
    pageTitles (rPage read') @?= ["First", "Second"],

  testCase "what the feed has dropped is still on the page" $ do
    root <- caseRoot "dropped"
    now <- getCurrentTime
    writeRecord root (addUTCTime (negate rssCacheBuffer - 60) now) [("a", "First"), ("b", "Second")]
    -- The feed has moved on and carries neither of them any more.
    read' <- readFeed root [Just $ feedSaying [("c", "Third")]]
    -- What it says now first, and what it used to say below.
    pageTitles (rPage read') @?= ["Third", "First", "Second"],

  testCase "an entry the feed has rewritten takes the new words and keeps its place" $ do
    root <- caseRoot "rewritten"
    now <- getCurrentTime
    writeRecord root (addUTCTime (negate rssCacheBuffer - 60) now) [("a", "First"), ("b", "Second")]
    read' <- readFeed root [Just $ feedSaying [("b", "Second, corrected")]]
    pageTitles (rPage read') @?= ["Second, corrected", "First"]
    -- Kept as long as it was first seen, which the answer that corrected it cannot say.
    record <- recordOf root
    let firstSeen = fmap (map ieFirstSeen . fcItems) record
    fmap (\seen -> length seen == 2 && all (< now) seen) firstSeen @?= Just True,

  testCase "a feed which cannot be reached is the record, and is asked again next time" $ do
    root <- caseRoot "unreachable"
    now <- getCurrentTime
    let fetched = addUTCTime (negate rssCacheBuffer - 60) now
    writeRecord root fetched [("a", "First")]
    read' <- readFeed root [Nothing]
    rAsked read' @?= 1
    pageTitles (rPage read') @?= ["First"]
    -- Not stamped, so the next read is not held off for another six hours by an attempt which
    -- answered nothing.
    record <- recordOf root
    fmap fcFetched record @?= Just fetched,

  testCase "a feed which cannot be reached at all is a page saying so, not a read which failed" $ do
    root <- caseRoot "never-reached"
    read' <- readFeed root [Nothing]
    pageTitles (rPage read') @?= []
    assertBool "the page says the feed could not be read" $ "could not be read" `isInfixOf` pageComment (rPage read'),

  testCase "an answer which is not a feed is read as no feed" $ do
    root <- caseRoot "not-a-feed"
    -- A server which is unwell answers with a page saying so, and that page parses as a feed of
    -- nothing. Recording it would be recording that a live feed is empty.
    read' <- readFeed root [Just "<html><body>Nothing to see</body></html>"]
    pageTitles (rPage read') @?= []
    -- Nothing is recorded of an answer which said nothing, so the next read asks again.
    record <- recordOf root
    assertBool "nothing was recorded" $ isNothing record,

  testCase "a record of another feed is read past" $ do
    root <- caseRoot "another-feed"
    now <- getCurrentTime
    createDirectoryIfMissing True (takeDirectory (feedCacheFile root feedUrl))
    -- Written under the name this feed hashes to, but saying it is of another one.
    encodeFile (feedCacheFile root feedUrl) $ FeedCache "https://example.com/other.xml" "Other" now [ItemEntry "x" "Another feed's entry" "" "" "" now]
    read' <- readFeed root [Just $ feedSaying [("a", "First")]]
    rAsked read' @?= 1
    pageTitles (rPage read') @?= ["First"],

  testCase "a warm read builds the same page as the read which fetched" $ do
    root <- caseRoot "same-page"
    -- The page is read again on every read, so a warm read building a different page from a cold
    -- one would be a page rewritten whenever the feed happened to be asked.
    cold <- readFeed root [Just $ feedSaying [("a", "First"), ("b", "Second")]]
    warm <- readFeed root []
    rAsked warm @?= 0
    pageComment (rPage warm) @?= pageComment (rPage cold)
    pageTitles (rPage warm) @?= pageTitles (rPage cold),

  testCase "a feed read off the network is read again every time" $ do
    root <- caseRoot "reads"
    env <- newRecordedReadEnv (Just root) noImporters
    reader <- newRssReaderWith (const $ return Nothing) env
    imp <- feedExpr
    preloaded <- irPreload reader imp
    -- Nothing on disk to be timed by, so the page is read again on every read -- which is the only
    -- way six hours passing is ever noticed.
    plReads preloaded @?= Nothing
    -- And its shape is known without asking the feed, so walking the graph costs no network.
    assertBool "the feed was not read to place the page" $ isNothing (plParsed preloaded)
    length (plImports preloaded) @?= 1,

  testCase "a feed read from a file is read again when the file is written to" $ do
    root <- caseRoot "file-reads"
    env <- newRecordedReadEnv (Just root) noImporters
    reader <- newRssReaderWith (const $ return Nothing) env
    imp <- impAbs <$> mkDesCanonicalImportStr testCtssConfig "rss(\"test/Integration/code/artifacts/xkcd.xml\")"
    preloaded <- irPreload reader imp
    parsed <- irParse reader imp
    assertBool "the file it was read from" $ case plReads preloaded of
      Just [ReadFile file] -> "xkcd.xml" `isInfixOf` file
      _                    -> False
    -- Read off disk with nothing asked of anybody.
    length (pageTitles (piPrgm parsed)) @?= 4
                                          ]
