--------------------------------------------------------------------
-- |
-- Module    :  WebDos
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This file defines the server for the Catln webdocs.
--------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module WebDocs where

import           Data.Aeson                    (ToJSON, Value, eitherDecode,
                                                encode, object, (.=))
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Key                as AK
import qualified Data.Aeson.KeyMap             as AKM
import           Web.Scotty

import           Data.ByteString.Builder       (lazyByteString)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as H
import           Data.Scientific               (floatingOrInteger)
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding       (decodeUtf8, encodeUtf8)
import           GHC.Generics                  (Generic)
import           Network.Wai.Middleware.Static

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.STM        (STM, atomically, orElse,
                                                readTVar, registerDelay, retry)
import           Control.Exception             (SomeException, evaluate, try)
import           Control.Monad                 (unless, when)
import           Control.Monad.Trans           (lift)
import           CRes
import           CtConstants                   (printAnnot, printAnnotText)
import           CtService
import           Data.Bifunctor                (Bifunctor (first))
import           Data.Graph
import           Data.Maybe                    (listToMaybe)
import           Data.UUID                     (UUID, fromText)
import           Eval                          (evalAllTargetModes, evalAnnots,
                                                prgmFromGraphData)
import           Eval.Common                   (EvalMode, SSF (..),
                                                Val (FloatVal, IntVal, StrVal),
                                                newEvalService)
import           Eval.Runtime.WebDocs          (sidebarOrder)
import           MapMeta                       (interleaveMeta, interleavePrgm,
                                                parseMetaIDScope, zip4MetaFun)
import           Semantics                     (buildCtssConfig)
import           Semantics.Prgm
import           Semantics.Types               (mkName, nameStr, partialKey)
import           Syntax.Ct.Desugarf            (desFinalPass, desPrgm)
import           Syntax.Ct.Desugarf.Expr       (desFileImport)
import           Syntax.Ct.Formatter           (formatExpr)
import           Syntax.Ct.MapRawMeta          (addRawPrgmMetaIDs,
                                                mapMetaRawPrgm, replaceRawExpr)
import           Syntax.Ct.Parser.Expr         (pExpr)
import           Syntax.Ct.Parser.Syntax       (ParseMetaDat)
import           Syntax.Ct.Prgm                (RawExpr (RawValue),
                                                RawPrgm (RawPrgm),
                                                RawStatement (RawAnnot),
                                                RawStatementTree (RawStatementTree),
                                                mkRawFileImport,
                                                rawExprAppliedArgsMap)
import           Syntax.Parsers                (mkDesCanonicalImportStr,
                                                pageLinkDir)
import           Text.Megaparsec               (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck                     (typecheckPrgms)
import           TypeCheck.Common              (TypedMeta, typeCheckToRes)
import           Utils

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

-- | A program as the webdocs show it: written as it was written, with what each stage made of each
-- part of it hung off the part it was written as — the type that part was inferred to have, how it
-- is to be evaluated, the value it displays as, and the source of the computation a @#print@ shows
-- the result of (see 'printSources'). Each is absent for a part of the program the stage said
-- nothing about, which for every one of them includes a program that failed to compile, and for the
-- last of them includes every statement that is not a print.
type ShownPrgm = RawPrgm (Maybe TypedMeta, Maybe EvalMode, Maybe Val, Maybe String)

-- | What @/api/page@ answers with: the page, and where it sits among the pages the doc server holds.
-- The neighbours and the directory are the server's answers to questions the webdocs would otherwise
-- answer by keeping their own copy of the page list.
data PageResp p = PageResp {
    pgPrgm   :: p
  , pgSource :: Maybe String
  -- | The pages either side of this one in the order the navigation shows them. Absent at the ends
  -- of that order, where there is no page to offer.
  , pgPrev   :: Maybe String
  , pgNext   :: Maybe String
  -- | The directory a relative link written in the page's documentation resolves against.
  --
  -- The directory the page was imported relative to for nearly every page, and whatever the syntax
  -- which read the page says instead ('Syntax.Parsers.pageLinkDir') — a note of an Obsidian vault
  -- names another note by that note's path within the vault, so a note's links resolve against the
  -- vault rather than against the folder it sits in.
  , pgDir    :: Maybe FilePath
  }
  deriving (Generic, ToJSON)

-- | The entries either side of one in a reading order. An entry at an end has nothing on that side,
-- and so is answered with nothing rather than with the entry that wraps round to the other end.
neighbours :: (Eq a) => a -> [a] -> (Maybe a, Maybe a)
neighbours x xs = case break (== x) xs of
  (before, _:after) -> (listToMaybe $ reverse before, listToMaybe after)
  _                 -> (Nothing, Nothing)

maybeJson :: (ToJSON a) => CRes a -> ActionM ()
maybeJson res = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  raw $ resBody res

-- | What a request is answered with, as the bytes of it. The same for a request answered once and
-- for every frame of one answered over and over, so what a reader who asked to be kept up to date
-- is shown cannot come apart from what a reader who asked once is shown.
resBody :: (ToJSON a) => CRes a -> BL.ByteString
resBody (CRes notes r) = encode $ Success r notes
resBody (CErr notes)   = encode (ResFail notes :: ResSuccess () CNote)

-- | An exception thrown where an answer was expected, said as the answer.
--
-- For a stream, where letting one through is the connection dropping — which an @EventSource@ answers
-- by opening another, so a page which cannot be compiled at all would be compiled again every few
-- seconds forever. Said instead, which is also what a reader needs: what failed is the very thing
-- they are looking at, and a request answered once already reports a build that failed this way.
exceptionBody :: SomeException -> BL.ByteString
exceptionBody e = encode (ResFail [MkCNote $ GenCErr Nothing (show e)] :: ResSuccess () CNote)

-- | Answer with what is being shown, and go on answering it for a reader who asked to be kept up to
-- date.
--
-- Which reads reach a display is decided here rather than told to the reader. What a change is
-- depends on the whole import graph — a write low in the standard library invalidates nearly every
-- page there is — while what a reader wants to hear about is the one thing in front of them. So the
-- server is told what is being shown ('Shown') and answers with that, and a change to a page nobody
-- has open crosses nothing.
--
-- Asked for by @Accept@, which is what a browser's @EventSource@ sends, so the same URL answers a
-- reader who wants to be kept up to date and one who wants to be answered once.
serve :: (ToJSON a) => CTSS -> Shown -> IO (CRes a) -> ActionM ()
serve ctss shown compute = do
  accept <- header "Accept"
  if maybe False (T.isInfixOf eventStream) accept
    then do
      setHeader "Content-Type" eventStream
      -- A frame is the answer of the moment, so an answer held from before the last change would be
      -- the one thing a display which follows its page must not show.
      setHeader "Cache-Control" "no-cache"
      -- Followed before the first answer is computed, so a change arriving while it is being
      -- computed is one the next wait ends on rather than one nothing hears about.
      stream $ \write flush -> ctssFollow ctss shown $ \await -> do
        let sending sent = do
              -- A failure is said as the answer rather than let through, because letting one through
              -- is the connection dropping — which an 'EventSource' answers by opening another, so a
              -- page which cannot be compiled at all would be compiled again every few seconds
              -- forever. A request answered once has no such loop to fall into and reports it as the
              -- failure it is, which is why this is here and not in 'answering'.
              answer <- either exceptionBody id <$> try (answering ctss compute)
              -- An answer the reader already has is not sent again. What a display is followed by is
              -- what it is compiled FROM, which is more than what it SHOWS: a navigation is compiled
              -- in the scope of a page, so every edit to that page reaches it, while which pages
              -- there are — the whole of what it shows — has not changed. Nothing can tell those
              -- apart ahead of the answer, since what an annotation reads is its own business, so
              -- they are told apart by the answer itself.
              when (Just answer /= sent) $ do
                write $ "data: " <> lazyByteString answer <> "\n\n"
                flush
              -- A connection which has gone is found by writing to it, so something is written
              -- whether or not anything has changed. An 'EventSource' passes over a comment frame,
              -- which is what this is: it is here to fail rather than to be read.
              untilChanged await keepAlive $ write ": \n\n" >> flush
              sending (Just answer)
        sending Nothing
    else do
      answer <- liftAndCatchIO $ answering ctss compute
      setHeader "Content-Type" "application/json; charset=utf-8"
      raw answer

-- | Compute an answer, as the bytes of it, counted as work a reader is waiting for.
--
-- The bytes are made here rather than left for whoever writes them. A page's annotations are
-- evaluated lazily ('CtService.getEvalAnnots'), and so is the graph of values they come to, so much
-- of what the reader is waiting for is in fact paid by whatever first walks the answer — which is
-- the encoding. Left to the write, that work would happen after the service had said it had stopped
-- working, and a reader would be shown a bar which went away while the page was still being made.
--
-- Failures are left to whoever asked, since what to make of one differs between a request answered
-- once and a connection held open.
answering :: (ToJSON a) => CTSS -> IO (CRes a) -> IO BL.ByteString
answering ctss compute = ctssWorking ctss $ do
  answer <- resBody <$> (ctssRead ctss >> compute)
  _ <- evaluate (BL.length answer)
  return answer

eventStream :: T.Text
eventStream = "text/event-stream"

-- | How long a stream with nothing to say waits before saying nothing, to find whether anyone is
-- still there.
keepAlive :: Int
keepAlive = 30000000

-- | The soonest a stream of what the service is working on says anything again.
atMostEvery :: Int
atMostEvery = 200000

-- | Wait for a display to be reached, doing something every so often for as long as it is not.
--
-- The waiting and the timing out are one transaction, so a change arriving as the wait times out is
-- either waited out or left for the next wait, and never both or neither.
untilChanged :: STM () -> Int -> IO () -> IO ()
untilChanged await every act = do
  timer <- registerDelay every
  timedOut <- atomically $ (await >> return False) `orElse` do
    late <- readTVar timer
    unless late retry
    return True
  when timedOut $ act >> untilChanged await every act

-- | The page as it now is, ready to be shown. Lifted out of the request for it so that a reader
-- following the page is shown the same thing, computed the same way, on every change to it.
pageResp :: CTSS -> FileImport -> IO (CRes (PageResp ShownPrgm))
pageResp ctss prgmName' = runCResT $ do
  rawPrgm' <- ctssLookupReq ssfRaw prgmName' ctss
  maybeTPrgmRes <- lift $ runCResT $ do
    -- The page and what it imports, rather than every page the doc server holds. What a page
    -- shows is compiled from the program the page is in, so the pages beside it are pages this
    -- request would compile to show nothing of. Inside the inner run because it is now a
    -- request which can fail: 'ctssGet' passes over a page it has not computed, where
    -- 'ctssGetFrom' requires the ones it was asked for, and a page which cannot be compiled is
    -- still a page to serve with its source and what it says.
    tprgms <- ctssGetFrom ssfTPrgm prgmName' ctss
    case graphLookup prgmName' tprgms of
      Just tprgm -> do
        reachTprgms <- asCResT $ prgmFromGraphData prgmName' tprgms
        return (tprgm, reachTprgms)
      Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName')
  let maybeTPrgm = fmap fst maybeTPrgmRes
  let maybeTPrgmFull = fmap snd maybeTPrgmRes
  maybeAnnots <- lift $ runCResT $ getEvalAnnots ctss prgmName'
  let tprgm' = maybe H.empty interleavePrgm (cresToMaybe maybeTPrgm)
  let targetModes = maybe H.empty evalAllTargetModes (cresToMaybe maybeTPrgmFull)
  let annots' = maybe H.empty (H.fromList . map (first (getMetaID . getExprMeta))) (cresToMaybe maybeAnnots)
  let prgm' = mapMetaRawPrgm (zip4MetaFun (interleaveMeta tprgm') (interleaveMeta targetModes) (interleaveMeta annots') (interleaveMeta $ printSources rawPrgm')) rawPrgm'
  maybeSource <- lift $ runCResT $ getSource ctss prgmName'
  -- The pages either side of this one travel with it, so a page knows where it sits without the
  -- webdocs holding a list of every page to work it out from. A page the order cannot place is
  -- reported rather than served as though it were an end of the order: 'sidebarOrder' fails only
  -- when a page has no path to open it by or two pages share one, which is a doc server that
  -- cannot be navigated at all.
  pages <- lift $ ctssKeys ctss
  order <- either fail return $ sidebarOrder pages
  let (prevPage, nextPage) = maybe (Nothing, Nothing) (`neighbours` order) (impDisp prgmName')
  linkDir <- lift $ pageLinkDir prgmName'
  return $ PageResp prgm' (cresToMaybe maybeSource) prevPage nextPage linkDir

-- | The annotation as it now compiles, in the scope of the page it names. Lifted out of the request
-- for it for the same reason as 'pageResp'.
annotResp :: CTSS -> Maybe FileImport -> String -> IO (CRes ShownPrgm)
annotResp ctss scope' annot = do
  runCResT $ do
    rawPrgm <- case runParser pExpr "<annot>" annot of
      Left err     -> fail $ show $ errorBundlePretty err
      Right parsed -> return (RawPrgm [] [RawStatementTree (RawAnnot parsed) []])
    let annotName = desFileImport $ mkRawFileImport $ RawValue emptyMetaN "<annot>"
    let rawPrgm' = addRawPrgmMetaIDs (parseMetaIDScope annotName) rawPrgm
    (annotDesPrgm, annotPrgmName, _) <- asCResT $ desPrgm (rawPrgm', annotName, [])
    let annotDesPrgm' = desFinalPass (annotDesPrgm, annotPrgmName, [], [])
    -- The pages of the scope which can be evaluated, with the rest reported rather than fatal. A
    -- page which did not typecheck used to take every annotation in the scope out with it, including
    -- a navigation that reads no program at all — see 'ctssEvaluableFrom' for why it cannot simply
    -- be left in.
    --
    -- One fetch rather than two, so the program the annotation is checked against cannot be a
    -- different one from the program it is evaluated in.
    tprgmsTrace <- ctssEvaluableFrom ssfTPrgmWithTrace scope' ctss
    let tprgms = fmapGraph fst3 tprgmsTrace
    let annotDeps = map snd3 $ graphToNodes tprgms
    [(annotTprgm, _, _)] <- asCResT $ typeCheckToRes $ typecheckPrgms [(annotDesPrgm', [], map fst3 $ graphToNodes tprgmsTrace)]
    let joinedTprgms = graphFromEdges ((annotTprgm, annotPrgmName, annotDeps) : graphToNodes tprgms)
    let targetModes = evalAllTargetModes annotTprgm
    -- The service is scoped to the page the annotation was asked about rather than to the synthetic
    -- page holding the annotation itself, which is a page of nothing that nobody wrote.
    -- As in 'CtService.ctssCallDShow', nothing keeps what this answers, so nothing reads what it
    -- asked about.
    svc <- lift $ newEvalService ctss scope' =<< serviceToday ctss
    -- As in 'CtService.getEvalAnnots', with no count of its own. This is the path the navigation and
    -- the annotation route take, which on a doc server that has just started is the slowest thing a
    -- reader waits at.
    lift $ ctssReport ctss StepEvaluating 0 0 (impDisp =<< scope')
    maybeAnnots <- asCResT $ evalAnnots (Just svc) annotPrgmName joinedTprgms
    let annots' = H.fromList $ map (first (getMetaID . getExprMeta)) maybeAnnots
    return $ mapMetaRawPrgm (zip4MetaFun (interleaveMeta $ interleavePrgm annotTprgm) (interleaveMeta targetModes) (interleaveMeta annots') (interleaveMeta $ printSources rawPrgm')) rawPrgm'

-- | The arguments of an @/api/call@, as the values they are called with.
--
-- Sent as a JSON object of argument name to value, which is what a caller has: a call names a
-- function of the program and says what to compute over, and what it has to say that with is
-- whatever the reader typed or picked. Read as values rather than written into Catln source for the
-- compiler to read back, which is the whole of what this route is for — a query holding a quote is a
-- query rather than the start of a parse error.
--
-- Only the scalars a value can be rebuilt from ('Eval.ExprBuilder.valToEExpr'). A structure would be
-- a shape agreed between the webdocs and a program's declarations, which is a thing to want when
-- something wants it; anything else is reported rather than approximated.
callArgs :: T.Text -> CRes [(String, Val)]
callArgs src = case eitherDecode (encodeUtf8 src) of
  Left err -> fail $ printf "The arguments of the call are not JSON: %s" err
  Right (A.Object o) -> mapM arg $ AKM.toList o
  Right _ -> fail "The arguments of the call are not a JSON object of argument name to value"
  where
    arg (k, v) = (AK.toString k,) <$> argVal (AK.toString k) v
    argVal _ (A.String s) = return $ StrVal $ T.unpack $ T.fromStrict s
    argVal _ (A.Number n) = return $ either FloatVal IntVal $ floatingOrInteger n
    argVal k v = fail $ printf "The argument %s was given %s, which is not a String or a number" k (show $ encode v)

-- | The computation each @#print@ of a program displays the result of, as the source it was written
-- as, keyed by the annotation it belongs to.
--
-- Carried with the page because a print is shown as the value it printed and nothing else — the
-- expression that produced it is not on screen to point at, and a reader wanting to debug that
-- computation has nothing to name it by. The webdocs cannot work it out either: an operator apply
-- carries no source position of its own (see @Syntax.Ct.Parser.Expr.mkOp2@), so a print's span
-- cannot be recovered from the page's text, and a prefix operator would silently come back without
-- its operator. So the compiler renders it, through the same 'formatExpr' that writes the page back
-- out when it is formatted.
--
-- Only prints, which is why it is a map rather than a fourth thing every meta carries: an annotation
-- that shows what it was written as is already readable on the page.
printSources :: RawPrgm ParseMetaDat -> H.HashMap UUID String
printSources (RawPrgm _ statements) = H.fromList $ concatMap ofTree statements
  where
    ofTree (RawStatementTree statement subStatements) = ofStatement statement ++ concatMap ofTree subStatements
    -- Matched against the absolute name, which is what the @> @ a print is written as parses to
    -- (see @Syntax.Ct.Parser.pPrintStatement@), and what 'Syntax.Ct.Formatter' matches to write it
    -- back out.
    ofStatement (RawAnnot annot) | nameStr (exprPath annot) == printAnnot =
      case H.lookup (partialKey (mkName printAnnotText)) $ rawExprAppliedArgsMap annot of
        Just (Just (_, Just p)) -> [(getMetaID $ getExprMeta annot, formatExpr p)]
        _                       -> []
    ofStatement _ = []

-- | The service a doc server answers from: the files it was given, read and watched.
--
-- Watched because a doc server is read from over and over. Every request brings the service into
-- line with the files first ('ctssRead'), which the watch makes cost what the change costs
-- rather than what the program costs, so that the pages served are the files as they now are
-- without every request re-reading every file to find out.
--
-- Read but not built. What to build is what a request asks about, which the request says and this
-- cannot: building everything here is a doc server which cannot answer at all until it has compiled
-- every page it holds, however few of them the reader opens.
mkDocService :: [String] -> IO CTSS
mkDocService baseFileNames = do
  ss <- ctssBaseFiles buildCtssConfig baseFileNames
  ctssWatch ss
  -- Read when the files change, rather than when a request arrives. A reader following a page is not
  -- making requests — that is what following is — so a service which only read when asked would go
  -- on showing them a page whose file has been written to.
  ctssWatchLive ss
  return ss

docApiBase :: Bool -> CTSS -> ScottyM ()
docApiBase readonly ctss = do

  -- What the service is working on, said again as it changes, for a reader waiting at something which
  -- has not arrived.
  --
  -- Its own route rather than frames on the page's, because it has to answer WHILE the work it reports
  -- on is running, and a page is answered by computing it. Computing a page takes the service's lock
  -- and holds it for a whole component; this reads a 'TVar' which sits beside that lock and takes
  -- nothing. For the same reason it does not read the service ('ctssRead') — that would put it behind
  -- exactly the read it is here to report on.
  --
  -- Not a display: it is about no page, so it is not registered with 'ctssFollow' and no read wakes
  -- it. What wakes it is the work itself.
  get "/api/progress" $ do
    accept <- header "Accept"
    if maybe False (T.isInfixOf eventStream) accept
      then do
        setHeader "Content-Type" eventStream
        setHeader "Cache-Control" "no-cache"
        stream $ \write flush -> do
          let sending sent = do
                now <- ctssProgress ctss
                when (Just now /= sent) $ do
                  write $ "data: " <> lazyByteString (resBody (pure now :: CRes Progress)) <> "\n\n"
                  flush
                -- A reader reads a bar, not every step of one. A read takes up a page thousands of
                -- times over and each is a change, so what is said next is said no sooner than this.
                -- The wait below then reads where the work has got to rather than where it went
                -- through, so what those thousands of changes cost is one frame.
                threadDelay atMostEvery
                untilChanged (ctssProgressChanged ctss now) keepAlive $ write ": \n\n" >> flush
                sending (Just now)
          sending Nothing
      else do
        now <- liftAndCatchIO $ ctssProgress ctss
        maybeJson (pure now :: CRes Progress)

  get "/api/config" $ do
    -- The page the docs open at, which is the doc server's own answer rather than a page picked out
    -- of a listing of all of them.
    rootPage <- liftAndCatchIO $ ctssRootPage ctss
    maybeJson (return (object ["readonly" .= readonly, "rootPage" .= rootPage]) :: CRes Value)

  -- Writes are rejected under --readonly so a shared server cannot be edited, no matter
  -- what the client believes it is allowed to do.
  post "/api/write" $ do
    prgmName <- param "prgmName"
    src <- body
    resp <- liftAndCatchIO $ runCResT $ if readonly
      then fail "Saving is disabled because the doc server is running with --readonly"
      else do
        prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
        setSource ctss prgmName' (T.unpack $ decodeUtf8 src)
    maybeJson resp

  -- Replaces one expression of a page and saves the page back as the file it was read from. That is
  -- what a value shown in the webdocs edits: it names the expression it was compiled from by its
  -- meta id, and says what it should be as the Catln source of the new one. Writing the file is left
  -- to the parser which read it, so a checkbox checked in an Obsidian note is written back as
  -- markdown rather than as the program the note was read into.
  --
  -- Guarded by @--readonly@ and does its own reading, for the same reasons as @/api/write@.
  post "/api/write-expr" $ do
    prgmName <- param "prgmName"
    metaID <- param "metaId"
    src <- body
    resp <- liftAndCatchIO $ runCResT $ if readonly
      then fail "Saving is disabled because the doc server is running with --readonly"
      else do
        prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
        metaID' <- maybe (fail $ printf "The expression id %s is not an id" (T.unpack metaID)) return $ fromText $ T.toStrict metaID
        expr <- case runParser pExpr "<expr>" (T.unpack $ decodeUtf8 src) of
          Left err     -> fail $ show $ errorBundlePretty err
          Right parsed -> return parsed
        rawPrgm <- getRawPrgmOf ctss prgmName'
        case replaceRawExpr metaID' expr rawPrgm of
          Nothing -> fail $ printf "The page %s holds no expression %s to replace" prgmName (T.unpack metaID)
          Just rawPrgm' -> setRawPrgm ctss prgmName' rawPrgm'
    maybeJson resp

  -- A page is shown from its own program and the programs it imports, so a read of any of those
  -- reaches it. It also names the pages either side of it in the order pages are read in, so a page
  -- appearing or going reaches it too, which 'changeReaches' answers for everything alike.
  get "/api/page" $ do
    prgmName <- param "prgmName"
    prgmName' <- liftAndCatchIO $ mkDesCanonicalImportStr buildCtssConfig prgmName
    serve ctss (Shown (Just prgmName')) (pageResp ctss prgmName')

  get "/api/annot" $ do
    annot <- param "annot"
    -- An annotation is local to the program it is written in, so evaluating a free-standing one has
    -- to say which program it is written in. Naming a page evaluates the annotation as though it sat
    -- in that page: every stage reported about it, and the synthetic program's own dependencies,
    -- narrow to that page and what it imports. Without a name the annotation is evaluated against
    -- every loaded file, which is what a listing of the whole program wants.
    scope <- fmap Just (param "prgmName") `rescue` (\_ -> return Nothing)
    scope' <- liftAndCatchIO $ traverse (mkDesCanonicalImportStr buildCtssConfig) scope
    -- Scoped to the page it names, or to every page there is where it names none.
    serve ctss (Shown scope') (annotResp ctss scope' annot)

  -- Calls a function the program holds, with what to compute over sent as its arguments, for the
  -- value it displays as. This is how the webdocs ask the compiler to compute something about what a
  -- reader is doing: an annotation says what it is about when it is WRITTEN, so a display that
  -- answers to typing cannot be one. See 'CtService.ctssCallDShow'.
  --
  -- Scoped like @/api/annot@ and for the same reason, and named the same way, so that a call and an
  -- annotation are the same question asked of the same program.
  get "/api/call" $ do
    fun <- param "fun"
    args <- param "args" `rescue` (\_ -> return "{}")
    scope <- fmap Just (param "prgmName") `rescue` (\_ -> return Nothing)
    scope' <- liftAndCatchIO $ traverse (mkDesCanonicalImportStr buildCtssConfig) scope
    serve ctss (Shown scope') (runCResT $ do
      args' <- asCResT $ callArgs args
      ctssCallDShow ctss scope' (T.unpack fun) args')


docApi :: Bool -> [String] -> IO ()
docApi readonly baseFileNames = do

  ctss <- mkDocService baseFileNames

  scotty 31204 $ docApiBase readonly ctss

docServe :: Bool -> [String] -> IO ()
docServe readonly baseFileNames = do

  let handleIndex p = case p of
        "" -> Just "index.html"
        _  -> Just p

  ctss <- mkDocService baseFileNames

  scotty 8080 $ do

    docApiBase readonly ctss

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/dist")

    get (regex ".*") $ file "webdocs/dist/index.html"
