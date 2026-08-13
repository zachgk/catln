--------------------------------------------------------------------
-- |
-- Module    :  CtService
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is a top-level manager for the compilation pipeline.
-- It supports partial and incremental compilation and rebuilding, for both the Main build and pieces like webdocs or IDE integration.
--------------------------------------------------------------------

module CtService where
import           Control.Concurrent      (MVar, modifyMVar_, newMVar, readMVar,
                                          withMVar)
import           Control.Monad           (forM, forM_, unless, when)
import           Control.Monad.Trans     (lift)
import           CRes
import           CtConstants             (testAnnot)
import           Data.Graph
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (elemIndex, intercalate, partition,
                                          sortOn)
import           Data.Maybe
import           Data.UUID               (UUID)
import           Eval                    (evalAnnots, evalBuild, evalBuildAll,
                                          evalDebugPrgm, evalPageValues,
                                          evalRun, evalRunMock)
import           Eval.Common             (EvalMetaDat, MockIO, StagePrgms (..),
                                          TExpr, TestResult,
                                          Val (StrVal, TupleVal), oaTestId)
import           Eval.Testing            (TestDesc (..), evalPrints, evalTest,
                                          runTestDesc)
import           FileWatch               (Watcher, allCovered, covered,
                                          startWatcher, watchReads, withPending)
import           Semantics               (CTSSConfig)
import           Semantics.Annots        (hasAnnot)
import           Semantics.Prgm
import           Semantics.Types         (Name)
import           Syntax.Ct.Desugarf      (desFinalPasses, desPrgm, validPrgm)
import           Syntax.Ct.MapRawMeta    (rawStatementExprIds)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           Syntax.Parsers
import           Text.Printf
import           TypeCheck               (typecheckPrgms)
import           TypeCheck.Common        (TPrgm, TraceConstrain, VPrgm,
                                          typeCheckToRes)
import           Utils

type TBPrgm = Prgm TExpr EvalMetaDat

newtype CTSS = CTSS (MVar CTSSDat)

-- Main data type for CtService Status
data CTSSDat = CTSSDat {
  ctssConfig        :: CTSSConfig,
  ctssBaseFileNames :: [String],
  ctssData          :: GraphData SSF FileImport,
  -- | A watch over the files the pages were read from, for a service read from over and over.
  --
  -- Without one a read looks at every file the service holds to find what changed, which costs what
  -- the program is; with one it costs what the change is. Absent by default because a service read
  -- once, or told what changed by whoever changed it, has nothing to gain from it.
  ctssWatcher       :: Maybe Watcher,
  -- | Held for a whole read, so that two reads cannot each take half of what has changed.
  --
  -- Not the lock the graph is behind: a read holding that one across its parsing would put every
  -- read behind a build, which holds it for a whole SCC.
  ctssReadLock      :: MVar ()
                                       }

data SSF = SSF {
  ssfSource         :: Maybe String,
  ssfRaw            :: CRes (RawPrgm ParseMetaDat),
  -- | What the read which produced this page left: the imports it walked on from, and when the
  -- files it read were last written. Handed back to the next read, which gives this page back
  -- untouched while those files are unchanged, so a page is parsed again only when something it was
  -- read from has been written to.
  ssfRead           :: ReadPage,
  ssfBuilt          :: Bool,
  ssfDes            :: Maybe (CRes (Prgm Expr ParseMetaDat)),
  ssfTPrgmWithTrace :: Maybe (CRes (TPrgm, VPrgm, TraceConstrain)),
  ssfTPrgm          :: Maybe (CRes TPrgm),
  ssfTBPrgm         :: Maybe (CRes TBPrgm),
  ssfAnnots         :: Maybe (CRes [(Expr EvalMetaDat, Val)]),
  -- | The tests this page wrote, run. Running a test is not part of building a page, so this is
  -- filled in when something asks for it (see 'ctssTestsFrom') rather than by the pipeline.
  ssfTests          :: Maybe (CRes [TestResult])
               }

emptyCTSS :: CTSSConfig -> IO CTSS
emptyCTSS config = do
  v <- newMVar =<< newCTSSDat config []
  return $ CTSS v

newCTSSDat :: CTSSConfig -> [String] -> IO CTSSDat
newCTSSDat config baseFileNames = CTSSDat config baseFileNames (graphFromEdges []) Nothing <$> newMVar ()

-- | A page as a read of it left it, with nothing yet built from it.
--
-- The source and the program are taken from the read rather than given, so that what a page holds
-- and what it was read from cannot come apart.
newSSF :: ReadPage -> SSF
newSSF rp = SSF { ssfSource = rpSource rp
                , ssfRaw = pure (rpPrgm rp)
                , ssfRead = rp
                , ssfBuilt = False
                , ssfDes = Nothing
                , ssfTPrgmWithTrace = Nothing
                , ssfTPrgm = Nothing
                , ssfTBPrgm = Nothing
                , ssfAnnots = Nothing
                , ssfTests = Nothing
                }

ctssSetFiles :: CTSS -> [String] -> IO ()
ctssSetFiles (CTSS ssmv) baseFileNames = do
  modifyMVar_ ssmv $ \ctss -> return ctss{ctssBaseFileNames=baseFileNames}
  ctssRead (CTSS ssmv)

ctssBaseFiles :: CTSSConfig -> [String] -> IO CTSS
ctssBaseFiles config baseFileNames = do
  v <- newMVar =<< newCTSSDat config baseFileNames
  let ctss = CTSS v
  ctssRead ctss
  return ctss

-- | Watch the files the service is read from, so that later reads read only what has changed.
--
-- For a service read from over and over, rather than read once and thrown away. Without it a read
-- looks at every file the service holds, which is cheap next to reading them but is still work
-- proportional to the program rather than to the change.
ctssWatch :: CTSS -> IO ()
ctssWatch ctss@(CTSS ssmv) = do
  watcher <- startWatcher
  modifyMVar_ ssmv $ \ss -> return ss{ctssWatcher=Just watcher}
  -- The watches go on here rather than being left to the next read. A read only sees to them when
  -- it read something, and the read after this one gives every page back — so leaving it to a read
  -- would leave a service watching nothing, and answering forever with what it read once.
  ctssRewatch ctss watcher

-- | Read the files the service was given, and take up whatever they now say.
--
-- Only the pages whose files have been written to are read again; every other page is given back by
-- 'readFiles' as the last read left it. A read therefore costs what has changed rather than what the
-- program is, which is what lets a caller read as often as it likes instead of choosing between
-- working from what it read once and re-reading everything to find out what it now says.
--
-- The files are read outside the lock and merged into the graph inside it, against the graph as it
-- is by then rather than the one the read started from. Holding the lock across the read would put
-- every read behind a build, which holds the same lock for a whole SCC; merging into the graph the
-- read started from would throw away everything a build ('ctssBuildSCC') or a store ('ctssStore')
-- committed while the read was parsing.
ctssRead :: CTSS -> IO ()
ctssRead ctss@(CTSS ssmv) = do
  CTSSDat{ctssConfig, ctssBaseFileNames, ctssData=before, ctssWatcher, ctssReadLock} <- readMVar ssmv
  withMVar ctssReadLock $ \() -> withWritten ctssWatcher $ \written -> do
    unchanged <- ctssUnchanged ctssWatcher written [(prgmName, ssfRead ssf) | (ssf, prgmName, _) <- graphToNodes before]
    rawPrgms <- readFiles ctssConfig unchanged (map mkRawImportStr ctssBaseFileNames)
    modifyMVar_ ssmv $ \ss -> do
      let (data', invalidations) = unzip $ map (buildSSF (ctssData ss)) $ graphToNodes rawPrgms
      return $ ctssClearInvalidations (catMaybes invalidations) ss{ctssData=graphFromEdges data'}
    -- The watches only need seeing to where the read read something, since they are watches on what
    -- the reads read. A read which gave every page back left every path it was watching for exactly
    -- as it was, and gathering them up again to say so would be work for every path of every page,
    -- to find nothing to do.
    let read' = graphToNodes rawPrgms
    let readNothing = length read' == H.size unchanged && all (\(_, prgmName, _) -> H.member prgmName unchanged) read'
    unless readNothing $ forM_ ctssWatcher $ ctssRewatch ctss
  where
    buildSSF held (page, prgmName, imports) = case graphLookup prgmName held of
      -- A page given back unread is the page already held, so there is nothing to compare it
      -- against and nothing to invalidate. The comparison below walks the whole syntax tree, so a
      -- read which read nothing would otherwise compare every page against itself.
      --
      -- Read times identify the read rather than the files: a page read again is timed afresh
      -- before it is parsed, so times equal to the ones the page already holds are the very times
      -- it was handed back with.
      Just old | isJust (rpReads page), rpReads (ssfRead old) == rpReads page -> ((old, prgmName, imports), Nothing)
      -- A page which read back the same program keeps everything built from it, but takes the new
      -- source text: a change the parse discards (trailing whitespace) leaves the program equal
      -- while the file on disk is no longer the one the page last served. It takes the new read
      -- too, or a page written to without changing would be read again on every read forever.
      Just old@SSF{ssfRaw=CRes _ oldRaw} | oldRaw == rpPrgm page -> ((old{ssfSource=rpSource page, ssfRead=page}, prgmName, imports), Nothing)
      Just{} -> ((newSSF page, prgmName, imports), Just prgmName)
      Nothing -> ((newSSF page, prgmName, imports), Nothing)

-- | Act on the paths a watcher saw written, where there is one to have seen any.
withWritten :: Maybe Watcher -> (S.HashSet FilePath -> IO a) -> IO a
withWritten Nothing act        = act S.empty
withWritten (Just watcher) act = withPending watcher act

-- | The pages a read need not read again: those nothing they were read from has been written to.
--
-- A watched page is answered from what the watcher saw written, which costs nothing to ask. Every
-- other page — one no watcher covers, or one whose read could not say what it read — is looked at
-- instead, so that a page is given back only when something can say it is unchanged.
ctssUnchanged :: Maybe Watcher -> S.HashSet FilePath -> [(FileImport, ReadPage)] -> IO (H.HashMap FileImport ReadPage)
ctssUnchanged Nothing _ pages = unchangedPages pages
ctssUnchanged (Just watcher) written pages = do
  everyPath <- allCovered watcher
  if S.null written && everyPath then return (H.fromList pages) else ctssUnchangedOf watcher written pages

-- | 'ctssUnchanged' where something has been written to, or some path is one the watcher cannot
-- answer for.
--
-- Split out because it asks its question of each path of each page, and a page may be read from as
-- many paths as the program has pages — a page read from a directory listing is read from it
-- however many other pages that listing holds. Asking that of every page every time would be the
-- program squared, to find that nothing had changed; nothing having changed is answered whole
-- instead, and this is reached only when something has.
ctssUnchangedOf :: Watcher -> S.HashSet FilePath -> [(FileImport, ReadPage)] -> IO (H.HashMap FileImport ReadPage)
ctssUnchangedOf watcher written pages = do
  watched <- covered watcher
  let isWatched page = maybe False (all ((`S.member` watched) . readPathWatch . fst) . rtPaths) (rpReads page)
  let (byWatch, byLook) = partition (isWatched . snd) pages
  looked <- unchangedPages byLook
  return $ H.union looked $ H.fromList $ filter (not . any (readPathWrittenIn written) . readPaths . snd) byWatch

-- | The watch it takes to notice a change to what a read read.
readPathWatch :: ReadPath -> (FilePath, Bool)
readPathWatch (ReadAt file)   = (file, False)
readPathWatch (ReadUnder dir) = (dir, True)

-- | Watch the files the pages were read from, and look at whichever of them were read before there
-- was a watch on them.
--
-- The pages read before their files were watched are the gap the watcher cannot close by itself: a
-- write between the reading of a file and the watching of it is one no event will ever report.
-- Every page is in that gap when a service first starts watching, and a page found in a directory
-- nothing had been read from before is in it afterwards, so both are answered the same way — by
-- looking, once, at the pages whose files have only now come to be watched.
ctssRewatch :: CTSS -> Watcher -> IO ()
ctssRewatch (CTSS ssmv) watcher = do
  CTSSDat{ctssData} <- readMVar ssmv
  before <- covered watcher
  watchReads watcher $ map readPathWatch $ concatMap (readPaths . ssfRead . fst3) $ graphToNodes ctssData
  after <- covered watcher
  let fresh = S.difference after before
  unless (S.null fresh) $ do
    let freshly = [(prgmName, ssfRead ssf) | (ssf, prgmName, _) <- graphToNodes ctssData
                                           , any ((`S.member` fresh) . readPathWatch) $ readPaths $ ssfRead ssf]
    current <- unchangedPages freshly
    forM_ freshly $ \(prgmName, _) -> unless (H.member prgmName current) $ ctssStale (CTSS ssmv) prgmName

ctssClearInvalidations :: [FileImport] -> CTSSDat -> CTSSDat
ctssClearInvalidations invalidations ss@CTSSDat{ctssData=(g, nodeFromVertex, vertexFromKey)} = ss{ctssData=fmapGraphWithKey maybeClear $ ctssData ss}
  where
    propagated = map (snd3 . nodeFromVertex) $ foldMap (foldMap (:[])) $ dfs (transposeG g) (mapMaybe vertexFromKey invalidations)
    maybeClear prgmName ssf
      -- Rebuilt from the read the page already has: an invalidation drops what was built from a
      -- page, not what the page was read from, so the next read still gives it back unread.
      | prgmName `elem` propagated = newSSF (ssfRead ssf)
      | null invalidations = ssf
      -- Which tests a page has, and what they find, is decided over the whole program the page
      -- belongs to rather than over the page and its imports (see 'ctssTestsFrom'). Invalidation
      -- walks the pages that DEPEND on the changed one, so it never reaches a page the change is
      -- downstream of — whose tests the change can still alter. Every page's tests are therefore
      -- dropped by any change at all, and re-run for whichever page is asked about next.
      | otherwise = ssf{ssfTests = Nothing}

-- | Build pages incrementally, one SCC at a time.
-- Each successfully built SCC is committed to the MVar independently,
-- so earlier SCCs (e.g. core library) remain cached even if a later SCC fails.
ctssBuildPagesExact :: CTSS -> GraphData SSF FileImport -> IO ()
ctssBuildPagesExact _ pages | all (ssfBuilt . fst3) (graphToNodes pages) = return ()
ctssBuildPagesExact (CTSS ssmv) pages = do
  -- stronglyConnCompR returns SCCs in reverse topological order (dependencies first)
  let sccs = stronglyConnCompR $ graphToNodes pages
  forM_ sccs $ \comp -> do
    let sccNodes = flattenSCC comp
    unless (all (ssfBuilt . fst3) sccNodes) $
      ctssBuildSCC ssmv sccNodes pages

-- | Build a single SCC and commit the result to the MVar atomically.
ctssBuildSCC :: MVar CTSSDat -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport -> IO ()
ctssBuildSCC ssmv sccNodes pages = do
  let sccFileNames = map snd3 sccNodes
  let sccNamesSet = S.fromList sccFileNames
  -- Get dep lists from the pages graph structure (stable across MVar updates)
  let pageDeps = H.fromList [(name, deps) | (_, name, deps) <- graphToNodes pages]
  modifyMVar_ ssmv $ \ctssdat@CTSSDat{ctssData} -> do
    -- Re-check with current state (another thread may have built this SCC)
    let currentSSFs = mapMaybe (`graphLookup` ctssData) sccFileNames
    if all ssfBuilt currentSSFs
      then return ctssdat
      else do
        -- Look up current SSFs and deps for SCC members
        let currentSCCNodes = mapMaybe (\name -> do
              ssf <- graphLookup name ctssData
              let deps = fromMaybe [] $ H.lookup name pageDeps
              return (ssf, name, deps)) sccFileNames
        p <- runCResT $ buildSCCPipeline ctssData sccNamesSet currentSCCNodes pages
        case p of
          CRes notes updates -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            let updatedData = fmapGraphWithKey (\k ssf -> fromMaybe ssf (H.lookup k updates)) ctssData
            return ctssdat{ctssData = updatedData}
          CErr notes -> do
            unless (null notes) $ putStrLn $ prettyCNotes notes
            fail "Could not build catln service"

-- | Run the full pipeline (desugar → typecheck → build → annotate) for a single SCC.
-- Returns a map of updated SSFs for the SCC members.
buildSCCPipeline :: GraphData SSF FileImport -> S.HashSet FileImport
                 -> [(SSF, FileImport, [FileImport])] -> GraphData SSF FileImport
                 -> CResT IO (H.HashMap FileImport SSF)
buildSCCPipeline ctssData sccNamesSet sccNodes pages = do
  -- Get raw programs for all SCC members
  rawPrgms <- forM sccNodes $ \(ssf, name, deps) -> do
    raw <- asCResT $ ssfRaw ssf
    return (raw, ssf, name, deps)

  -- Separate valid programs from ctx-annotated ones (which skip the pipeline)
  let (validRaws, invalidRaws) = partition (\(raw, _, _, _) -> validPrgm raw) rawPrgms
  let invalidUpdates = [(name, ssf{ssfBuilt=True}) | (_, ssf, name, _) <- invalidRaws]

  if null validRaws
    then return $ H.fromList invalidUpdates
    else do
      let namesAndDeps = [(name, deps) | (_, _, name, deps) <- validRaws]

      -- Step 1: Desugar each raw program individually
      desResults <- forM validRaws $ \(raw, _, name, deps) ->
        asCResT $ desPrgm (raw, name, deps)

      -- Step 2: Collect data from already-built transitive dependencies
      -- All non-SCC nodes in `pages` are potential external deps
      let extDepNames = filter (not . (`S.member` sccNamesSet)) $
            map snd3 $ graphToNodes pages
      let extDepDes = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfDes >>= cresToMaybe
            ) extDepNames

      -- Step 3: Run final desugar passes with SCC peers and external deps
      let desNodesForFinal = [(des, name, peerDes, extDepDes)
            | (des, name, _) <- desResults
            , let peerDes = [d | (d, n, _) <- desResults, n /= name]]
      finalDes <- desFinalPasses desNodesForFinal

      -- Step 4: Collect typechecked data from already-built dependencies
      let extDepTC = mapMaybe (\name ->
            graphLookup name ctssData >>= ssfTPrgmWithTrace >>= cresToMaybe
            ) extDepNames

      -- Step 5: Typecheck with SCC peers and external deps
      let tcNodesForTC = [(des, peerDes, extDepTC)
            | (des, (name, _)) <- zip finalDes namesAndDeps
            , let peerDes = [d | (d, (n, _)) <- zip finalDes namesAndDeps, n /= name]]
      let tcCRes = typeCheckToRes $ typecheckPrgms tcNodesForTC
      let tcNotes = getCNotes tcCRes
      tcResults <- asCResT tcCRes

      -- Step 6: Construct TPrgm graph for eval (SCC results + built dep TPrgms)
      let sccTPrgmNodes = [( fst3 tc, name, deps)
            | (tc, (name, deps)) <- zip tcResults namesAndDeps]
      let depTPrgmNodes = mapMaybe (\(_, name, deps) ->
            if name `S.member` sccNamesSet then Nothing
            else do
              ssf <- graphLookup name ctssData
              ssfTPrgm ssf >>= cresToMaybe >>= \t -> Just (t, name, deps)
            ) (graphToNodes pages)
      let tprgmGraph = graphFromEdges (sccTPrgmNodes ++ depTPrgmNodes)

      -- Step 7: Eval build all (processes SCC + deps, we extract SCC results).
      -- Tolerate failure: a type-erroneous program cannot build a tree, but we
      -- still want to cache the desugar/typecheck artifacts (including the trace)
      -- for tooling. Any eval-build error is deferred into ssfTBPrgm below.
      let tbprgmGraphRes = evalBuildAll tprgmGraph

      -- Step 8: Assemble updated SSFs
      let validUpdates = [(name, ssf{
              ssfBuilt = True,
              ssfDes = Just $ pure des,
              -- Carries the notes but is deliberately not failOnErrorNotes: the whole point of
              -- keeping the trace is to explain a program that failed to typecheck, and the notes
              -- are what say why, so a type inference view needs both together.
              ssfTPrgmWithTrace = Just $ CRes tcNotes tc,
              -- failOnErrorNotes so eval consumers (run/test/build) cleanly reject
              -- a type-erroneous program, while ssfTPrgmWithTrace still carries the trace.
              ssfTPrgm = Just $ failOnErrorNotes $ CRes tcNotes (fst3 tc),
              ssfTBPrgm = case tbprgmGraphRes of
                CRes _ g  -> fmap pure $ graphLookup name g
                CErr errs -> Just $ CErr errs
            })
            | ((_, ssf, name, _), des, tc) <-
                zip3 validRaws finalDes tcResults]

      return $ H.fromList (validUpdates ++ invalidUpdates)

ctssBuildFrom :: CTSS -> FileImport -> IO ()
ctssBuildFrom ctss@(CTSS ssmv) src = do
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss (graphFilterReaches src ctssData)

ctssBuildAll :: CTSS -> IO ()
ctssBuildAll ctss@(CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  ctssBuildPagesExact ctss ctssData

-- | Bring the service into line with the files on disk: read whatever has changed, and build
-- whatever that left unbuilt.
--
-- What to ask for before working from the service, where the files may have been written to since
-- it was last read, so that what it holds is what the files now say.
ctssRefresh :: CTSS -> IO ()
ctssRefresh ctss = do
  ctssRead ctss
  ctssBuildAll ctss

ctssKeys :: CTSS -> IO [FileImport]
ctssKeys (CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ map snd3 $ graphToNodes ctssData

-- | The page a service opens at: the first file it was started with. Which page that is, is the
-- service's own knowledge, and a reader of it would otherwise have to guess it out of a list of
-- every page. Resolved the same way a page asked for by name is, so it is a page that can be asked
-- for; a service started with no files at all has none.
ctssRootPage :: CTSS -> IO (Maybe String)
ctssRootPage (CTSS ssmv) = do
  CTSSDat{ctssConfig, ctssBaseFileNames} <- readMVar ssmv
  case ctssBaseFileNames of
    []       -> return Nothing
    (name:_) -> impDisp <$> mkDesCanonicalImportStr ctssConfig name

ctssGet :: (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO (GraphData a FileImport)
ctssGet f (CTSS ssmv) = do
  lift $ ctssBuildAll (CTSS ssmv)
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  let computed = graphFromEdges $ filter (isJust . f . fst3) $ graphToNodes ctssData
  asCResT $ mapMGraph (requireComputed . f) computed

ctssGetFrom :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (GraphData a FileImport)
ctssGetFrom f prgmName (CTSS ssmv) = do
  lift $ ctssBuildFrom (CTSS ssmv) prgmName
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph (requireComputed . f) (graphFilterReaches prgmName ctssData)

requireComputed :: Maybe (CRes r) -> CRes r
requireComputed = fromMaybe (CErr [MkCNote $ GenCErr Nothing "Value not computed in catln service"])

ctssGetJoined :: (Monoid a) => (SSF -> Maybe (CRes a)) -> CTSS -> CResT IO a
ctssGetJoined f ctss = do
  graph <- ctssGet f ctss
  return $ mconcat $ map fst3 $ graphToNodes graph

ctssLookup :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO (Maybe a)
ctssLookup f k ctss = do
  graph <- ctssGetFrom f k ctss
  return $ graphLookup k graph

ctssLookupReq :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> CResT IO a
ctssLookupReq f k ctss = do
  mv <- ctssLookup f k ctss
  case mv of
    Just v  -> return v
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw k)

-- This does not use ctssGet because it doesn't require building
getSource :: CTSS -> FileImport -> CResT IO String
getSource (CTSS ssmv) prgmName = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Just SSF{ssfSource=Just src} -> return src
    Just SSF{ssfSource=Nothing}  -> fail $ printf "No source text available for %s" (show prgmName)
    Nothing                      -> fail $ printf "Could not find %s" (show $ impRaw prgmName)

-- | The parsed program of one page. Like 'getSource' this does not build, as the parse is what
-- reading the page already produced.
getRawPrgmOf :: CTSS -> FileImport -> CResT IO PPrgm
getRawPrgmOf (CTSS ssmv) prgmName = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Just SSF{ssfRaw} -> asCResT ssfRaw
    Nothing          -> fail $ printf "Could not find %s" (show $ impRaw prgmName)

-- | Overwrite a page with a program, written back as the syntax the page is written in.
--
-- The reversion belongs to the parser which read the page, so a page whose syntax cannot be written
-- back fails here rather than being written as something else. What it produces is the page's new
-- source, so the write itself is 'setSource' and everything that guards a write guards this one too.
setRawPrgm :: CTSS -> FileImport -> PPrgm -> CResT IO ()
setRawPrgm ctss prgmName prgm = do
  src <- asCResT $ unparseImport prgmName prgm
  setSource ctss prgmName src

-- | Overwrite the source file backing a page, then re-read so the change is picked up.
-- Only pages already in the service graph with source text can be written, which confines
-- writes to the files served by the doc roots.
setSource :: CTSS -> FileImport -> String -> CResT IO ()
setSource ctss@(CTSS ssmv) prgmName src = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  case graphLookup prgmName ctssData of
    Nothing -> fail $ printf "Could not find %s" (show $ impRaw prgmName)
    Just SSF{ssfSource=Nothing} -> fail $ printf "No source text available for %s" (show prgmName)
    Just SSF{ssfSource=Just _} -> case impDisp prgmName of
      Nothing -> fail $ printf "No file path available for %s" (show prgmName)
      Just filePath -> lift $ do
        writeFile filePath src
        -- The page is read again because it was written, not because the file looks written. A
        -- filesystem which keeps modification times to the second has nothing to show for a page
        -- written and read within one, and the read would give back the page it already holds.
        ctssStale ctss prgmName
        ctssRead ctss

-- This does not use ctssGet because it doesn't require building
getRawPrgm :: CTSS -> CResT IO PPrgmGraphData
getRawPrgm (CTSS ssmv) = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph ssfRaw ctssData

-- | Like getRawPrgm but only returns the subgraph reachable from a given file.
getRawPrgmFrom :: FileImport -> CTSS -> CResT IO PPrgmGraphData
getRawPrgmFrom prgmName (CTSS ssmv) = do
  CTSSDat{ctssData} <- lift $ readMVar ssmv
  asCResT $ mapMGraph ssfRaw (graphFilterReaches prgmName ctssData)

getEvaluated :: CTSS -> FileImport -> String -> CResT IO Integer
getEvaluated ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalRun fun prgmName base

getEvaluatedMock :: CTSS -> MockIO -> FileImport -> String -> CResT IO Integer
getEvaluatedMock ctss mockIO prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalRunMock mockIO fun prgmName base

getEvalBuild :: CTSS -> FileImport -> String -> CResT IO Val
getEvalBuild ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  fst <$> evalBuild fun prgmName base

getTestResults :: CTSS -> CResT IO [TestDesc]
getTestResults ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalTest base

-- | The tests a page wrote, run and kept.
--
-- Which tests exist at all is decided over the whole PROGRAM the page belongs to, NOT over the page
-- and what it imports. A class law is checked against the instances of that class, and those live in
-- the files that define them — which are downstream of the file stating the law. @/Data/Algebra@
-- states that @++@ is associative and imports nothing that is a @Semigroup@; deriving its tests from
-- what it can reach would leave the law quantifying over nothing and passing vacuously. Likewise an
-- agreement test pairs two definitions of one function, which two unrelated files can each hold half
-- of. See 'ctssTestPages' for what "the program the page belongs to" means.
--
-- Each derived test is then attributed to the page that wrote the definition it came from, and only
-- that page's are run here. So the answer is the same set of tests @catln test@ runs on that program,
-- sliced by who wrote them, and asking about one page costs that page's tests rather than every
-- page's.
ctssTestsFrom :: CTSS -> FileImport -> CResT IO [TestResult]
ctssTestsFrom ctss prgmName = do
  cached <- lift $ ctssCached ssfTests prgmName ctss
  case cached of
    Just tests -> asCResT tests
    Nothing -> do
      tprgms <- ctssTestPages ctss prgmName
      descs <- asCResT $ evalTest tprgms
      let owners = pageOwners tprgms
      -- Every test comes from a definition and every definition was written in a page, so a test
      -- that cannot be placed is reported rather than dropped — dropped, it would be a test that
      -- belongs to no page and so is run by nobody.
      case [tdName d | d <- descs, not (H.member (tdOwnerId d) owners)] of
        []      -> return ()
        orphans -> fail $ printf "Could not find the page that wrote %s" (intercalate ", " orphans)
      results <- lift $ mapM runTestDesc [d | d <- descs, H.lookup (tdOwnerId d) owners == Just prgmName]
      lift $ ctssStore prgmName (\ssf -> ssf{ssfTests = Just $ pure results}) ctss
      return results

-- | The pages a page's tests are decided over: every page reached by a file the service was asked to
-- serve that reaches this page.
--
-- Not every loaded page. A service can be asked to serve several files that are separate programs —
-- the integration tests load thirty of them — and a page belongs to the ones that reach it, not to
-- the ones beside it. Joining programs that were never one is also what the compiler cannot yet do:
-- @buildSCCPipeline@ offers every loaded page as an external dependency of every other, so an
-- unrelated file's @abs@ collides with a test file's own.
--
-- Not just what the page imports either, which is the point of 'ctssTestsFrom' above. Asked to serve
-- the standard library, the file it is asked for reaches every page of it, so a law stated in one
-- file is checked against the instances defined in another.
ctssTestPages :: CTSS -> FileImport -> CResT IO (GraphData TPrgm FileImport)
ctssTestPages ctss@(CTSS ssmv) prgmName = do
  CTSSDat{ctssConfig, ctssBaseFileNames, ctssData} <- lift $ readMVar ssmv
  roots <- lift $ mapM (mkDesCanonicalImportStr ctssConfig) ctssBaseFileNames
  let reaching = [root | root <- roots, isJust $ graphLookup prgmName $ graphFilterReaches root ctssData]
  when (null reaching) $
    fail $ printf "No file this service was asked to serve reaches %s, so it is in no program to take the tests of" (show $ impRaw prgmName)
  mapM_ (lift . ctssBuildFrom ctss) reaching
  CTSSDat{ctssData=built} <- lift $ readMVar ssmv
  let scope = S.fromList $ concat [map snd3 $ graphToNodes $ graphFilterReaches root built | root <- reaching]
  asCResT $ mapMGraph (requireComputed . ssfTPrgm) $ graphFromEdges
    [node | node@(_, name, _) <- graphToNodes built, name `S.member` scope]

-- | Which page each definition was written in, keyed the way a test names the definition it was
-- derived from. A page's own program holds what that page wrote; the joined program the tests are
-- derived over has lost which file anything came from.
pageOwners :: GraphData TPrgm FileImport -> H.HashMap UUID FileImport
pageOwners tprgms = H.fromList
  [ (i, name)
  | (Prgm objMap _ globalAnnots, name, _) <- graphToNodes tprgms
  , i <- map oaTestId (flatObjectMap objMap) ++ map (getMetaID . getExprMeta) globalAnnots
  ]

-- | The freshly-evaluated golden values for every string-serializable @#print@ reachable from the
-- loaded program, each keyed by the source range of its @p@ argument. Used by @catln test --update@.
getPrintGoldens :: CTSS -> CResT IO [(CodeRangeDat, String)]
getPrintGoldens ctss = do
  base <- ctssGet ssfTPrgm ctss
  asCResT $ evalPrints $ mconcat $ map fst3 $ graphToNodes base

-- | The annotations a page writes, evaluated to the values they display as.
--
-- They are evaluated when something asks to see them rather than when the page is built, so a build
-- that displays nothing — a run, a compile, an editor session — does not pay for them. The result is
-- kept on the page, so the second reader of a page gets the first reader's answer.
getEvalAnnots :: CTSS -> FileImport -> CResT IO [(Expr EvalMetaDat, Val)]
getEvalAnnots ctss prgmName = do
  cached <- lift $ ctssCached ssfAnnots prgmName ctss
  case cached of
    Just annots -> asCResT annots
    Nothing -> do
      tprgms <- ctssGetFrom ssfTPrgm prgmName ctss
      stages <- lift $ ctssStagePrgmsFrom ctss prgmName
      -- A @#test@ displays the run of the test it marks, which is the one thing an evaluation
      -- cannot work out for itself. The page's tests are run only when the page writes one, since
      -- running them is work rather than a lookup and no other annotation reads them.
      stages' <- if pageWritesATest prgmName tprgms
        then (\tests -> stages{spTests = pure tests}) <$> ctssTestsFrom ctss prgmName
        else return stages
      let annots = evalAnnots stages' prgmName tprgms
      lift $ ctssStore prgmName (\ssf -> ssf{ssfAnnots = Just annots}) ctss
      asCResT annots

-- | Whether a page marks any of its definitions as a test. A page that does not has no annotation
-- that reads a test run, so it is not made to wait for one.
pageWritesATest :: FileImport -> GraphData TPrgm FileImport -> Bool
pageWritesATest prgmName tprgms = case graphLookup prgmName tprgms of
  Just (Prgm objMap _ _) -> any (hasAnnot testAnnot) (flatObjectMap objMap)
  -- A page that is not in its own program graph has no annotations either; 'evalAnnots' is what
  -- reports that, rather than it being decided here.
  Nothing                -> False

-- | Read a page's computed item, without building the page or computing anything.
ctssCached :: (SSF -> Maybe (CRes a)) -> FileImport -> CTSS -> IO (Maybe (CRes a))
ctssCached f prgmName (CTSS ssmv) = do
  CTSSDat{ctssData} <- readMVar ssmv
  return $ f =<< graphLookup prgmName ctssData

-- | Keep a page's computed item on the page, for whoever asks for it next.
--
-- The page is read again under the lock rather than being written back from a copy captured before
-- the computation: a page that was invalidated while its item was being computed is unbuilt again,
-- and restoring it from a stale copy of itself would undo the invalidation. Two threads racing to
-- compute the same item both store it, which is harmless — they computed it from the same built
-- page, so they computed the same thing.
--
-- Whatever computes the item must do so OUTSIDE this lock. It is the same lock a build takes, and
-- computing an item needs the page built.
ctssStore :: FileImport -> (SSF -> SSF) -> CTSS -> IO ()
ctssStore prgmName store (CTSS ssmv) = modifyMVar_ ssmv $ \ctssdat ->
  return ctssdat{ctssData = fmapGraphWithKey keep (ctssData ctssdat)}
  where
    keep k ssf | k == prgmName && ssfBuilt ssf = store ssf
               | otherwise = ssf

-- | Forget when a page's files were last written, so the next read reads it again.
--
-- What a page was read from is kept, so the read still knows which files to look at; only the claim
-- that they are still as they were is dropped.
ctssStale :: CTSS -> FileImport -> IO ()
ctssStale (CTSS ssmv) prgmName = modifyMVar_ ssmv $ \ctssdat ->
  return ctssdat{ctssData = fmapGraphWithKey forget (ctssData ctssdat)}
  where
    forget k ssf | k == prgmName = ssf{ssfRead = (ssfRead ssf){rpReads = Nothing}}
                 | otherwise = ssf

getWeb :: CTSS -> FileImport -> String -> CResT IO String
getWeb ctss prgmName fun = do
  base <- ctssGetFrom ssfTPrgm prgmName ctss
  (TupleVal _ args, _) <- evalBuild fun prgmName base
  case H.lookup "contents" args of
    Just (StrVal s) -> return s
    _               -> return "";

-- Helpers

getPrgm :: CTSS -> CResT IO (GraphData DesPrgm FileImport)
getPrgm = ctssGet ssfDes

getTPrgmWithTrace :: CTSS -> CResT IO (GraphData (TPrgm, VPrgm, TraceConstrain) FileImport)
getTPrgmWithTrace = ctssGet ssfTPrgmWithTrace

getTPrgm :: CTSS -> CResT IO (GraphData TPrgm FileImport)
getTPrgm = ctssGet ssfTPrgm

getTPrgmJoined :: CTSS -> CResT IO TPrgm
getTPrgmJoined = ctssGetJoined ssfTPrgm

getTBPrgm :: CTSS -> CResT IO (GraphData TBPrgm FileImport)
getTBPrgm = ctssGet ssfTBPrgm

-- | The desugar and tree build programs of every loaded page, joined and left unforced. This is the
-- 'StagePrgms' for an evaluation that is not scoped to one page, and so reaches all of them.
--
-- It cannot go through 'ctssGetJoined': that runs in @CResT IO@ and so forces its result, and
-- merely inspecting 'ssfTBPrgm' forces 'evalBuildAll' over every page. Keeping the join a thunk is
-- what lets a caller that never reads a stage avoid building it.
--
-- Neither the type inference nor a debug run is available here. Points are numbered per typecheck
-- run, so the traces of every page joined together would be a set of colliding numbers rather than an
-- answer, and a run needs one program with one entry point where the join has as many as it has
-- pages. An evaluation that wants either has to name the page it is asking about
-- ('ctssStagePrgmsFrom').
ctssStagePrgms :: CTSS -> IO StagePrgms
ctssStagePrgms ctss@(CTSS ssmv) = do
  ctssBuildAll ctss
  CTSSDat{ctssData} <- readMVar ssmv
  let ssfs = map fst3 $ graphToNodes ctssData
  return $ StagePrgms (joinedStage ssfDes ssfs) (joinedStage ssfTBPrgm ssfs) (notScoped "type inference to report on") (const $ notScoped "program to run") (return $ map snd3 $ graphToNodes ctssData) (notScoped "tests to report on") (pageValues ctssData)
  where
    notScoped :: String -> CRes a
    notScoped what = CErr [MkCNote $ GenCErr Nothing $ "This evaluation is not scoped to a single page, so it has no " ++ what]

-- | Like 'ctssStagePrgms' but for an evaluation scoped to one page: every stage covers that page and
-- what it imports, the type inference is that page's own, which is the only scope its points are
-- numbered in, and a debug run runs that page.
ctssStagePrgmsFrom :: CTSS -> FileImport -> IO StagePrgms
ctssStagePrgmsFrom ctss@(CTSS ssmv) prgmName = do
  ctssBuildFrom ctss prgmName
  CTSSDat{ctssData} <- readMVar ssmv
  let reaches = graphFilterReaches prgmName ctssData
  let ssfs = map fst3 $ graphToNodes reaches
  let typeInferred = do
        (_, vprgm, trace) <- requireComputed $ ssfTPrgmWithTrace =<< graphLookup prgmName ctssData
        return (vprgm, trace)
  -- Runs against the page's own typecheck program rather than the evaluation's, which for an
  -- annotation is that program joined with the synthetic one holding the annotation itself.
  let debugRun fun = evalDebugPrgm fun =<< joinedStage ssfTPrgm ssfs
  -- No test run: unlike every stage here, one cannot be looked up — it has to be produced by
  -- running the page's tests, which is the work an evaluation with nothing to report on them must
  -- not be made to do. An evaluation that does have a test to report on is given one by whoever
  -- asked for it (see 'getEvalAnnots').
  return $ StagePrgms (joinedStage ssfDes ssfs) (joinedStage ssfTBPrgm ssfs) typeInferred debugRun (return $ map snd3 $ graphToNodes reaches) notRun (pageValues reaches)
  where
    notRun = CErr [MkCNote $ GenCErr Nothing "This evaluation was not given a run of the page's tests"]

-- | The values one page declares of one type, evaluated to the values they display as. This is the
-- 'StagePrgms.spPageValues' of a set of pages: what a value gathered across pages means, without any
-- notion of which type is worth gathering, which belongs to whatever is doing the gathering.
--
-- Ordered, which a program's declarations are not: they are held in a hash map, so one page's arrive
-- in no order at all, and a declaration a syntax builds rather than parses carries no source
-- position to be sorted by either (@Syntax.Ct.Builder@ builds it with an empty meta). They are put
-- back into the order the page writes them, so what gathers them reads as the pages do rather than
-- reshuffling itself between readings.
--
-- A page that was never built has nothing to report and is answered with nothing, as 'joinedStage'
-- passes over such a page in the stages beside this one; a page that failed to build fails what
-- asked. The graph is read once for whichever pages are asked about, since the answer is a field of
-- one 'StagePrgms' rather than a call made afresh each time.
pageValues :: GraphData SSF FileImport -> Name -> FileImport -> CRes [(Expr EvalMetaDat, Val)]
pageValues graph = valuesOf
  where
    computed = graphFromEdges $ filter (isJust . ssfTPrgm . fst3) $ graphToNodes graph
    tprgms = mapMGraph (requireComputed . ssfTPrgm) computed

    valuesOf name prgmName = case graphLookup prgmName computed of
      Nothing -> return []
      Just ssf -> do
        values <- evalPageValues name prgmName =<< tprgms
        written <- rawStatementExprIds <$> ssfRaw ssf
        return $ sortOn (writtenAt written . fst) values

    -- A value the page does not write is one the page was rebuilt without, so it goes after the ones
    -- it does rather than before them.
    writtenAt written e = fromMaybe (length written) $ elemIndex (exprId e) written

    exprId = getMetaID . getExprMeta

-- | Joins one stage over a set of pages, left unforced. See 'ctssStagePrgms' for why it must stay a
-- thunk.
joinedStage :: (Monoid a) => (SSF -> Maybe (CRes a)) -> [SSF] -> CRes a
joinedStage f ssfs = fmap mconcat $ sequence $ mapMaybe f ssfs
