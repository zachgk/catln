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

import           Data.Aeson                    (ToJSON, Value, object, (.=))
import           Web.Scotty

import qualified Data.HashMap.Strict           as H
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding       (decodeUtf8)
import           GHC.Generics                  (Generic)
import           Network.Wai.Middleware.Static

import           Control.Monad.Trans           (lift)
import           CRes
import           CtService
import           Data.Bifunctor                (Bifunctor (first))
import           Data.Graph
import           Data.Maybe                    (listToMaybe)
import           Eval                          (evalAllTargetModes, evalAnnots,
                                                prgmFromGraphData)
import           Eval.Runtime.WebDocs          (sidebarOrder)
import           MapMeta                       (interleaveMeta, interleavePrgm,
                                                parseMetaIDScope, zip3MetaFun)
import           Semantics                     (buildCtssConfig)
import           Semantics.Prgm
import           Syntax.Ct.Desugarf            (desFinalPasses, desPrgm)
import           Syntax.Ct.Desugarf.Expr       (desFileImport)
import           Syntax.Ct.MapRawMeta          (addRawPrgmMetaIDs,
                                                mapMetaRawPrgm)
import           Syntax.Ct.Parser.Expr         (pExpr)
import           Syntax.Ct.Prgm                (RawExpr (RawValue),
                                                RawPrgm (RawPrgm),
                                                RawStatement (RawAnnot),
                                                RawStatementTree (RawStatementTree),
                                                mkRawFileImport)
import           Syntax.Parsers                (mkDesCanonicalImportStr)
import           Text.Megaparsec               (errorBundlePretty, runParser)
import           Text.Printf
import           TypeCheck                     (typecheckPrgms)
import           TypeCheck.Common              (typeCheckToRes)
import           Utils

data ResSuccess a n = Success a [n]
  | ResFail [n]
  deriving (Generic, ToJSON)

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
  -- | The directory the page was imported relative to, which is what a relative link written in its
  -- documentation resolves against.
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
maybeJson (CRes notes r) = json $ Success r notes
maybeJson (CErr notes)   = json (ResFail notes :: ResSuccess () CNote)

-- | Both providers return the service handle alongside the per-request refresh action.
-- The handle is needed by writes, which must not pay for the live provider's rebuild.
mkLiveWDProvider :: [String] -> IO (CTSS, IO CTSS)
mkLiveWDProvider baseFileNames = do
  ss <- ctssBaseFiles buildCtssConfig baseFileNames
  ctssBuildAll ss
  return (ss, do
    ctssRead ss
    ctssBuildAll ss -- builds whichever pages the read found a change in, and their dependents
    return ss)

mkCacheWDProvider :: [String] -> IO (CTSS, IO CTSS)
mkCacheWDProvider baseFileNames = do
  ss <- ctssBaseFiles buildCtssConfig baseFileNames
  ctssBuildAll ss
  return (ss, return ss)

docApiBase :: Bool -> CTSS -> IO CTSS -> ScottyM ()
docApiBase readonly ctss getProvider = do

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

  get "/api/page" $ do
    provider <- liftAndCatchIO getProvider
    prgmName <- param "prgmName"
    resp <- liftAndCatchIO $ runCResT $ do
      prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
      rawPrgm' <- ctssLookupReq (Just . ssfRaw) prgmName' provider
      tprgms <- getTPrgm provider
      maybeTPrgmRes <- lift $ runCResT $ do
        case graphLookup prgmName' tprgms of
          Just tprgm -> do
            reachTprgms <- asCResT $ prgmFromGraphData prgmName' tprgms
            return (tprgm, reachTprgms)
          Nothing -> fail $ printf "Could not find typechecked program %s" (show prgmName)
      let maybeTPrgm = fmap fst maybeTPrgmRes
      let maybeTPrgmFull = fmap snd maybeTPrgmRes
      maybeAnnots <- lift $ runCResT $ getEvalAnnots provider prgmName'
      let tprgm' = maybe H.empty interleavePrgm (cresToMaybe maybeTPrgm)
      let targetModes = maybe H.empty evalAllTargetModes (cresToMaybe maybeTPrgmFull)
      let annots' = maybe H.empty (H.fromList . map (first (getMetaID . getExprMeta))) (cresToMaybe maybeAnnots)
      let prgm' = mapMetaRawPrgm (zip3MetaFun (interleaveMeta tprgm') (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
      maybeSource <- lift $ runCResT $ getSource provider prgmName'
      -- The pages either side of this one travel with it, so a page knows where it sits without the
      -- webdocs holding a list of every page to work it out from. A page the order cannot place is
      -- reported rather than served as though it were an end of the order: 'sidebarOrder' fails only
      -- when a page has no path to open it by or two pages share one, which is a doc server that
      -- cannot be navigated at all.
      pages <- lift $ ctssKeys provider
      order <- either fail return $ sidebarOrder pages
      let (prevPage, nextPage) = maybe (Nothing, Nothing) (`neighbours` order) (impDisp prgmName')
      return $ PageResp prgm' (cresToMaybe maybeSource) prevPage nextPage (impDir prgmName')
    maybeJson resp

  get "/api/annot" $ do
    provider <- liftAndCatchIO getProvider
    annot <- param "annot"
    -- An annotation is local to the program it is written in, so evaluating a free-standing one has
    -- to say which program it is written in. Naming a page evaluates the annotation as though it sat
    -- in that page: every stage reported about it, and the synthetic program's own dependencies,
    -- narrow to that page and what it imports. Without a name the annotation is evaluated against
    -- every loaded file, which is what a listing of the whole program wants.
    scope <- fmap Just (param "prgmName") `rescue` (\_ -> return Nothing)
    scope' <- liftAndCatchIO $ traverse (mkDesCanonicalImportStr buildCtssConfig) scope
    stages <- liftAndCatchIO $ maybe (ctssStagePrgms provider) (ctssStagePrgmsFrom provider) scope'
    resp <- liftAndCatchIO $ runCResT $ do
      rawPrgm <- case runParser pExpr "<annot>" annot of
        Left err     -> fail $ show $ errorBundlePretty err
        Right parsed -> return (RawPrgm [] [RawStatementTree (RawAnnot parsed) []])
      let annotName = desFileImport $ mkRawFileImport $ RawValue emptyMetaN "<annot>"
      let rawPrgm' = addRawPrgmMetaIDs (parseMetaIDScope annotName) rawPrgm
      (annotDesPrgm, annotPrgmName, _) <- asCResT $ desPrgm (rawPrgm', annotName, [])
      [annotDesPrgm'] <- desFinalPasses [(annotDesPrgm, annotPrgmName, [], [])]
      tprgmsTrace <- maybe (getTPrgmWithTrace provider) (\n -> ctssGetFrom ssfTPrgmWithTrace n provider) scope'
      tprgms <- maybe (getTPrgm provider) (\n -> ctssGetFrom ssfTPrgm n provider) scope'
      let annotDeps = map snd3 $ graphToNodes tprgms
      [(annotTprgm, _, _)] <- asCResT $ typeCheckToRes $ typecheckPrgms [(annotDesPrgm', [], map fst3 $ graphToNodes tprgmsTrace)]
      let joinedTprgms = graphFromEdges ((annotTprgm, annotPrgmName, annotDeps) : graphToNodes tprgms)
      let targetModes = evalAllTargetModes annotTprgm
      maybeAnnots <- asCResT $ evalAnnots stages annotPrgmName joinedTprgms
      let annots' = H.fromList $ map (first (getMetaID . getExprMeta)) maybeAnnots
      return $ mapMetaRawPrgm (zip3MetaFun (interleaveMeta $ interleavePrgm annotTprgm) (interleaveMeta targetModes) (interleaveMeta annots')) rawPrgm'
    maybeJson resp


docApi :: Bool -> Bool -> [String] -> IO ()
docApi cached readonly baseFileNames = do

  (ctss, provider) <- if cached
    then mkCacheWDProvider baseFileNames
    else mkLiveWDProvider baseFileNames

  scotty 31204 $ docApiBase readonly ctss provider

docServe :: Bool -> Bool -> [String] -> IO ()
docServe cached readonly baseFileNames = do

  let handleIndex p = case p of
        "" -> Just "index.html"
        _  -> Just p

  (ctss, provider) <- if cached
    then mkCacheWDProvider baseFileNames
    else mkLiveWDProvider baseFileNames

  scotty 8080 $ do

    docApiBase readonly ctss provider

    middleware $ staticPolicy (noDots >-> policy handleIndex >-> addBase "webdocs/dist")

    get (regex ".*") $ file "webdocs/dist/index.html"
