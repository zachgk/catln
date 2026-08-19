--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.WebDocs
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the primitives backing the webdocs display
-- annotations declared in @stack/core/webdoc.ct@. They let a page be
-- defined in Catln (as a @Show/*@ value carrying its own data) rather
-- than by the webdocs web application fetching from a dedicated API
-- route.
--------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Eval.Runtime.WebDocs where

import           CRes
import           CtConstants         (dirEntryFileNames, noteObsidianTaskPath,
                                      noteSimpleTaskPath)
import           Data.Aeson          (ToJSON)
import           Data.Aeson.Text     (encodeToLazyText)
import           Data.Char           (toLower)
import           Data.Graph
import qualified Data.HashMap.Strict as H
import           Data.List           (elemIndex, intercalate, isInfixOf,
                                      partition, sort, sortOn)
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import qualified Data.Text.Lazy      as TL
import qualified Data.UUID           as UUID
import           Eval.Common
import           GHC.Generics        (Generic)
import           Semantics.Prgm
import           Semantics.Types     (ArgName, ClassGraph (..),
                                      ClassSatInfo (..), PartialType (..),
                                      SatCache (..), Type, TypeEnv (..),
                                      TypeGraph (..), TypeName, fromPartialName,
                                      maybeGetClassSingleton, maybeGetSingleton,
                                      mkName, nameStr, pkName,
                                      relativeNameMatches)
import           System.IO.Unsafe    (unsafePerformIO)
import           Text.Printf
import           TypeCheck.Common    (TraceConstrain, VPrgm,
                                      filterTraceConstrain, flipTraceConstrain)
import           Utils

-- | The primitives backing the @Catln/Doc@ display annotations.
webDocsPrims :: [EPrim]
webDocsPrims = [listProgram, filterProgram, typeInfer, typeInferPnt, debugger, debugExpr, sidebar, searchPagesFor, docPagesFor, testRun, taskList]

-- | The argument of @/Catln/Doc/listProgram@ naming which stage to list, and the members of the
-- @ListProgramMode@ enum it takes. @stack\/core\/webdoc.ct@ is the source of truth for these; a
-- name that drifts from it is reported rather than defaulted, so the drift is visible instead of
-- being served as the wrong stage.
listProgramModeArg :: String
listProgramModeArg = "/mode"

desugarMode, typecheckMode, treeBuildMode :: String
desugarMode = "/Catln/Doc/Desugar"
typecheckMode = "/Catln/Doc/Typecheck"
treeBuildMode = "/Catln/Doc/TreeBuild"

-- | Backs @/Catln/Doc/listProgram@, the data carried by a @Show/ListProgram@ page. It renders the
-- program being evaluated as JSON for the webdocs listing to display, at the pipeline stage the
-- annotation's @mode@ names. The program comes from the evaluation 'Env', so the listing is scoped
-- to whatever program the annotation was evaluated against — the page holding the @#listProgram@
-- annotation and everything it reaches — at every stage alike.
--
-- The typecheck stage is the program the evaluation itself runs on, so it is read straight off the
-- 'TBEnv' and costs nothing. The other two are stages of the program the evaluation cannot reach,
-- and are asked of the service page by page — which is why the mode is read first: a listing of the
-- stage already in hand does not build anything to answer.
listProgram :: EPrim
listProgram = EEnvPrim "listProgram" prim
  where
    prim env args = case H.lookup listProgramModeArg args of
      Just (TupleVal mode _)
        | mode == desugarMode -> fromStage env ssfDes
        | mode == typecheckMode -> Right $ encodeJson $ tbPrgm $ evTbEnv env
        | mode == treeBuildMode -> fromStage env ssfTBPrgm
      Just v -> Left $ printf "listProgram was given %s, which is not a ListProgramMode" (show v)
      Nothing -> Left $ printf "listProgram has no %s argument, only %s" listProgramModeArg (show $ sort $ H.keys args)

-- | One stage of the program a display covers, joined over the pages of its scope.
--
-- The scope is the evaluation's own: the page the annotation is written in and everything it
-- imports, or every page there is where it was written in none. That is what makes a listing of a
-- page list that page and what it reaches at every stage alike.
fromStage :: (Monoid a, ToJSON a) => Env -> (SSF -> Maybe (CRes a)) -> Either String Val
fromStage env stage = case evService env of
  Nothing -> Left noService
  Just (EvalService s scope) -> askService $ do
    graph <- csBuilt s scope
    let pages = maybe graph (`graphFilterReaches` graph) scope
    return $ encodeJsonText <$> orNotes (joinedStage stage $ map fst3 $ graphToNodes pages)

-- | The argument of @/Catln/Doc/filterProgram@ naming which type or class to keep.
filterProgramNameArg :: String
filterProgramNameArg = "/name"

-- | Backs @/Catln/Doc/filterProgram@, the data carried by a @Show/TypePage@ page. It is
-- 'listProgram' narrowed to one name: the same program, filtered to the declarations and definitions
-- of a single type or class, rendered as JSON.
--
-- Only the typecheck stage is offered, because that is the stage the evaluation itself runs on and so
-- the one readable straight off the 'TBEnv'. A type page has no stage selector to need the others.
filterProgram :: EPrim
filterProgram = EEnvPrim "filterProgram" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}} args = case H.lookup filterProgramNameArg args of
      Just (StrVal name) -> Right $ encodeJson $ filterByType name tbPrgm
      Just v -> Left $ printf "filterProgram was given %s, which is not a String type name" (show v)
      Nothing -> Left $ printf "filterProgram has no %s argument, only %s" filterProgramNameArg (show $ sort $ H.keys args)

-- | Filters a program's object map and class map to only the declarations and definitions of a
-- particular type or class name. It is polymorphic in the program because it reads only names, so it
-- applies at whichever pipeline stage the caller holds.
filterByType :: (ExprClass e, MetaDat m, Show m, Show (e m)) => String -> Prgm e m -> Prgm e m
filterByType name (Prgm objMap (ClassGraph classGraph) _) = Prgm objMap' classGraph' []
  where
    objMap' = filterObjectMap (relativeNameMatches (mkName name) . oaObjPath) objMap
    classGraph' = ClassGraph $ graphFromEdges $ filter (\(_, n, subTypes) -> relativeNameMatches (mkName name) (fromPartialName n) || n `elem` subTypes) $ graphToNodes classGraph

-- | The argument of @/Catln/Doc/typeInferPnt@ naming which point of the trace to report on.
typeInferPntArg :: String
typeInferPntArg = "/pnt"

-- | Backs @/Catln/Doc/typeInfer@, the data carried by a @Show/TypeInfer@ page. It renders the type
-- inference of the program being evaluated as JSON: a type variable on every meta, paired with the
-- notes its typecheck produced.
--
-- The trace itself is deliberately not included. It records every scheme update of every point in
-- every epoch, so serializing it whole would dwarf the program it describes; a page reads one point
-- at a time through 'typeInferPnt'.
typeInfer :: EPrim
typeInfer = EEnvPrim "typeInfer" prim
  where
    prim env _ = withTypeInfer env render
    -- The notes are rendered beside the inference rather than being what a failure is reported as.
    -- The whole point of keeping a trace is to explain a program that did NOT typecheck, so its
    -- notes are the answer here rather than an obstacle to one.
    render (CRes notes (vprgm, _)) = Right $ encodeJsonText (vprgm, notes)
    render (CErr notes)            = Left $ prettyCNotes notes

-- | The type inference of the page an evaluation is scoped to, rendered by whoever asked.
--
-- Of that page alone, never of a scope joined: a typecheck numbers its points within the run that
-- assigned them, so two pages' traces have colliding numbers and mean nothing merged together. An
-- evaluation scoped to no page therefore has no inference to report rather than an amalgamated one.
--
-- It hands the whole answer to its caller rather than the value inside it, because the notes a
-- typecheck left are part of what a reader of this is asking about.
withTypeInfer :: Env -> (CRes (VPrgm, TraceConstrain) -> Either String String) -> Either String Val
withTypeInfer env render = case evService env of
  Nothing -> Left noService
  Just (EvalService s scope) -> case scopedTo scope "type inference to report on" of
    Left err       -> Left err
    Right prgmName -> askService $ do
      graph <- csBuilt s (Just prgmName)
      return $ render $ do
        (_, vprgm, trace) <- requireComputed $ ssfTPrgmWithTrace =<< graphLookup prgmName graph
        return (vprgm, trace)

-- | Backs @/Catln/Doc/typeInferPnt@, the data carried by a @Show/TypeInferPnt@ page. It renders the
-- part of the same trace that touches a single point: the constraints mentioning it and, per epoch,
-- the updates it received, flipped into the chronological order the page reads them in.
typeInferPnt :: EPrim
typeInferPnt = EEnvPrim "typeInferPnt" prim
  where
    -- Which point was asked about is read before the inference is asked for, so a display written
    -- wrongly says so rather than reporting whatever the typecheck of the page had to say.
    prim env args = case H.lookup typeInferPntArg args of
      Just (IntVal pnt) -> withTypeInfer env (render pnt)
      Just v -> Left $ printf "typeInferPnt was given %s, which is not an Integer point" (show v)
      Nothing -> Left $ printf "typeInferPnt has no %s argument, only %s" typeInferPntArg (show $ sort $ H.keys args)
    render pnt (CRes _ (_, trace)) = Right $ encodeJsonText $ flipTraceConstrain $ filterTraceConstrain trace (fromInteger pnt)
    render _ (CErr notes)          = Left $ prettyCNotes notes

-- | The argument of @/Catln/Doc/debugger@ naming which function to run.
debuggerFunArg :: String
debuggerFunArg = "/fun"

-- | Backs @/Catln/Doc/debugger@, the data carried by a @Show/Debugger@ page. It runs a function of
-- the program being evaluated and renders that run as JSON: what it exited with, what it printed,
-- and the tree of what it evaluated.
--
-- Unlike the prims above this one does not read an artifact, it asks for one to be produced. It
-- cannot produce it itself: the evaluator is downstream of this module (@Eval@ imports
-- @Eval.Runtime@ imports this), so a run is something to ask the service for rather than something
-- to do here.
debugger :: EPrim
debugger = EEnvPrim "debugger" prim
  where
    prim env args = case H.lookup debuggerFunArg args of
      Just (StrVal fun) -> askRun env "program to run" (\s prgmName -> csRunFun s prgmName fun)
      Just v -> Left $ printf "debugger was given %s, which is not a String function name" (show v)
      Nothing -> Left $ printf "debugger has no %s argument, only %s" debuggerFunArg (show $ sort $ H.keys args)

-- | A run of the page an evaluation is scoped to, rendered as JSON.
--
-- A page is needed rather than a scope for both of the runs below. A named function needs one
-- program with one entry point, where every loaded page joined has as many @main@s as it has pages;
-- an expression means what the page it is written in makes it mean, and the join of every page is
-- not a page anything was written in.
askRun :: Env -> String -> (forall s. CtServiceApi s => s -> FileImport -> IO (CRes DebugRun)) -> Either String Val
askRun env what run = case evService env of
  Nothing -> Left noService
  Just (EvalService s scope) -> case scopedTo scope what of
    Left err       -> Left err
    Right prgmName -> askService $ fmap (fmap encodeJsonText . orNotes) (run s prgmName)

-- | The argument of @/Catln/Doc/debugExpr@ holding the expression to run, as its source.
debugExprArg :: String
debugExprArg = "/expr"

-- | Backs @/Catln/Doc/debugExpr@, the data carried by a @Show/Debugger@ page of one expression. It
-- compiles the expression as though it were written in the page being evaluated, runs it, and
-- renders the run as JSON.
--
-- Like 'debugger' it asks for an artifact to be produced rather than reading one, and for the same
-- reason cannot produce it here: compiling and evaluating are both downstream of this module.
debugExpr :: EPrim
debugExpr = EEnvPrim "debugExpr" prim
  where
    prim env args = case H.lookup debugExprArg args of
      Just (StrVal expr) -> askRun env "expression to run" (\s prgmName -> csRunExpr s prgmName expr)
      Just v -> Left $ printf "debugExpr was given %s, which is not a String expression" (show v)
      Nothing -> Left $ printf "debugExpr has no %s argument, only %s" debugExprArg (show $ sort $ H.keys args)

-- | One entry of the page tree a @Show/Sidebar@ carries: a directory holding further entries, or a
-- page to open. Both know the path they are at, which for a file is the name a page is asked for by,
-- so the webdocs link and label the tree without reassembling either.
data SidebarNode
  = SidebarDir { sbName :: String, sbPath :: String, sbChildren :: [SidebarNode] }
  -- | @sbDir@ is the directory the page was imported relative to, which is what a relative link
  -- written in that page's documentation resolves against.
  | SidebarFile { sbName :: String, sbPath :: String, sbDir :: Maybe FilePath }
  deriving (Generic, ToJSON)

-- | Backs @/Catln/Doc/sidebar@, the data carried by a @Show/Sidebar@ page. It renders every page
-- there is as the tree of directories they sit in.
--
-- Every page rather than the pages the evaluation covers, so this answers the same wherever it is
-- written. A navigation is for reaching the pages one has not reached, so a navigation of only what
-- the page it sits on imports would offer nothing but where the reader already is.
--
-- 'csPages' rather than anything that builds: which pages there are is known as soon as they were
-- read, and nothing has to be compiled to say so. That is what lets a doc server put a navigation up
-- without compiling everything it names.
sidebar :: EPrim
sidebar = EEnvPrim "sidebar" prim
  where
    prim Env{evService = Nothing} _ = Left noService
    prim Env{evService = Just (EvalService s _)} _ = askService $ do
      pages <- csPages s
      return $ encodeJsonText <$> sidebarNodes pages

-- | The pages as the tree of directories they sit in, which is the shape a navigation of them is
-- walked in.
sidebarNodes :: [FileImport] -> Either String [SidebarNode]
sidebarNodes pages = sidebarLevel "" . map splitEntry =<< traverse pageEntry pages
  where
    -- A page with no display path cannot be linked to, so it is reported rather than dropped from
    -- the navigation, where it would read as a page the doc server does not hold.
    pageEntry imp = case impDisp imp of
      Just disp -> Right (disp, impDir imp)
      Nothing -> Left $ printf "sidebar has no file path for %s, so it has no page to link to" (show imp)
    splitEntry (disp, dir) = (splitOn "/" $ drop 1 disp, dir)

-- | Groups the pages under one directory by their next path component, a directory of pages
-- becoming a node of its own that groups what is below it in turn.
--
-- Ordering is the pages a directory may be entered through first, in 'dirEntryFileNames' order, and
-- then alphabetical — so a directory leads with its entry point, and one holding both an overlay's
-- @dir.ctc@ and a @main.ct@ leads with the file it is actually entered through. It is a total order
-- over the names rather than the order the pages arrived in, since which page is built first says
-- nothing about where it belongs in a navigation.
sidebarLevel :: String -> [([String], Maybe FilePath)] -> Either String [SidebarNode]
sidebarLevel prefix entries = traverse toNode $ sortOn (\(n, _) -> (entryRank n, n)) $ M.toList grouped
  where
    grouped = M.fromListWith (++) [(n, [(rest, dir)]) | (n:rest, dir) <- entries]
    toNode (n, subs) = case partition (null . fst) subs of
      ([(_, dir)], []) -> Right $ SidebarFile n (subPath n) dir
      -- A directory of pages is a page in its own right: importing a directory imports its
      -- @main.ct@ through a program held under the directory's own path. That page is the directory
      -- node, so it is not also listed as a file beside the files it holds.
      (_, subDirs@(_:_)) -> SidebarDir n (subPath n) <$> sidebarLevel (subPath n) subDirs
      _ -> Left $ printf "sidebar has more than one page displayed as %s" (subPath n)
    subPath n = prefix ++ "/" ++ n
    -- Where a name sits among the entry files, and after all of them where it is not one of them.
    entryRank n = fromMaybe (length dirEntryFileNames) $ elemIndex n dirEntryFileNames

-- | The pages in the order the navigation shows them, as the flat list of the paths it links to.
-- This is the reading order the webdocs' previous/next buttons follow, so it is the tree's own order
-- rather than a second one: a directory is recursed into rather than listed, which is how the
-- navigation shows it.
sidebarOrder :: [FileImport] -> Either String [String]
sidebarOrder pages = concatMap pagePaths <$> sidebarNodes pages
  where
    pagePaths SidebarDir{sbChildren} = concatMap pagePaths sbChildren
    pagePaths SidebarFile{sbPath}    = [sbPath]

-- | The argument of @/Catln/Doc/searchPagesFor@ holding what was typed to search for.
searchQueryArg :: String
searchQueryArg = "/query"

-- | Backs @/Catln/Doc/searchPagesFor@, the data carried by a @Show/Search@ page. It renders the pages
-- whose path holds the query as the flat list of the paths they are asked for, sorted, which is the
-- order a box offers them in. An empty query holds of every path, so it renders every page there is,
-- which is what a box that has not been typed into shows.
--
-- The matching is done here rather than in the webdocs so that what is sent is what was asked for
-- rather than everything there is to ask about. It is done here rather than in Catln because a
-- [String] is not yet something Catln can look inside — @stack\/core\/str.ct@ has equality and
-- concatenation and no substring — so a search written there could only compare whole paths.
--
-- Case-insensitively, and on the path alone, which is what the webdocs matched against when they
-- held the list and matched it themselves. Only where the search happens has changed.
--
-- These are the same pages 'sidebar' renders as a tree, read the same way off 'csPages', and for the
-- same reason: what is worth searching for is above all what one has not already got to. A box
-- matching typing against a list and a navigation being walked want different shapes of the same
-- answer, and each page carries the shape it displays, so the two are rendered separately rather
-- than one being derived from the other in the webdocs.
searchPagesFor :: EPrim
searchPagesFor = EEnvPrim "searchPagesFor" prim
  where
    -- What was asked is read before who to ask, so a display written wrongly says so rather than
    -- reporting that it had nobody to ask.
    prim env args = case H.lookup searchQueryArg args of
      Nothing -> Left $ printf "searchPagesFor has no %s argument, only %s" searchQueryArg (show $ sort $ H.keys args)
      Just (StrVal query) -> case evService env of
        Nothing -> Left noService
        Just (EvalService s _) -> askService $ do
          pages <- csPages s
          return $ encodeJsonText . sort . filter (matchesQuery query) <$> traverse pagePath pages
      Just v -> Left $ printf "searchPagesFor was given %s, which is not a String query" (show v)
    -- As in 'sidebar', a page with no display path cannot be opened, so it is reported rather than
    -- dropped from the list, where it would read as a page the doc server does not hold.
    pagePath imp = case impDisp imp of
      Just disp -> Right disp
      Nothing -> Left $ printf "searchPagesFor has no file path for %s, so it has no page to open" (show imp)

-- | Whether what a reader typed holds of what they are being offered: without regard to case, and
-- anywhere within it rather than at its start, since a path is looked for by the name at its end as
-- often as by the directory at its front.
--
-- Shared by the searches rather than written once each, so that a box offering pages and pages to
-- run in one list narrows both by the same rule. Two copies would drift, and a reader would see one
-- section answer a query the other ignored.
matchesQuery :: String -> String -> Bool
matchesQuery query offered = map toLower query `isInfixOf` map toLower offered

-- | The class an annotation is marked as a member of, in @stack\/core\/webdoc.ct@, to be offered as
-- a page of its own.
docPageClassName :: TypeName
docPageClassName = mkName "/Catln/Doc/DocPage"

-- | The argument of @/Catln/Doc/docPagesFor@ holding what was typed to search for. The same argument
-- as 'searchPagesFor' takes, because it is the same typing: one box asks both.
docPagesQueryArg :: String
docPagesQueryArg = searchQueryArg

-- | One page a reader can ask for by name. 'dpAnnot' is the annotation that produces it, written
-- with absolute names so it means the same evaluated anywhere — it is what a link is built from, and
-- so is source the reader could equally have written themselves. 'dpLabel' is that source with its
-- values shortened to the names they are declared by, which is how a reader picks one.
data DocPageEntry = DocPageEntry {
    dpAnnot :: String,
    dpLabel :: String
  } deriving (Show, Generic, ToJSON)

-- | Backs @/Catln/Doc/docPagesFor@, the data carried by a @Show/DocPages@ display. It renders the
-- annotations that are pages in their own right, and that hold the query, as a JSON list of the
-- source to run and the label to offer it under.
--
-- Which annotations those are is read off the program rather than listed here: an annotation is one
-- by being marked @isa DocPage@ where it is declared, so adding a display to @webdoc.ct@ makes it
-- findable by marking it and nothing else. This is the same reason 'listProgram' takes its stage as
-- an enum member rather than a string — what there is to ask for is the program's to say.
--
-- An empty query holds of everything, so a box that has not been typed into is offered every page
-- there is to run. Unlike 'searchPagesFor' that is a handful of entries rather than the doc server's
-- whole listing, which is why this one is asked before anything is typed and that one is not.
docPagesFor :: EPrim
docPagesFor = EEnvPrim "docPagesFor" prim
  where
    prim Env{evTypeEnv} args = case H.lookup docPagesQueryArg args of
      Just (StrVal query) -> Right $ encodeJson $ filter (offered query) $ allDocPages evTypeEnv
      Just v -> Left $ printf "docPagesFor was given %s, which is not a String query" (show v)
      Nothing -> Left $ printf "docPagesFor has no %s argument, only %s" docPagesQueryArg (show $ sort $ H.keys args)
    -- Matched against both, since the two differ only in how much of a value's name is written and a
    -- reader may have typed either the module it is in or the name it is picked by.
    offered query DocPageEntry{dpAnnot, dpLabel} = matchesQuery query dpAnnot || matchesQuery query dpLabel

-- | Every page the marked annotations offer, sorted, which is the order a box offers them in.
allDocPages :: (TypeGraph tg) => TypeEnv tg -> [DocPageEntry]
allDocPages typeEnv = sortOn dpAnnot $ concatMap (annotDocPages typeEnv) (docPageAnnots typeEnv)

-- | The annotations marked as pages of their own.
docPageAnnots :: (TypeGraph tg) => TypeEnv tg -> [TypeName]
docPageAnnots typeEnv = case H.lookup docPageClassName (scClasses (teSatCache typeEnv)) of
  Just ClassSatInfo{csiTypes} -> mapMaybe (fmap ptName . maybeGetSingleton) csiTypes
  -- A program that declared no such class marked no annotations with it, which is no pages rather
  -- than an error: the class lives in webdoc.ct, and a program is not required to reach it.
  Nothing                     -> []

-- | The pages one annotation offers: the one it is where it takes no arguments, and one per
-- combination of its arguments' members where it takes only arguments whose values can be named.
--
-- None at all where any argument cannot be, which falls out rather than being tested for — an
-- argument with no members to offer is an empty product. An annotation taking a [String] names
-- something only the reader knows (a type to filter to, a function to run), so there is no list of
-- what to offer, and it is left to the places that ask for that name.
--
-- Over every declaration of the name, since one annotation may be declared at more than one arity —
-- @#assert@ is, with and without its message — and each arity is its own thing to offer.
annotDocPages :: (TypeGraph tg) => TypeEnv tg -> TypeName -> [DocPageEntry]
annotDocPages typeEnv annotName = do
  PartialType{ptArgs} <- typeGraphData typeEnv annotName
  -- Sorted by name so that an annotation of two arguments is written the one way, rather than in
  -- whichever order the arguments happened to hash into.
  binds <- traverse argBinds $ sortOn (nameStr . pkName . fst) $ H.toList ptArgs
  return $ DocPageEntry (written nameStr binds) (written lastNamePart binds)
  where
    argBinds (argName, argType) = [(argName, member) | member <- sealedMembers typeEnv argType]
    written :: (TypeName -> String) -> [(ArgName, TypeName)] -> String
    written _ [] = lastNamePart annotName
    written shorten binds = printf "%s(%s)" (lastNamePart annotName) (intercalate ", " $ map (writtenArg shorten) binds)
    writtenArg :: (TypeName -> String) -> (ArgName, TypeName) -> String
    -- The argument names are absolute, and an argument is written by the name it is declared under.
    writtenArg shorten (argName, member) = printf "%s= %s" (drop 1 $ nameStr $ pkName argName) (shorten member)

-- | The members a sealed class names, and none for anything else.
--
-- Sealed because that is what makes the members the whole of what there is: an open class may be
-- joined by a type in a program that has not been read yet, so what it could be offered as is not
-- something to answer now. This is why @#print(p -> DShow)@ contributes nothing though [DShow] is a
-- class — it is open, and every displayable type there will ever be belongs to it.
--
-- A member carrying fields of its own is left out for the same reason a [String] argument is: it is
-- a shape to fill in rather than a value to pick.
sealedMembers :: (TypeGraph tg) => TypeEnv tg -> Type -> [TypeName]
sealedMembers typeEnv argType = case maybeGetClassSingleton argType of
  Just cls | Just ClassSatInfo{csiSealed=True, csiTypes} <- H.lookup (ptName cls) (scClasses (teSatCache typeEnv))
             -> [ptName member | Just member <- map maybeGetSingleton csiTypes, H.null (ptArgs member)]
  _ -> []

-- | A name as it reads where the module it is in is not in question, which is the part after its
-- last separator.
lastNamePart :: TypeName -> String
lastNamePart = last . splitOn "/" . nameStr

-- | Backs @/Catln/Doc/testRun@, the data carried by a @Show/Test@ display. It renders the tests of
-- the definition the annotation is written on as JSON: what each was, whether it passed, how many
-- generated cases it got through, and what a failure reported.
--
-- Of that definition, not of the page: a @#test@ marks one definition as a test, so it reports on
-- that one. An annotation written on nothing — a free-standing one asked for over the API — is not
-- on a definition to report on, and says so rather than answering about some other one.
--
-- Like 'debugger' this is a run rather than an artifact: running a page's tests is work, and neither
-- something an evaluation can do to itself. It asks the service for the run, which is what makes
-- the tests run exactly when a @#test@ is being displayed. Nothing has to decide in advance whether
-- a page will want them — the display that wants them is what asks.
testRun :: EPrim
testRun = EEnvPrim "testRun" prim
  where
    prim Env{evAnnotObj=Nothing} _ = Left "This #test is not written on a definition, so it has no test of one to report on"
    prim env@Env{evAnnotObj=Just oa} _ = case evService env of
      Nothing -> Left noService
      Just (EvalService s scope) -> case scopedTo scope "tests to report on" of
        Left err -> Left err
        Right prgmName -> askService $ do
          tests <- csTests s prgmName
          -- Of the definition the annotation is written on rather than of the page, since a @#test@
          -- marks one definition and reports on that one.
          return $ encodeJsonText . filter ((== oaTestId oa) . trOwnerId) <$> orNotes tests

-- | One task of one note, as the value it displays as and as the expression it was declared by.
--
-- A task shown on its own page is edited by replacing the expression it was compiled from, which the
-- page it is shown on names. A task shown in a list of every note's tasks has no such page — the
-- list is not the note — so it carries its own: 'ptPage' is the page the task was written in and
-- 'ptExpr' the expression of it, which together are what @\/api\/write-expr@ replaces. So checking a
-- box in a list of tasks writes the note the task came from rather than whatever is displaying it.
--
-- 'ptExpr' is the rendered form of the meta id rather than the id itself, since that is what a
-- request naming the expression sends.
data PageTask = PageTask { ptPage :: String
                         , ptExpr :: String
                         , ptTask :: Val
                         } deriving (Generic, ToJSON)

-- | Backs @/Data/Note/taskList@, the data carried by a @Show/TaskList@ display. It renders the tasks
-- of the pages the evaluation covers as JSON: each the value the task displays as, beside the page it
-- was written in and the expression it was declared by.
--
-- That a task is a @\/Data\/Note\/Task@ is known here and nowhere below. What the service offers is
-- the values a page declares of a type ('csValues'); which types are worth gathering, and what to
-- make of the ones it finds, is this display's own business. So the compilation service stays a
-- service for programs rather than one that knows what a note is.
--
-- The members of that class are named rather than the class itself, because a value is declared as
-- the member it is and what the pipeline offers is values by the path they were declared at. Naming
-- the class instead would be asking the evaluator which types are members of it, which is the type
-- environment's to answer and not this side's. They are named together in the one call so that a
-- note mixing the two readings gives its tasks back in the order it wrote them.
--
-- Like 'sidebar' this reports on which pages there are rather than on the program it was evaluated
-- against, so evaluated unscoped it is every task the doc server holds — the list of all of them,
-- taken page by page in the order the pages are opened by.
taskList :: EPrim
taskList = EEnvPrim "taskList" prim
  where
    prim env _ = case evService env of
      Nothing -> Left noService
      Just (EvalService s scope) -> askService $ do
        graph <- csBuilt s scope
        -- One gatherer for every page asked about, not one per page. It holds the scope's
        -- typechecked programs, so asking for a fresh one per page would pay for the scope once per
        -- page — over a vault of notes, quadratic in the notes.
        gather <- csValues s scope
        let pages = map snd3 $ graphToNodes $ maybe graph (`graphFilterReaches` graph) scope
        return $ encodeJsonText . concat <$> traverse (pageTasks gather) (sortOn impDisp pages)

    pageTasks pageValues prgmName = do
      tasks <- orNotes $ pageValues (map mkName [noteSimpleTaskPath, noteObsidianTaskPath]) prgmName
      case (tasks, impDisp prgmName) of
        ([], _) -> return []
        -- As in 'sidebar', a page with no display path is reported rather than dropped. Here it is a
        -- task with no note to be written back to, which is worth saying rather than serving a list
        -- that is quietly missing it.
        (_, Nothing) -> Left $ printf "taskList has no file path for %s, so its tasks have no note to be written back to" (show prgmName)
        (_, Just disp) -> return $ map (pageTask disp) tasks

    pageTask disp (e, task) = PageTask disp (UUID.toString $ getMetaID $ getExprMeta e) task

-- | A pipeline answer as a primitive's, whose failure is the notes that produced it. 'CRes.fromCRes'
-- is the partial version of this, which a primitive must not use: a stage it was not given is a
-- failure to report rather than one to crash on.
orNotes :: CRes a -> Either String a
orNotes (CRes _ r)   = Right r
orNotes (CErr notes) = Left $ prettyCNotes notes

-- | What a primitive says when it was given no compilation service to ask.
--
-- Said rather than answered around: an evaluation with nothing to report on cannot answer, and
-- reporting something else would be worse than failing. Every evaluation the service itself starts
-- is one of these ('Eval.evalBaseEnv'), so a display reached by way of another display says this
-- rather than recurring.
noService :: String
noService = "This evaluation was not given the compilation service its program was compiled by"

-- | The page an evaluation is scoped to, or why it is not scoped to one.
--
-- Some displays are about the program the annotation is written in and some are about the pages
-- there are, and only the first needs a page. The wording is what @\/api\/annot@ answers with when
-- it was asked without one, and is pinned by the webdocs' own integration tests, so it is written
-- here once rather than in each display that needs a page.
scopedTo :: Maybe FileImport -> String -> Either String FileImport
scopedTo (Just prgmName) _ = Right prgmName
scopedTo Nothing what = Left $ "This evaluation is not scoped to a single page, so it has no " ++ what

-- | Asks the compilation service for the text a display carries.
--
-- The 'unsafePerformIO' is what lets a primitive ask at all: a primitive is pure, and every question
-- worth asking the service — which pages there are, what a page compiled to, what its tests did — is
-- something the service answers in 'IO'. @Eval.Runtime.ioInput@ and 'Eval.evalDebugPrgm' already make
-- the same trade.
--
-- The answer is forced HERE, which is the whole discipline of doing so. The service is behind an
-- MVar which a build holds for a whole SCC (@CtService.ctssBuildSCC@) and a read holds for a whole
-- read (@CtService.ctssReadOf@), and an MVar is not re-entrant. A primitive's value is kept on the
-- page it was evaluated for (@CtService.getEvalAnnots@) and read later by whoever displays it — so an
-- unforced ask left inside one would be run at some unrelated moment, possibly inside one of those
-- locks, and would HANG there rather than fail. Asking inside the primitive is asking at the one
-- moment nothing is held.
--
-- It takes the answer as text rather than as a 'Val' so that forcing it is total: there is no
-- constructor a lazy remainder could hide behind. Every display here carries JSON, so this is what
-- all of them return anyway.
--
-- The ask may be run more than once, which 'unsafePerformIO' does not promise not to do. That is
-- harmless for all of them: building is idempotent, a test run is kept on its page, and a debugger's
-- run is mocked. Nothing here writes anything a second run would double.
askService :: IO (Either String String) -> Either String Val
askService ask = case unsafePerformIO ask of
  Left err   -> Left err
  Right text -> length text `seq` Right (StrVal text)

encodeJson :: (ToJSON p) => p -> Val
encodeJson = StrVal . encodeJsonText

-- | A display's data as the JSON text it is carried as.
encodeJsonText :: (ToJSON p) => p -> String
encodeJsonText = TL.unpack . encodeToLazyText
