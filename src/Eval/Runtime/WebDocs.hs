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

module Eval.Runtime.WebDocs where

import           CRes
import           CtConstants         (noteSimpleTaskPath)
import           Data.Aeson          (ToJSON)
import           Data.Aeson.Text     (encodeToLazyText)
import           Data.Graph
import qualified Data.HashMap.Strict as H
import           Data.List           (partition, sort, sortOn)
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import qualified Data.Text.Lazy      as TL
import qualified Data.UUID           as UUID
import           Eval.Common
import           GHC.Generics        (Generic)
import           Semantics.Prgm
import           Semantics.Types     (ClassGraph (..), fromPartialName, mkName,
                                      relativeNameMatches)
import           Text.Printf
import           TypeCheck.Common    (filterTraceConstrain, flipTraceConstrain)
import           Utils

-- | The primitives backing the @Catln/Doc@ display annotations.
webDocsPrims :: [EPrim]
webDocsPrims = [listProgram, filterProgram, typeInfer, typeInferPnt, debugger, debugExpr, sidebar, searchPages, testRun, taskList]

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
-- 'TBEnv'. The other two are not reachable from an evaluation and are supplied by whoever ran those
-- stages, as 'evStagePrgms'.
listProgram :: EPrim
listProgram = EEnvPrim "listProgram" prim
  where
    prim Env{evTbEnv=TBEnv{tbPrgm}, evStagePrgms=StagePrgms{spDesPrgm, spTBPrgm}} args = case H.lookup listProgramModeArg args of
      Just (TupleVal mode _)
        | mode == desugarMode -> fromStage spDesPrgm
        | mode == typecheckMode -> Right $ encodeJson tbPrgm
        | mode == treeBuildMode -> fromStage spTBPrgm
      Just v -> Left $ printf "listProgram was given %s, which is not a ListProgramMode" (show v)
      Nothing -> Left $ printf "listProgram has no %s argument, only %s" listProgramModeArg (show $ sort $ H.keys args)
    fromStage (CRes _ prgm) = Right $ encodeJson prgm
    fromStage (CErr notes)  = Left $ prettyCNotes notes

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
    prim Env{evStagePrgms=StagePrgms{spTypeInfer}} _ = case spTypeInfer of
      CRes notes (vprgm, _) -> Right $ encodeJson (vprgm, notes)
      CErr notes            -> Left $ prettyCNotes notes

-- | Backs @/Catln/Doc/typeInferPnt@, the data carried by a @Show/TypeInferPnt@ page. It renders the
-- part of the same trace that touches a single point: the constraints mentioning it and, per epoch,
-- the updates it received, flipped into the chronological order the page reads them in.
typeInferPnt :: EPrim
typeInferPnt = EEnvPrim "typeInferPnt" prim
  where
    prim Env{evStagePrgms=StagePrgms{spTypeInfer}} args = case (spTypeInfer, H.lookup typeInferPntArg args) of
      (CErr notes, _) -> Left $ prettyCNotes notes
      (CRes _ (_, trace), Just (IntVal pnt)) -> Right $ encodeJson $ flipTraceConstrain $ filterTraceConstrain trace (fromInteger pnt)
      (_, Just v) -> Left $ printf "typeInferPnt was given %s, which is not an Integer point" (show v)
      (_, Nothing) -> Left $ printf "typeInferPnt has no %s argument, only %s" typeInferPntArg (show $ sort $ H.keys args)

-- | The argument of @/Catln/Doc/debugger@ naming which function to run.
debuggerFunArg :: String
debuggerFunArg = "/fun"

-- | Backs @/Catln/Doc/debugger@, the data carried by a @Show/Debugger@ page. It runs a function of
-- the program being evaluated and renders that run as JSON: what it exited with, what it printed,
-- and the tree of what it evaluated.
--
-- Unlike the prims above this one does not read an artifact, it asks for one to be produced, so it
-- cannot compute its own answer: the evaluator is downstream of this module (@Eval@ imports
-- @Eval.Runtime@ imports this), and reaching back up to it would be a module cycle. The run therefore
-- arrives as a function on 'StagePrgms', supplied by whoever is above both.
debugger :: EPrim
debugger = EEnvPrim "debugger" prim
  where
    prim Env{evStagePrgms=StagePrgms{spDebugRun}} args = case H.lookup debuggerFunArg args of
      Just (StrVal fun) -> case spDebugRun fun of
        CRes _ run -> Right $ encodeJson run
        CErr notes -> Left $ prettyCNotes notes
      Just v -> Left $ printf "debugger was given %s, which is not a String function name" (show v)
      Nothing -> Left $ printf "debugger has no %s argument, only %s" debuggerFunArg (show $ sort $ H.keys args)

-- | The argument of @/Catln/Doc/debugExpr@ holding the expression to run, as its source.
debugExprArg :: String
debugExprArg = "/expr"

-- | Backs @/Catln/Doc/debugExpr@, the data carried by a @Show/Debugger@ page of one expression. It
-- compiles the expression as though it were written in the page being evaluated, runs it, and
-- renders the run as JSON.
--
-- Like 'debugger' it asks for an artifact to be produced rather than reading one, and for the same
-- reason cannot produce it here: compiling and evaluating are both downstream of this module. The
-- run arrives as a function on 'StagePrgms'.
debugExpr :: EPrim
debugExpr = EEnvPrim "debugExpr" prim
  where
    prim Env{evStagePrgms=StagePrgms{spDebugExpr}} args = case H.lookup debugExprArg args of
      Just (StrVal expr) -> case spDebugExpr expr of
        CRes _ run -> Right $ encodeJson run
        CErr notes -> Left $ prettyCNotes notes
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
-- Every page rather than the pages the evaluation covers ('spAllPages', not 'spPages'), so this
-- answers the same wherever it is written. A navigation is for reaching the pages one has not
-- reached, so a navigation of only what the page it sits on imports would offer nothing but where
-- the reader already is. Reading it off the pages there are rather than off a compiled program is
-- also what lets a doc server put a navigation up without compiling everything it names.
sidebar :: EPrim
sidebar = EEnvPrim "sidebar" prim
  where
    prim Env{evStagePrgms=StagePrgms{spAllPages}} _ = case spAllPages of
      CErr notes   -> Left $ prettyCNotes notes
      CRes _ pages -> encodeJson <$> sidebarNodes pages

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

-- | The page a directory of pages is entered through: importing a directory imports this file of it
-- (see @Syntax.InferImport@), so it is the page a directory's entry in the navigation leads with.
mainFileName :: String
mainFileName = "main.ct"

-- | Groups the pages under one directory by their next path component, a directory of pages
-- becoming a node of its own that groups what is below it in turn.
--
-- Ordering is @main.ct@ first and then alphabetical, so a directory leads with its entry point. It
-- is a total order over the names rather than the order the pages arrived in, since which page is
-- built first says nothing about where it belongs in a navigation.
sidebarLevel :: String -> [([String], Maybe FilePath)] -> Either String [SidebarNode]
sidebarLevel prefix entries = traverse toNode $ sortOn (\(n, _) -> (n /= mainFileName, n)) $ M.toList grouped
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

-- | The pages in the order the navigation shows them, as the flat list of the paths it links to.
-- This is the reading order the webdocs' previous/next buttons follow, so it is the tree's own order
-- rather than a second one: a directory is recursed into rather than listed, which is how the
-- navigation shows it.
sidebarOrder :: [FileImport] -> Either String [String]
sidebarOrder pages = concatMap pagePaths <$> sidebarNodes pages
  where
    pagePaths SidebarDir{sbChildren} = concatMap pagePaths sbChildren
    pagePaths SidebarFile{sbPath}    = [sbPath]

-- | Backs @/Catln/Doc/searchPages@, the data carried by a @Show/Search@ page. It renders every page
-- there is as the flat list of the paths they are asked for, sorted, which is the order a box offers
-- them in.
--
-- These are the same pages 'sidebar' renders as a tree, read the same way off 'spAllPages', and for
-- the same reason: what is worth searching for is above all what one has not already got to. A box
-- matching typing against a list and a navigation being walked want different shapes of the same
-- answer, and each page carries the shape it displays, so the two are rendered separately rather
-- than one being derived from the other in the webdocs.
searchPages :: EPrim
searchPages = EEnvPrim "searchPages" prim
  where
    prim Env{evStagePrgms=StagePrgms{spAllPages}} _ = case spAllPages of
      CErr notes   -> Left $ prettyCNotes notes
      CRes _ pages -> encodeJson . sort <$> traverse pagePath pages
    -- As in 'sidebar', a page with no display path cannot be opened, so it is reported rather than
    -- dropped from the list, where it would read as a page the doc server does not hold.
    pagePath imp = case impDisp imp of
      Just disp -> Right disp
      Nothing -> Left $ printf "searchPages has no file path for %s, so it has no page to open" (show imp)

-- | Backs @/Catln/Doc/testRun@, the data carried by a @Show/Test@ display. It renders the tests of
-- the definition the annotation is written on as JSON: what each was, whether it passed, how many
-- generated cases it got through, and what a failure reported.
--
-- Of that definition, not of the page: a @#test@ marks one definition as a test, so it reports on
-- that one. An annotation written on nothing — a free-standing one asked for over the API — is not
-- on a definition to report on, and says so rather than answering about some other one.
--
-- Like 'debugger' this is a run rather than an artifact, and running the tests is neither something
-- an evaluation can do to itself nor something to do while merely displaying a page that has none.
-- It therefore arrives already run, on 'StagePrgms', from whoever asked for the display.
testRun :: EPrim
testRun = EEnvPrim "testRun" prim
  where
    prim Env{evAnnotObj=Nothing} _ = Left "This #test is not written on a definition, so it has no test of one to report on"
    prim Env{evAnnotObj=Just oa, evStagePrgms=StagePrgms{spTests}} _ = case spTests of
      CErr notes -> Left $ prettyCNotes notes
      CRes _ tests -> Right $ encodeJson $ filter ((== oaTestId oa) . trOwnerId) tests

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
-- That a task is a @\/Data\/Note\/Task@ is known here and nowhere below. What the pipeline offers is
-- the values a page declares of a type ('spPageValues'); which types are worth gathering, and what
-- to make of the ones it finds, is this display's own business. So the compilation service stays a
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
    prim Env{evStagePrgms=StagePrgms{spPages, spPageValues}} _ = do
      pages <- orNotes spPages
      encodeJson . concat <$> traverse (pageTasks spPageValues) (sortOn impDisp pages)

    pageTasks pageValues prgmName = do
      tasks <- orNotes $ pageValues (map mkName [noteSimpleTaskPath]) prgmName
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

encodeJson :: (ToJSON p) => p -> Val
encodeJson = StrVal . TL.unpack . encodeToLazyText
