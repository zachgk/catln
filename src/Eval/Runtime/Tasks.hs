--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.Tasks
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the primitives backing the displays a note's tasks are shown by, which are
-- declared in @stack/core/note.ct@.
--
-- Apart from the webdocs' own displays because these are the ones that know what a note is. What
-- the compilation service offers is the values a page declares of a type ('Eval.Common.csValues');
-- which types are worth gathering, and what to make of the ones it finds, is a display's own
-- business — so the knowledge that a task is a @\/Data\/Note\/Task@ lives here, in the module of the
-- library that declares one, and nowhere below it.
--------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Eval.Runtime.Tasks where

import           CtConstants              (falsePrim, noteObsidianTaskPath,
                                           noteSimpleTaskPath, noteTaskContents,
                                           noteTaskDescription, noteTaskDue,
                                           noteTaskPriority, noteTaskRecurrence,
                                           noteTaskRest, noteTaskScheduled,
                                           noteTaskState, noteTaskStatusType,
                                           truePrim)
import           Data.Aeson               (ToJSON)
import qualified Data.HashMap.Strict      as H
import           Data.List                (intercalate, sort, sortOn)
import           Data.Maybe               (fromMaybe, isJust)
import qualified Data.UUID                as UUID
import           Eval.Common
import           Eval.Runtime.WebDocs     (askService, encodeJsonText,
                                           noService, orNotes)
import           GHC.Generics             (Generic)
import           Semantics.Prgm
import           Semantics.Types          (mkName)
import           Syntax.Obsidian.Parser   (memberPriority, noteVault,
                                           statusTypeKind)
import           Syntax.Obsidian.Query    (QueryTask (..), applyQuery,
                                           parseQuery, readDate)
import           Syntax.Obsidian.Settings (StatusKind (..))
import           Syntax.Obsidian.Task     (TaskPriority (..), TaskRest (..),
                                           readTaskRest, tags)
import           System.FilePath          (makeRelative)
import           Text.Printf
import           Utils

-- | The primitives backing the @Data/Note@ displays.
tasksPrims :: [EPrim]
tasksPrims = [taskList, tasksQuery]

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
-- The members of @\/Data\/Note\/Task@ are named rather than the class itself, because a value is
-- declared as the member it is and what the pipeline offers is values by the path they were declared
-- at. Naming the class instead would be asking the evaluator which types are members of it, which is
-- the type environment's to answer and not this side's. They are named together in the one call so
-- that a note mixing the two readings gives its tasks back in the order it wrote them.
--
-- Like 'Eval.Runtime.WebDocs.sidebar' this reports on which pages there are rather than on the
-- program it was evaluated against, so evaluated unscoped it is every task the doc server holds —
-- the list of all of them, taken page by page in the order the pages are opened by.
taskList :: EPrim
taskList = EEnvPrim "taskList" prim
  where
    prim env _ = case evService env of
      Nothing -> Left noService
      Just svc -> askService $ do
        let scope = esScope svc
        graph <- esBuilt svc scope
        -- One gatherer for every page asked about, not one per page. It holds the scope's
        -- typechecked programs, so asking for a fresh one per page would pay for the scope once per
        -- page — over a vault of notes, quadratic in the notes.
        gather <- esValues svc scope
        let pages = map snd3 $ graphToNodes $ maybe graph (`graphFilterReaches` graph) scope
        return $ encodeJsonText . concat <$> traverse (pageTasks gather) (sortOn impDisp pages)

    pageTasks pageValues prgmName = do
      tasks <- orNotes $ pageValues (map mkName [noteSimpleTaskPath, noteObsidianTaskPath]) prgmName
      case (tasks, impDisp prgmName) of
        ([], _) -> return []
        -- As in 'Eval.Runtime.WebDocs.sidebar', a page with no display path is reported rather than
        -- dropped. Here it is a task with no note to be written back to, which is worth saying
        -- rather than serving a list that is quietly missing it.
        (_, Nothing) -> Left $ printf "taskList has no file path for %s, so its tasks have no note to be written back to" (show prgmName)
        (_, Just disp) -> return $ map (pageTask disp) tasks

    pageTask disp (e, task) = PageTask disp (UUID.toString $ getMetaID $ getExprMeta e) task

-- | The argument of @/Data/Note/tasksQuery@ holding the query as the note wrote it.
tasksQueryArg :: String
tasksQueryArg = "/query"

-- | What a note's query found, or why nothing was found.
--
-- Both in the one answer because a query is something a reader typed: a query nobody can read is an
-- ordinary answer and not a failure of the page, so the block says which line was not understood
-- where the reader wrote it. Answering the two apart would be running the query a second time to
-- ask whether it ran.
data QueryResult = QueryResult { qrError :: Maybe String
                               , qrTasks :: [PageTask]
                               } deriving (Generic, ToJSON)

-- | Backs @/Data/Note/tasksQuery@, the data carried by a @Show/TaskQuery@ display. It answers the
-- query a note wrote in a block fenced as @tasks@, over the tasks of the vault that note is in.
--
-- The vault and not the scope. An annotation is evaluated scoped to the page it was written in, and
-- a note's page reaches only the notes it links to — so a query answered over what the page reaches
-- would answer about almost nothing. Answered over every page there is instead, and then narrowed
-- to the pages written under the vault holding the note, which is what the plugin's own query asks
-- about and is the one narrowing a reader means.
--
-- Which pages there are is asked about unscoped, so the record of what this asked ('AnnotAsks') is
-- that any change at all can alter it. That is the right grain and not a widening: a vault does not
-- import its notes, so the walk which drops what a change reached can never reach a query's page
-- from the note whose task changed.
--
-- The query is read before anything is asked, so a query nobody can read costs no build and records
-- no ask. That answer stops being the answer only when the note itself changes, and the note's own
-- page is where the walk that drops it begins.
tasksQuery :: EPrim
tasksQuery = EEnvPrim "tasksQuery" prim
  where
    prim env args = case evService env of
      Nothing -> Left noService
      Just svc -> case H.lookup tasksQueryArg args of
        Just (StrVal src) -> askService $ answer svc src
        Just v -> Left $ printf "tasksQuery was given %s, which is not a String query" (show v)
        Nothing -> Left $ printf "tasksQuery has no %s argument, only %s" tasksQueryArg (show $ sort $ H.keys args)

    answer svc src = do
      today <- esToday svc
      case parseQuery today src of
        Left bad -> return $ said $ intercalate "\n" bad
        Right query -> do
          vault <- queryVault svc
          case vault of
            Left message -> return $ said message
            Right dir    -> fmap (found query) <$> gather svc dir

    -- Something the reader is told in the block they wrote, which is an answer rather than a
    -- failure: the note around it is shown as it was written.
    said message = Right $ encodeJsonText $ QueryResult (Just message) []
    found query gathered = encodeJsonText $ QueryResult Nothing $ map snd $ applyQuery query gathered

    gather svc vault = do
      graph <- esBuilt svc Nothing
      -- One gatherer for every page, as 'taskList' takes one and for the same reason.
      gatherer <- esValues svc Nothing
      -- A page with no file path is not under the vault, so it is left out here rather than
      -- reported as 'taskList' reports one: a page which is not a note of this vault is not a note
      -- whose tasks went missing.
      let pages = sortOn (\(_, _, within) -> within) [(page, disp, within) | page <- map snd3 (graphToNodes graph)
                                                    , Just disp <- [impDisp page]
                                                    , Just within <- [underVault vault disp]]
      return $ concat <$> traverse (pageQueryTasks gatherer) pages

    pageQueryTasks gatherer (prgmName, disp, within) = do
      tasks <- orNotes $ gatherer (map mkName [noteSimpleTaskPath, noteObsidianTaskPath]) prgmName
      traverse (asked within disp) tasks

    asked within disp (e, task) = (, pageTask disp (e, task)) <$> queryTask within task
    pageTask disp (e, task) = PageTask disp (UUID.toString $ getMetaID $ getExprMeta e) task

-- | The vault a query is to search, or what to tell the reader instead.
--
-- The vault of the page the annotation was evaluated for, which is the note the query was written
-- in. A page under no vault is told about rather than answered over every page there is: a query
-- written where there are no notes has nothing to search, and searching everything instead would
-- answer a question nobody asked.
queryVault :: EvalService -> IO (Either String FilePath)
queryVault svc = case esScope svc of
  Nothing -> return $ Left "This evaluation is not of one page, so a tasks query in it has no vault to search."
  Just prgmName -> case impDisp prgmName of
    Nothing -> return $ Left $ printf "The page %s has no file path, so a tasks query written in it has no vault to search." (show prgmName)
    Just path -> maybe (Left $ noVault path) Right <$> noteVault path
  where
    noVault :: FilePath -> String
    noVault = printf "The page %s is in no Obsidian vault, so a tasks query written in it has no notes to search."

-- | Where a page is written within a vault, and nothing for a page written outside it.
--
-- Read off the path rather than by walking for the vault again: every page under the vault's
-- directory is a page of it. A vault written inside another is not told apart from the one holding
-- it, so a query in the outer one searches both.
underVault :: FilePath -> FilePath -> Maybe FilePath
underVault vault path
  | path == vault = Nothing
  | within /= path = Just within
  | otherwise = Nothing
  where within = makeRelative vault path

-- | One gathered task as everything a query may ask about it.
--
-- A checkbox read without the plugin's own reading of its line fills in what such a line says — what
-- it says, whether it is done, the tags in it, and where it was written — and carries no dates, no
-- priority and no recurrence. So a query asking about those finds it as a task which has none rather
-- than not at all, which is the reading a vault running no plugin has of every checkbox in it.
queryTask :: FilePath -> Val -> Either String QueryTask
queryTask within (TupleVal name args)
  | name == noteSimpleTaskPath = do
      says <- valStrArg name noteTaskContents args
      done <- valDoneArg name args
      return (emptyQueryTask within){
        qtStatusKind = if done then KDone else KTodo,
        qtStatusName = if done then "Done" else "Todo",
        qtDescription = says,
        qtTags = tags says
                                    }
  | name == noteObsidianTaskPath = do
      description <- valStrArg name noteTaskDescription args
      kind <- valMemberArg name noteTaskStatusType statusTypeKind "a status type" args
      priority <- valMemberArg name noteTaskPriority memberPriority "a priority" args
      scheduled <- valMaybeStrArg name noteTaskScheduled args
      due <- valMaybeStrArg name noteTaskDue args
      recurrence <- valMaybeStrArg name noteTaskRecurrence args
      rest <- readRest =<< valStrArg name noteTaskRest args
      return QueryTask{
        qtStatusKind = kind,
        qtStatusName = fromMaybe "" (trStatusName rest),
        qtDescription = description,
        qtTags = trTags rest,
        qtPriority = priority,
        qtDue = readDate =<< due,
        qtScheduled = readDate =<< scheduled,
        qtStart = readDate =<< trStart rest,
        qtCreated = readDate =<< trCreated rest,
        -- The one date a finished task and a dropped one both write, read as whichever of the two
        -- its status says it is. A task is done or it is cancelled and never both.
        qtDone = if kind == KDone then readDate =<< trStatusDate rest else Nothing,
        qtCancelled = if kind == KCancelled then readDate =<< trStatusDate rest else Nothing,
        qtRecurring = isJust recurrence,
        qtPath = within,
        qtSubItem = False
                      }
  where
    readRest json = maybe (Left $ printf "the %s of a %s is not JSON: %s" noteTaskRest name json) Right $ readTaskRest json
queryTask _ v = Left $ printf "%s is not a task" (show v)

-- | A checkbox saying nothing, which reading one fills in.
emptyQueryTask :: FilePath -> QueryTask
emptyQueryTask within = QueryTask KTodo "Todo" "" [] PNone Nothing Nothing Nothing Nothing Nothing Nothing False within False

-- | Whether a plain checkbox's box is checked, read from the state it was declared with.
valDoneArg :: String -> H.HashMap String Val -> Either String Bool
valDoneArg name args = do
  v <- valArg name noteTaskState args
  case v of
    TupleVal state _ | state == truePrim -> Right True
                     | state == falsePrim -> Right False
    _ -> Left $ printf "The %s of a %s is %s, which is not a Boolean" noteTaskState name (show v)

-- | Which member of a class an argument holds, out of the members that class has.
valMemberArg :: String -> String -> (String -> Maybe a) -> String -> H.HashMap String Val -> Either String a
valMemberArg name argName member what args = do
  v <- valArg name argName args
  case v of
    TupleVal m _ | Just found <- member m -> Right found
    _ -> Left $ printf "The %s of a %s is %s, which is not %s" argName name (show v) what
