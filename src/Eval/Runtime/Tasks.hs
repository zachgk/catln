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

import           CtConstants          (noteObsidianTaskPath, noteSimpleTaskPath)
import           Data.Aeson           (ToJSON)
import           Data.List            (sortOn)
import qualified Data.UUID            as UUID
import           Eval.Common
import           Eval.Runtime.WebDocs (askService, encodeJsonText, noService,
                                       orNotes)
import           GHC.Generics         (Generic)
import           Semantics.Prgm
import           Semantics.Types      (mkName)
import           Text.Printf
import           Utils

-- | The primitives backing the @Data/Note@ displays.
tasksPrims :: [EPrim]
tasksPrims = [taskList]

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
