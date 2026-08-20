-- | Tests for the query language a note writes in a block fenced as @tasks@.
--
-- Two halves, which are the two halves of the module: what a line of a query is read as, and which
-- tasks a query read that way finds. They are tested apart because they fail apart — a query which
-- finds the wrong tasks was either read wrongly or answered wrongly, and only one of those is about
-- the language.
--
-- Every relative date is read against 'today' below, which is the day 'Semantics.testCtssConfig'
-- pins so that a query written in a fixture asks one thing forever. It is a Saturday, so a
-- @last friday@ is yesterday and a @next monday@ is two days off.
module Syntax.ObsidianQueryTests where

import           CRes                     (CRes (..), prettyCNotes, runCResT)
import           CtService                (CTSS, ctssBaseFiles, ctssKeys,
                                           getEvalAnnots)
import           Data.Aeson               (FromJSON (..), Value (..), decode,
                                           withObject, (.:), (.:?))
import qualified Data.Aeson.KeyMap        as KM
import qualified Data.HashMap.Strict      as H
import           Data.List                (isInfixOf)
import           Data.Maybe               (listToMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           Data.Time.Calendar       (Day, fromGregorian)
import           Eval.Common              (Val (..))
import           Semantics                (testCtssConfig)
import           Semantics.Prgm           (FileImport, impDisp)
import           Syntax.Obsidian.Query
import           Syntax.Obsidian.Settings (StatusKind (..))
import           Syntax.Obsidian.Task     (TaskPriority (..))
import           Syntax.RereadTests       (caseDir)
import           System.Directory         (createDirectoryIfMissing,
                                           makeAbsolute)
import           System.FilePath          ((</>))
import           Test.Tasty
import           Test.Tasty.HUnit         (assertEqual, assertFailure, testCase)

-- | The day every query below is read against, which is the day every test of the compiler is read
-- against.
today :: Day
today = fromGregorian 2026 1 3

-- | A date written the way a task writes one.
date :: Int -> Day
date = fromGregorian 2026 1

-- | The one filter a query says, or a failure naming what came back instead.
filterOf :: String -> IO Filter
filterOf src = case parseQuery today src of
  Right q | Just f <- tqFilter q -> return f
  Right _ -> assertFailure $ "the query said no filter: " ++ show src
  Left bad -> assertFailure $ unlines bad

-- | What a line of a query is read as.
readAs :: String -> String -> Filter -> TestTree
readAs name src expected = testCase name $ assertEqual "" expected =<< filterOf src

-- | A line nothing reads, which is answered as the line it is rather than passed over.
readsAsNothing :: String -> String -> TestTree
readsAsNothing name src = testCase name $ case parseQuery today src of
  Left bad -> assertEqual "" [notUnderstood src] bad
  Right q  -> assertFailure $ "the line was read as " ++ show q

-- ---------------------------------------------------------------------------------------------
-- Reading a query
-- ---------------------------------------------------------------------------------------------

testStatusFilters :: TestTree
testStatusFilters = testGroup "a query asks what a task's box says" [
  readAs "done" "done" (FDone True),
  readAs "not done" "not done" (FDone False),
  readAs "a kind of status" "status.type is IN_PROGRESS" (FStatusType True KInProgress),
  readAs "a kind of status it is not" "status.type is not DONE" (FStatusType False KDone),
  readAs "what the vault calls the status" "status.name includes Hold" (FStatusName True "Hold"),
  readAs "what the vault does not call it" "status.name does not include Hold" (FStatusName False "Hold"),
  readsAsNothing "a kind of status there is none of" "status.type is BEGUN"
                                                                    ]

testDateFilters :: TestTree
testDateFilters = testGroup "a query asks where a task's dates fall" [
  readAs "on a day" "due on 2026-01-04" (FDate DDue (DateOn (only (date 4)))),
  readAs "on a day, with the word left out" "due 2026-01-04" (FDate DDue (DateOn (only (date 4)))),
  readAs "before a day" "due before 2026-01-04" (FDate DDue (DateBefore (only (date 4)))),
  readAs "after a day" "due after 2026-01-04" (FDate DDue (DateAfter (only (date 4)))),
  readAs "on or before a day" "due on or before 2026-01-04" (FDate DDue (DateOnOrBefore (only (date 4)))),
  readAs "on or after a day" "due on or after 2026-01-04" (FDate DDue (DateOnOrAfter (only (date 4)))),
  readAs "when a task starts" "starts before 2026-01-04" (FDate DStart (DateBefore (only (date 4)))),
  readAs "when a task is scheduled" "scheduled on 2026-01-04" (FDate DScheduled (DateOn (only (date 4)))),
  readAs "when a task was created" "created on 2026-01-04" (FDate DCreated (DateOn (only (date 4)))),
  readAs "when a task was finished" "done on 2026-01-04" (FDate DDone (DateOn (only (date 4)))),
  readAs "when a task was dropped" "cancelled on 2026-01-04" (FDate DCancelled (DateOn (only (date 4)))),
  readAs "when a task happens" "happens before 2026-01-04" (FDate DHappens (DateBefore (only (date 4)))),
  readAs "a task with such a date" "has due date" (FHasDate True DDue),
  readAs "a task with no such date" "no due date" (FHasDate False DDue),
  readAs "a task with no day it starts" "no start date" (FHasDate False DStart),
  readsAsNothing "a date nobody could read" "due before soonish"
                                                                     ]
  where only d = DateRange d d

testRelativeDates :: TestTree
testRelativeDates = testGroup "a relative date is read against the day the query was read on" [
  resolves "today" "today" (date 3) (date 3),
  resolves "tomorrow" "tomorrow" (date 4) (date 4),
  resolves "yesterday" "yesterday" (date 2) (date 2),
  resolves "the next of a day of the week" "next monday" (date 5) (date 5),
  resolves "the last of a day of the week" "last friday" (date 2) (date 2),
  -- Written on a Saturday, the nearest Sunday is tomorrow and the nearest Friday is yesterday.
  resolves "the nearest day of the week ahead" "sunday" (date 4) (date 4),
  resolves "the nearest day of the week behind" "friday" (date 2) (date 2),
  resolves "the day of the week it is" "saturday" (date 3) (date 3),
  resolves "this week" "this week" (fromGregorian 2025 12 29) (date 4),
  resolves "last week" "last week" (fromGregorian 2025 12 22) (fromGregorian 2025 12 28),
  resolves "next week" "next week" (date 5) (date 11),
  resolves "this month" "this month" (date 1) (date 31),
  resolves "last month" "last month" (fromGregorian 2025 12 1) (fromGregorian 2025 12 31),
  resolves "this quarter" "this quarter" (date 1) (fromGregorian 2026 3 31),
  resolves "this year" "this year" (date 1) (fromGregorian 2026 12 31),
  resolves "two days written out" "2026-01-04 2026-01-06" (date 4) (date 6)
                                                                                             ]
  where
    resolves name written from to = testCase name $
      assertEqual "" (Just (DateRange from to)) (resolveRange today written)

testOtherFilters :: TestTree
testOtherFilters = testGroup "a query asks what else a task is" [
  readAs "how urgent it is" "priority is high" (FPriority PrioIs PHigh),
  readAs "how urgent it is not" "priority is not high" (FPriority PrioIsNot PHigh),
  readAs "more urgent than" "priority is above low" (FPriority PrioAbove PLow),
  readAs "less urgent than" "priority is below medium" (FPriority PrioBelow PMedium),
  readAs "that it comes round again" "is recurring" (FRecurring True),
  readAs "that it does not" "is not recurring" (FRecurring False),
  readAs "what it says" "description includes report" (FDescription True "report"),
  readAs "what it does not say" "description does not include report" (FDescription False "report"),
  readAs "a tag it wrote" "tags include #work" (FTags True "#work"),
  readAs "a tag it wrote, written in the singular" "tag includes #work" (FTags True "#work"),
  readAs "a tag it did not write" "tags do not include #work" (FTags False "#work"),
  readAs "that it wrote any" "has tags" (FHasTags True),
  readAs "that it wrote none" "no tags" (FHasTags False),
  readAs "where it was written" "path includes Projects" (FPath PathWhole True "Projects"),
  readAs "the folder it was written in" "folder includes Projects" (FPath PathFolder True "Projects"),
  readAs "the file it was written in" "filename includes Plan" (FPath PathFilename True "Plan"),
  readAs "the first folder of its path" "root includes Work" (FPath PathRoot True "Work"),
  readAs "that it was written at the note's own level" "exclude sub-items" FTopLevel,
  readsAsNothing "a search for nothing" "description includes"
                                                                ]

testBooleanFilters :: TestTree
testBooleanFilters = testGroup "a line joining what a task must be" [
  readAs "two things it must be" "(done) AND (is recurring)" (FAnd (FDone True) (FRecurring True)),
  readAs "either of two" "(done) OR (is recurring)" (FOr (FDone True) (FRecurring True)),
  readAs "one of two and not both" "(done) XOR (is recurring)" (FXor (FDone True) (FRecurring True)),
  readAs "something it must not be" "NOT (done)" (FNot (FDone True)),
  readAs "one thing and not another" "(done) AND NOT (is recurring)" (FAnd (FDone True) (FNot (FRecurring True))),
  readAs "either of two, the second negated" "(done) OR NOT (is recurring)" (FOr (FDone True) (FNot (FRecurring True))),
  readAs "written between square brackets" "[done] AND [is recurring]" (FAnd (FDone True) (FRecurring True)),
  readAs "written between braces" "{done} AND {is recurring}" (FAnd (FDone True) (FRecurring True)),
  readAs "written between quotes" "\"done\" AND \"is recurring\"" (FAnd (FDone True) (FRecurring True)),
  -- And is read before or, so a line saying both is read as either of two rather than as both of
  -- two, which is where a reader who wrote no brackets would want the line to break.
  readAs "three things, and binding tighter than or"
    "(done) AND (is recurring) OR (has tags)"
    (FOr (FAnd (FDone True) (FRecurring True)) (FHasTags True)),
  readAs "three things joined the same way, read left to right"
    "(done) AND (is recurring) AND (has tags)"
    (FAnd (FAnd (FDone True) (FRecurring True)) (FHasTags True)),
  readAs "a joining written inside a joining"
    "(done) AND ((is recurring) OR (has tags))"
    (FAnd (FDone True) (FOr (FRecurring True) (FHasTags True))),
  -- The word is a word of the language only where it is written as a word, in capitals. A search
  -- for something with the letters in it is a search.
  readAs "a search holding the letters of a joining word"
    "description includes stand OR sit"
    (FDescription True "stand OR sit"),
  readsAsNothing "a joining of something nothing reads" "(done) AND (soonish)"
                                                                    ]

testWholeQuery :: TestTree
testWholeQuery = testGroup "a query of several lines" [
  testCase "every line saying what a task must be is joined by and" $ do
    q <- parsed "not done\nhas due date"
    assertEqual "" (Just (FAnd (FDone False) (FHasDate True DDue))) (tqFilter q),

  testCase "the orders are kept in the order they were written" $ do
    q <- parsed "sort by due\nsort by description reverse"
    assertEqual "" [SortKey (SortDate DDue) False, SortKey SortDescription True] (tqSort q),

  testCase "a count is read however it is written" $ do
    plain <- parsed "limit 10"
    spelt <- parsed "limit to 10 tasks"
    assertEqual "" (Just 10) (tqLimit plain)
    assertEqual "" (tqLimit plain) (tqLimit spelt),

  testCase "a query saying nothing asks for every task" $ do
    q <- parsed ""
    assertEqual "" emptyQuery q,

  testCase "a line saying nothing, and a line of comment, are passed over" $ do
    q <- parsed "\n# what is left to do\nnot done\n"
    assertEqual "" (Just (FDone False)) (tqFilter q),

  testCase "every line nothing reads is answered, not the first of them" $
    assertEqual "" (Left [notUnderstood "soonish", notUnderstood "sort by urgency"])
      (parseQuery today "not done\nsoonish\nsort by urgency"),

  testCase "each order a query does not read is answered" $
    assertEqual "" (Left [notUnderstood "sort by random"]) (parseQuery today "sort by random")
                                                      ]
  where
    parsed src = either (assertFailure . unlines) return $ parseQuery today src

-- ---------------------------------------------------------------------------------------------
-- Answering a query
-- ---------------------------------------------------------------------------------------------

-- | A task saying nothing, which each case below fills in what it is about.
task :: String -> QueryTask
task says = QueryTask {
  qtStatusKind = KTodo,
  qtStatusName = "Todo",
  qtDescription = says,
  qtTags = [],
  qtPriority = PNone,
  qtDue = Nothing,
  qtScheduled = Nothing,
  qtStart = Nothing,
  qtCreated = Nothing,
  qtDone = Nothing,
  qtCancelled = Nothing,
  qtRecurring = False,
  qtPath = "Notes/Plan.md",
  qtSubItem = False
                      }

-- | The tasks each case below is asked about, named by what each says.
tasks :: [QueryTask]
tasks = [
  (task "read it"){qtDue = Just (date 5), qtPriority = PHigh, qtTags = ["#work"]},
  (task "write it"){qtStatusKind = KDone, qtStatusName = "Done", qtDone = Just (date 2)},
  (task "plan it"){qtStatusKind = KInProgress, qtStatusName = "In Progress", qtDue = Just (date 2), qtRecurring = True},
  (task "drop it"){qtStatusKind = KCancelled, qtStatusName = "Cancelled", qtPath = "Work/Old.md"},
  (task "ask about it"){qtPriority = PLow, qtSubItem = True, qtTags = ["#home", "#work"]}
        ]

-- | What a query finds among those tasks, by what each found task says.
found :: String -> IO [String]
found src = case parseQuery today src of
  Left bad -> assertFailure $ unlines bad
  Right q  -> return $ map (qtDescription . fst) $ applyQuery q [(t, ()) | t <- tasks]

finds :: String -> String -> [String] -> TestTree
finds name src expected = testCase name $ assertEqual "" expected =<< found src

testFinding :: TestTree
testFinding = testGroup "which tasks a query finds" [
  finds "every task, for a query saying nothing" "" ["read it", "write it", "plan it", "drop it", "ask about it"],
  -- Finished and dropped are both done; begun and to do are both not.
  finds "the tasks which are done" "done" ["write it", "drop it"],
  finds "the tasks which are not" "not done" ["read it", "plan it", "ask about it"],
  finds "the tasks of one kind of status" "status.type is IN_PROGRESS" ["plan it"],
  finds "the tasks the vault calls something" "status.name includes progress" ["plan it"],
  finds "the tasks due before a day" "due before 2026-01-05" ["plan it"],
  finds "the tasks due on or after a day" "due on or after 2026-01-05" ["read it"],
  finds "the tasks due within a span" "due in 2026-01-01 2026-01-03" ["plan it"],
  finds "the tasks with a date at all" "has due date" ["read it", "plan it"],
  finds "the tasks with none" "no due date" ["write it", "drop it", "ask about it"],
  finds "the tasks more urgent than one priority" "priority is above none" ["read it"],
  finds "the tasks less urgent than one" "priority is below none" ["ask about it"],
  finds "the tasks which come round again" "is recurring" ["plan it"],
  finds "the tasks saying something" "description includes it" ["read it", "write it", "plan it", "drop it", "ask about it"],
  finds "the tasks not saying something" "description does not include plan" ["read it", "write it", "drop it", "ask about it"],
  finds "the tasks written under a tag" "tags include #work" ["read it", "ask about it"],
  finds "the tasks written under any" "has tags" ["read it", "ask about it"],
  finds "the tasks written in a folder" "folder includes Work" ["drop it"],
  finds "the tasks written in a file" "filename includes Plan" ["read it", "write it", "plan it", "ask about it"],
  finds "the tasks written at the note's own level" "exclude sub-items" ["read it", "write it", "plan it", "drop it"],
  -- The search is for what is written anywhere within, however it was capitalised.
  finds "a search which does not match how the task was written" "description includes IT" ["read it", "write it", "plan it", "drop it", "ask about it"],
  finds "the tasks which are two things" "(not done) AND (has due date)" ["read it", "plan it"],
  finds "the tasks which are either of two" "(is recurring) OR (has tags)" ["read it", "plan it", "ask about it"],
  finds "the tasks which are not something" "NOT (has tags)" ["write it", "plan it", "drop it"],
  -- "read it" and "ask about it" are both, "write it" and "drop it" are neither.
  finds "the tasks which are one of two and not both" "(not done) XOR (has tags)" ["plan it"]
                                                   ]

testOrdering :: TestTree
testOrdering = testGroup "the order a query gives what it finds" [
  -- A task with no such date sorts after every task that has one, and reversing turns the tasks
  -- that have one around and moves the ones that do not to the front. Tasks the key says nothing
  -- about are still left in the order they came in, reversed or not, which is what a stable order
  -- means and is what leaves a second key to decide between them.
  finds "by a date" "sort by due" ["plan it", "read it", "write it", "drop it", "ask about it"],
  finds "by a date, turned around" "sort by due reverse" ["write it", "drop it", "ask about it", "read it", "plan it"],
  finds "by what a task says" "sort by description" ["ask about it", "drop it", "plan it", "read it", "write it"],
  finds "by how urgent a task is" "sort by priority" ["read it", "write it", "plan it", "drop it", "ask about it"],
  -- What is being done, then what is to be done, then what is finished, then what was dropped.
  finds "by the kind of status" "sort by status.type" ["plan it", "read it", "ask about it", "write it", "drop it"],
  finds "by whether a task is done" "sort by status" ["read it", "plan it", "ask about it", "write it", "drop it"],
  finds "by whether it comes round again" "sort by recurring" ["plan it", "read it", "write it", "drop it", "ask about it"],
  finds "by where a task was written" "sort by path" ["read it", "write it", "plan it", "ask about it", "drop it"],
  finds "by a tag a task wrote" "sort by tag" ["ask about it", "read it", "write it", "plan it", "drop it"],
  -- The first key decides and the rest break its ties: every task here is filed under one status
  -- but the two done ones are ordered by what they say.
  finds "by one key and then another" "sort by status\nsort by description"
    ["ask about it", "plan it", "read it", "drop it", "write it"],
  -- The count is of what was found and ordered, so a query asking for the two soonest gets the
  -- soonest two rather than two of them sorted.
  finds "cut to the count asked for, after the order" "sort by due\nlimit 2" ["plan it", "read it"],
  finds "cut with no order asked for" "limit 2" ["read it", "write it"]
                                                                 ]

-- ---------------------------------------------------------------------------------------------
-- Answering the query a note wrote
-- ---------------------------------------------------------------------------------------------

-- | What the display of a note's query carries, read back out of the JSON it carries it in.
data Answered = Answered { answeredError :: Maybe String, answeredTasks :: [Value] }

instance FromJSON Answered where
  parseJSON = withObject "Answered" $ \o -> Answered <$> o .:? "qrError" <*> o .: "qrTasks"

-- | The notes the cases below ask about, and the vault written for the one of them that needs a
-- query nobody can read.
--
-- One service holding all of them rather than one per case. What a case here costs is compiling the
-- standard library, which every note needs and which the service does once.
queryNote, simpleNote :: FilePath
queryNote = "test/Integration/code/artifacts/catln-tasks/Query.md"
simpleNote = "test/Integration/code/artifacts/catln-test/Tasks.md"

answeringNotes :: IO (CTSS, FilePath)
answeringNotes = do
  dir <- caseDir "obsidian-query-bad"
  createDirectoryIfMissing True (dir </> ".obsidian")
  writeFile (dir </> ".obsidian" </> "app.json") "{}"
  writeFile (dir </> "Bad.md") $ unlines ["```tasks", "not done", "soonish", "```"]
  ctss <- ctssBaseFiles testCtssConfig [queryNote, simpleNote, dir </> "Bad.md"]
  return (ctss, dir </> "Bad.md")

-- | The query a note wrote, answered by the compiler the way a doc server asks for it.
--
-- The whole of the way: the vault is read off disk, its notes are compiled, the annotation the query
-- was read as is evaluated, and the value it displays as is what comes back. Nothing else runs this
-- — the golden tests of a vault evaluate the annotations of the file a test names and not of the
-- notes it reaches, which for a query is every note there is.
answeredBy :: CTSS -> FilePath -> IO Answered
answeredBy ctss note = do
  page <- pageAt note ctss
  annots <- runCResT $ getEvalAnnots ctss page
  case annots of
    CErr notes -> assertFailure $ "the note did not compile: " ++ prettyCNotes notes
    CRes _ vals -> case [args | (_, TupleVal name args) <- vals, name == "/Data/Note/Show/TaskQuery"] of
      [args] -> case H.lookup "/result" args of
        Just (StrVal json) -> maybe (assertFailure $ "the answer is not JSON: " ++ json) return $
          decode (TLE.encodeUtf8 (TL.pack json))
        carried -> assertFailure $ "the display carries " ++ show carried ++ " rather than a result"
      displays -> assertFailure $ "the note displays " ++ show (length displays) ++ " queries"

-- | The page a service holds at a path.
--
-- Asked for by the path it resolves to rather than the path it was written as, since that is what
-- the service names its pages by.
pageAt :: FilePath -> CTSS -> IO FileImport
pageAt path ctss = do
  pages <- ctssKeys ctss
  resolved <- makeAbsolute path
  case [page | page <- pages, impDisp page `elem` [Just path, Just resolved]] of
    [page] -> return page
    held   -> assertFailure $ "the service holds " ++ show (length held) ++ " pages shown at " ++ path

-- | What a task the answer carries says, which is what a reader of the block sees beside its box.
saysOf :: Value -> Maybe String
saysOf v = case v of
  Object o | Just (Object shown) <- KM.lookup "ptTask" o
           , Just (Object args) <- KM.lookup "args" shown
           -> firstString [KM.lookup k args | k <- ["/description", "/contents"]]
  _ -> Nothing
  where
    firstString vs = listToMaybe [T.unpack s | Just (Object arg) <- vs, Just (String s) <- [KM.lookup "contents" arg]]

-- | Which note of the vault a task the answer carries came from.
pageOf :: Value -> Maybe String
pageOf (Object o) | Just (String p) <- KM.lookup "ptPage" o = Just (T.unpack p)
pageOf _ = Nothing

testAnswering :: TestTree
testAnswering = withResource answeringNotes (const $ return ()) $ \held ->
  testGroup "the query a note wrote, answered by the compiler" [

  testCase "over the tasks of the vault the note is in" $ do
    (ctss, _) <- held
    answer <- answeredBy ctss queryNote
    assertEqual "the query was not understood" Nothing (answeredError answer)
    -- The query is @not done@, ordered by how urgent each task is and cut to three. Ship it is the
    -- one task of the vault anybody gave a priority to, so it comes first and the rest follow in
    -- the order the note wrote them.
    assertEqual "the query did not find what the vault says"
      [Just "Ship it #work", Just "Read the note", Just "Plan the rest"]
      (map saysOf (answeredTasks answer)),

  testCase "over that vault and no other" $ do
    (ctss, _) <- held
    answer <- answeredBy ctss simpleNote
    assertEqual "the query was not understood" Nothing (answeredError answer)
    -- A vault running no plugin holds checkboxes all the same, and a query written in one finds
    -- them. The service holds the vault beside it as well, whose tasks this must not reach.
    assertEqual "the query did not find the checkboxes of a vault running no plugin"
      [Just "Read [[Welcome]]", Just "Plan the rest", Just "A checkbox indented deeper is a task of its own"]
      (map saysOf (answeredTasks answer))
    assertEqual "the query reached out of the vault it was written in"
      [] [p | Just p <- map pageOf (answeredTasks answer), not ("catln-test/" `isInfixOf` p)],

  testCase "saying which line of it was not understood, where it was written" $ do
    (ctss, bad) <- held
    answer <- answeredBy ctss bad
    assertEqual "the line the reader wrote was not named back to them"
      (Just (notUnderstood "soonish")) (answeredError answer)
    assertEqual "a query nobody can read found something" [] (map saysOf (answeredTasks answer))
                                                              ]

obsidianQueryTests :: TestTree
obsidianQueryTests = testGroup "ObsidianQueryTests" [
  testStatusFilters,
  testDateFilters,
  testRelativeDates,
  testOtherFilters,
  testBooleanFilters,
  testWholeQuery,
  testFinding,
  testOrdering,
  testAnswering
                                                    ]
