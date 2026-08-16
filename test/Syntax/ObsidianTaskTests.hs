-- | Tests for reading a vault's Obsidian Tasks settings, and for reading a checkbox line by them.
--
-- Both are covered here rather than through a vault of notes, because both are decided by a vault's
-- configuration and a fixture vault can only be set one way. What a note of a vault running the
-- plugin is read as is held by the goldens of @test\/Integration\/code\/artifacts\/catln-tasks@;
-- what a vault set some OTHER way would read as — a global filter, the @dataview@ notation, a
-- status the vault named itself — has no fixture and is covered here.
module Syntax.ObsidianTaskTests where

import           Syntax.Obsidian.Settings (StatusKind (..), TaskFormat (..),
                                           TaskSettings (..), TaskStatus (..),
                                           defaultTaskSettings, pluginDataFile,
                                           readTaskSettings, statusFor,
                                           tasksPluginId)
import           Syntax.Obsidian.Task     (ObsidianTask (..), TaskPriority (..),
                                           obsidianTask)
import           System.Directory         (createDirectoryIfMissing,
                                           getTemporaryDirectory,
                                           removePathForcibly)
import           System.FilePath          (takeDirectory, (</>))
import           Test.Tasty
import           Test.Tasty.HUnit

-- | A vault written for one case, emptied first.
caseVault :: String -> IO FilePath
caseVault name = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-obsidian-tasks" </> name
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> ".obsidian")
  return dir

-- | Turn a plugin on, as a vault does: by listing it among the ones it has turned on.
enablePlugin :: FilePath -> String -> IO ()
enablePlugin vault plugin = writeFile (vault </> ".obsidian" </> "community-plugins.json") $
  "[\"" ++ plugin ++ "\"]"

-- | Give the Tasks plugin something to be set to.
writePluginData :: FilePath -> String -> IO ()
writePluginData vault contents = do
  let path = pluginDataFile vault tasksPluginId
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path contents

-- | A vault which has turned no community plugin on has no such file at all, and a vault reading as
-- though it ran the plugin would read every checkbox in it as something it is not.
testNoPluginsAtAll :: TestTree
testNoPluginsAtAll = testCase "a vault with no community plugins runs no Tasks plugin" $ do
  vault <- caseVault "no-plugins"
  settings <- readTaskSettings vault
  assertEqual "" Nothing settings

-- | A vault may run community plugins and not this one.
testOtherPluginOnly :: TestTree
testOtherPluginOnly = testCase "a vault running other plugins runs no Tasks plugin" $ do
  vault <- caseVault "other-plugin"
  enablePlugin vault "dataview"
  settings <- readTaskSettings vault
  assertEqual "" Nothing settings

-- | The plugin is on, and has never been given a setting to keep. It is running its own defaults,
-- which is a vault to read tasks in and not a vault we cannot read.
testEnabledWithoutData :: TestTree
testEnabledWithoutData = testCase "a plugin turned on and never configured runs its defaults" $ do
  vault <- caseVault "enabled-default"
  enablePlugin vault tasksPluginId
  settings <- readTaskSettings vault
  assertEqual "" (Just defaultTaskSettings) settings

-- | What a vault does say is read, and what it does not say is left at the plugin's default. The
-- plugin lays a vault's settings over its defaults rather than requiring a whole file, so a file
-- naming one key is a vault which has changed one thing.
testPartialData :: TestTree
testPartialData = testCase "a setting a vault does not name is the plugin's default" $ do
  vault <- caseVault "partial-data"
  enablePlugin vault tasksPluginId
  writePluginData vault "{\"globalFilter\": \"#task\"}"
  settings <- readTaskSettings vault
  assertEqual "the filter is read" (Just "#task") (tstGlobalFilter <$> settings)
  assertEqual "the notation is left alone" (Just TasksEmoji) (tstFormat <$> settings)
  assertEqual "the statuses are left alone" (Just $ tstStatuses defaultTaskSettings) (tstStatuses <$> settings)

-- | The other notation, which decides how every line of the vault is read.
testDataviewFormat :: TestTree
testDataviewFormat = testCase "a vault may be set to the dataview notation" $ do
  vault <- caseVault "dataview-format"
  enablePlugin vault tasksPluginId
  writePluginData vault "{\"taskFormat\": \"dataview\"}"
  settings <- readTaskSettings vault
  assertEqual "" (Just Dataview) (tstFormat <$> settings)

-- | A vault names its own statuses, and they are read alongside the plugin's own rather than
-- instead of them.
testCustomStatuses :: TestTree
testCustomStatuses = testCase "a vault's own statuses are read beside the plugin's" $ do
  vault <- caseVault "custom-statuses"
  enablePlugin vault tasksPluginId
  writePluginData vault $ unlines [
    "{\"statusSettings\": {",
    "  \"coreStatuses\": [",
    "    {\"symbol\": \" \", \"name\": \"Todo\", \"nextStatusSymbol\": \"x\", \"availableAsCommand\": true, \"type\": \"TODO\"},",
    "    {\"symbol\": \"x\", \"name\": \"Done\", \"nextStatusSymbol\": \" \", \"availableAsCommand\": true, \"type\": \"DONE\"}],",
    "  \"customStatuses\": [",
    "    {\"symbol\": \"?\", \"name\": \"Question\", \"nextStatusSymbol\": \"x\", \"availableAsCommand\": true, \"type\": \"ON_HOLD\"}]}}"
                                  ]
  settings <- readTaskSettings vault
  assertEqual "" (Just [
    TaskStatus " " "Todo" KTodo,
    TaskStatus "x" "Done" KDone,
    TaskStatus "?" "Question" KOnHold
                       ]) (tstStatuses <$> settings)

-- | A symbol the vault names no status for is still a box, and a box is still something to do.
testUnknownStatusSymbol :: TestTree
testUnknownStatusSymbol = testCase "a symbol the vault does not name is read as something to do" $ do
  assertEqual "a status the vault names" (TaskStatus "/" "In Progress" KInProgress) (statusFor defaultTaskSettings "/")
  assertEqual "one it does not" (TaskStatus "!" "!" KTodo) (statusFor defaultTaskSettings "!")

-- | Settings written to be read by, rather than read from a vault, so that a case says what it is
-- about on its own line.
emoji, dataview :: TaskSettings
emoji = defaultTaskSettings
dataview = defaultTaskSettings{tstFormat = Dataview}

filtered :: TaskSettings
filtered = defaultTaskSettings{tstGlobalFilter = "#task"}

-- | The task a line is, where reading it is meant to answer.
readsAs :: TaskSettings -> String -> ObsidianTask
readsAs settings line = case obsidianTask settings line of
  Just task -> task
  Nothing   -> error $ "The line was read as no task at all: " ++ line

testPlainLine :: TestTree
testPlainLine = testCase "a line carrying nothing says all of itself" $ do
  let task = readsAs emoji "Read the note"
  assertEqual "" "Read the note" (otDescription task)
  assertEqual "" Nothing (otDue task)
  assertEqual "" PNone (otPriority task)
  assertEqual "the line itself is kept" "Read the note" (otContents task)

-- | Every date the plugin writes, and every mark it writes each of them after. A vault may write a
-- scheduled date after either hourglass and a due date after any of three calendars, so a reading
-- which knew one of each would read the others as writing.
testEveryDate :: TestTree
testEveryDate = testCase "every date is read, after each mark it may be written with" $ do
  assertEqual "created" (Just "2026-01-01") (otCreated $ readsAs emoji "Do it \x2795 2026-01-01")
  assertEqual "start" (Just "2026-01-02") (otStart $ readsAs emoji "Do it \x1F6EB 2026-01-02")
  assertEqual "scheduled" (Just "2026-01-03") (otScheduled $ readsAs emoji "Do it \x23F3 2026-01-03")
  assertEqual "scheduled, the other hourglass" (Just "2026-01-03") (otScheduled $ readsAs emoji "Do it \x231B 2026-01-03")
  assertEqual "due" (Just "2026-01-04") (otDue $ readsAs emoji "Do it \x1F4C5 2026-01-04")
  assertEqual "due, the tear-off calendar" (Just "2026-01-04") (otDue $ readsAs emoji "Do it \x1F4C6 2026-01-04")
  assertEqual "due, the spiral calendar" (Just "2026-01-04") (otDue $ readsAs emoji "Do it \x1F5D3 2026-01-04")
  assertEqual "done" (Just "2026-01-05") (otDone $ readsAs emoji "Do it \x2705 2026-01-05")
  assertEqual "cancelled" (Just "2026-01-06") (otCancelled $ readsAs emoji "Do it \x274C 2026-01-06")

-- | A mark written with a variation selector is the same mark. Which of the two a vault writes is a
-- difference in the bytes and in nothing that reads them.
testVariationSelector :: TestTree
testVariationSelector = testCase "a mark written with a variation selector is that mark" $ do
  let task = readsAs emoji "Do it \x1F5D3\xFE0F 2026-01-04"
  assertEqual "" (Just "2026-01-04") (otDue task)
  assertEqual "" "Do it" (otDescription task)

testEveryPriority :: TestTree
testEveryPriority = testCase "every priority is read, and a line with none has none" $ do
  assertEqual "highest" PHighest (otPriority $ readsAs emoji "Do it \x1F53A")
  assertEqual "high" PHigh (otPriority $ readsAs emoji "Do it \x23EB")
  assertEqual "medium" PMedium (otPriority $ readsAs emoji "Do it \x1F53C")
  assertEqual "low" PLow (otPriority $ readsAs emoji "Do it \x1F53D")
  assertEqual "lowest" PLowest (otPriority $ readsAs emoji "Do it \x23EC")
  assertEqual "none" PNone (otPriority $ readsAs emoji "Do it")

testRecurrenceAndCompletion :: TestTree
testRecurrenceAndCompletion = testCase "a recurrence and an action on completion are read" $ do
  let task = readsAs emoji "Water the plants \x1F501 every day when done \x1F3C1 delete"
  assertEqual "" (Just "every day when done") (otRecurrence task)
  assertEqual "" (Just "delete") (otOnCompletion task)
  assertEqual "" "Water the plants" (otDescription task)

testIdAndDependsOn :: TestTree
testIdAndDependsOn = testCase "an id and the ids a task waits on are read" $ do
  assertEqual "an id" (Just "dcf64c") (otTaskId $ readsAs emoji "Do this first \x1F194 dcf64c")
  assertEqual "one it waits on" ["dcf64c"] (otDependsOn $ readsAs emoji "Do this after \x26D4 dcf64c")
  assertEqual "several" ["dcf64c", "0h17ye"] (otDependsOn $ readsAs emoji "Do this last \x26D4 dcf64c,0h17ye")
  assertEqual "several, written apart" ["dcf64c", "0h17ye"] (otDependsOn $ readsAs emoji "Do this last \x26D4 dcf64c, 0h17ye")

-- | The @^id@ a note gives a line so that a link may name the line rather than the note.
testBlockLink :: TestTree
testBlockLink = testCase "the id a note gives the line is read" $ do
  let task = readsAs emoji "Do it \x1F4C5 2026-01-04 ^abc-123"
  assertEqual "" (Just "abc-123") (otBlockLink task)
  assertEqual "" (Just "2026-01-04") (otDue task)
  assertEqual "" "Do it" (otDescription task)

-- | The tags stay in what the task says as well as being listed, which is what the plugin does with
-- them: a tag is writing of the task and a way of finding it, and taking it out of the writing would
-- leave the task saying less than the line said.
testTags :: TestTree
testTags = testCase "the tags are listed and stay in what the task says" $ do
  let task = readsAs emoji "Ship it #work #urgent \x1F4C5 2026-01-04"
  assertEqual "" ["#work", "#urgent"] (otTags task)
  assertEqual "" "Ship it #work #urgent" (otDescription task)

-- | The reason a line is read from its end. A mark with no date after it is not a date, and reading
-- the line for the first mark it holds would take a lunch appointment for one.
testMarkWithoutValue :: TestTree
testMarkWithoutValue = testCase "a mark with nothing it names after it is writing" $ do
  let task = readsAs emoji "Book \x1F4C5 lunch"
  assertEqual "" Nothing (otDue task)
  assertEqual "" "Book \x1F4C5 lunch" (otDescription task)

-- | And the same line with a real date after all of that, which has both.
testMarkWithoutValueThenValue :: TestTree
testMarkWithoutValueThenValue = testCase "a mark that names nothing is kept, and a later one is read" $ do
  let task = readsAs emoji "Book \x1F4C5 lunch \x1F4C5 2026-01-04"
  assertEqual "" (Just "2026-01-04") (otDue task)
  assertEqual "" "Book \x1F4C5 lunch" (otDescription task)

-- | The marks are taken off one at a time from the end, so the order a vault wrote them in is not
-- an order this has to know.
testFieldsInAnyOrder :: TestTree
testFieldsInAnyOrder = testCase "the marks are read whatever order they were written in" $ do
  let one = readsAs emoji "Do it \x1F4C5 2026-01-04 \x23EB"
  let other = readsAs emoji "Do it \x23EB \x1F4C5 2026-01-04"
  assertEqual "" (Just "2026-01-04") (otDue one)
  assertEqual "" PHigh (otPriority one)
  assertEqual "" "Do it" (otDescription one)
  assertEqual "" (Just "2026-01-04") (otDue other)
  assertEqual "" PHigh (otPriority other)
  assertEqual "" "Do it" (otDescription other)

testDataviewFields :: TestTree
testDataviewFields = testCase "the same fields are read in the dataview notation" $ do
  let task = readsAs dataview "Ship it [due:: 2026-01-04] [priority:: high] [repeat:: every week]"
  assertEqual "" (Just "2026-01-04") (otDue task)
  assertEqual "" PHigh (otPriority task)
  assertEqual "" (Just "every week") (otRecurrence task)
  assertEqual "" "Ship it" (otDescription task)

-- | The plugin reads an inline field written between parentheses as readily as between brackets,
-- and reads neither where the two do not match.
testDataviewParentheses :: TestTree
testDataviewParentheses = testCase "an inline field may be written between parentheses" $ do
  assertEqual "" (Just "2026-01-04") (otDue $ readsAs dataview "Ship it (due:: 2026-01-04)")
  assertEqual "" (Just "abc") (otTaskId $ readsAs dataview "Ship it (id:: abc)")

-- | A vault set to one notation does not read the other, which is the whole reason the setting is
-- read: the marks of the notation a vault is not set to are writing of the note.
testNotationsDoNotCross :: TestTree
testNotationsDoNotCross = testCase "the notation a vault is not set to is writing" $ do
  assertEqual "emoji read by a dataview vault" Nothing (otDue $ readsAs dataview "Ship it \x1F4C5 2026-01-04")
  assertEqual "dataview read by an emoji vault" Nothing (otDue $ readsAs emoji "Ship it [due:: 2026-01-04]")

-- | Where a vault asks that every task carry a word, a checkbox not carrying it is no task of the
-- plugin, and is answered for as nothing rather than as a task saying nothing.
testGlobalFilter :: TestTree
testGlobalFilter = testCase "a line not carrying the vault's word is no task of the plugin" $ do
  assertEqual "carrying it" (Just "Ship it #task") (otDescription <$> obsidianTask filtered "Ship it #task")
  assertEqual "not carrying it" Nothing (obsidianTask filtered "Buy milk")

-- | Taken out of what the task says only where the vault asks for that, and the space it leaves
-- closed up behind it.
testRemoveGlobalFilter :: TestTree
testRemoveGlobalFilter = testCase "the vault's word is taken out of what the task says when asked" $ do
  let removed = filtered{tstRemoveGlobalFilter = True}
  assertEqual "" (Just "Ship it") (otDescription <$> obsidianTask removed "#task Ship it")
  assertEqual "" (Just "Ship it") (otDescription <$> obsidianTask removed "Ship it #task")
  assertEqual "left in otherwise" (Just "#task Ship it") (otDescription <$> obsidianTask filtered "#task Ship it")

-- | Whatever is read, the line itself is kept beside it exactly as it was written — which is what
-- the note is written back from.
testContentsKept :: TestTree
testContentsKept = testCase "the line is kept exactly as it was written" $ do
  let line = "Ship it #work \x1F4C5 2026-01-04 \x23EB ^abc"
  assertEqual "" line (otContents $ readsAs emoji line)

obsidianTaskTests :: TestTree
obsidianTaskTests = testGroup "ObsidianTask" [
  testGroup "Settings" [
    testNoPluginsAtAll,
    testOtherPluginOnly,
    testEnabledWithoutData,
    testPartialData,
    testDataviewFormat,
    testCustomStatuses,
    testUnknownStatusSymbol
                       ],
  testGroup "Reading a line" [
    testPlainLine,
    testEveryDate,
    testVariationSelector,
    testEveryPriority,
    testRecurrenceAndCompletion,
    testIdAndDependsOn,
    testBlockLink,
    testTags,
    testMarkWithoutValue,
    testMarkWithoutValueThenValue,
    testFieldsInAnyOrder,
    testDataviewFields,
    testDataviewParentheses,
    testNotationsDoNotCross,
    testGlobalFilter,
    testRemoveGlobalFilter,
    testContentsKept
                             ]
                                             ]
