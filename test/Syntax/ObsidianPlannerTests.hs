-- | Tests for reading a vault's Day Planner and Daily Notes settings, and for reading a line, a note's
-- name and a note's blocks by them.
--
-- Covered here rather than through a vault of notes for the reason 'Syntax.ObsidianTaskTests' gives:
-- a fixture vault is set one way, and what a vault set another way reads as — another daily note
-- format, a block extended until the next — has no fixture. What a note of the planner fixture vault
-- is read as is held by the goldens of @test\/Integration\/code\/artifacts\/catln-planner@.
module Syntax.ObsidianPlannerTests where

import           CtConstants              (falsePrim, noteSimpleTaskPath,
                                           noteTaskContents, noteTaskState)
import           Data.Time.Calendar       (Day, fromGregorian)
import           Semantics.Prgm           (ObjectBasis (..))
import           Semantics.Types          (ArgName, mkName, partialKey)
import           Syntax.Ct.Builder        (applyRawArgs, rawStr, rawVal)
import           Syntax.Ct.Prgm           (RawExpr, RawObjArr (..),
                                           RawPrgm (..), RawStatement (..),
                                           RawStatementTree (..))
import           Syntax.Obsidian.Parser   (NotePart (..), NotePlanner (..),
                                           NoteWriting (..), notePageStatement,
                                           noteParts, notePartsWith, placedPart,
                                           placedUnderHeading, plannedArgs,
                                           unparseNote, unparseNoteWith)
import           Syntax.Obsidian.Planner
import           Syntax.Obsidian.Settings (DailyNoteSettings (..),
                                           DateToken (..), PlannerSettings (..),
                                           TimeToken (..), dailyNotesFile,
                                           dateFormatTokens, dayPlannerPluginId,
                                           defaultDailyNoteFormat,
                                           defaultPlannerSettings,
                                           pluginDataFile, readPlannerSettings)
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
  let dir = tmp </> "catln-obsidian-planner" </> name
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> ".obsidian")
  return dir

-- | Turn the Day Planner on, as a vault does: by listing it among the plugins it has turned on.
enablePlanner :: FilePath -> IO ()
enablePlanner vault = writeFile (vault </> ".obsidian" </> "community-plugins.json") $ "[\"" ++ dayPlannerPluginId ++ "\"]"

writeFileIn :: FilePath -> String -> IO ()
writeFileIn path contents = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path contents

-- | A vault which has not turned the planner on plans nothing, however its daily notes are set.
testPlannerOff :: TestTree
testPlannerOff = testCase "a vault not running the Day Planner plans nothing" $ do
  vault <- caseVault "planner-off"
  writeFileIn (dailyNotesFile vault) "{\"folder\": \"Daily\"}"
  settings <- readPlannerSettings vault
  assertEqual "" Nothing settings

-- | Turned on and never configured, with no daily notes settings either: both plugins' defaults.
testPlannerDefaults :: TestTree
testPlannerDefaults = testCase "a planner turned on and never configured runs its defaults" $ do
  vault <- caseVault "planner-defaults"
  enablePlanner vault
  settings <- readPlannerSettings vault
  assertEqual "" (Just defaultPlannerSettings) settings

-- | What each file says is read, and what it leaves out is the default.
testPlannerSettingsRead :: TestTree
testPlannerSettingsRead = testCase "the planner's and the daily notes' settings are read" $ do
  vault <- caseVault "planner-settings"
  enablePlanner vault
  writeFileIn (pluginDataFile vault dayPlannerPluginId) "{\"defaultDurationMinutes\": 45, \"extendDurationUntilNext\": true}"
  writeFileIn (dailyNotesFile vault) "{\"folder\": \"/Journal/\", \"format\": \"YYYY/MM/DD\"}"
  settings <- readPlannerSettings vault
  assertEqual "the duration" (Just 45) (pstDefaultDuration <$> settings)
  assertEqual "extending" (Just True) (pstExtendUntilNext <$> settings)
  assertEqual "the folder, without its slashes" (Just "Journal") (dnsFolder . pstDailyNotes <$> settings)
  assertEqual "the format" (Just [DYear4, DLiteral "/", DMonth2, DLiteral "/", DDay2]) (dnsFormat . pstDailyNotes <$> settings)

-- | An empty format is the Daily Notes plugin saying it uses its default.
testEmptyFormat :: TestTree
testEmptyFormat = testCase "an empty daily note format is the default one" $ do
  vault <- caseVault "empty-format"
  enablePlanner vault
  writeFileIn (dailyNotesFile vault) "{\"format\": \"\"}"
  settings <- readPlannerSettings vault
  assertEqual "" (Just defaultDailyNoteFormat) (dnsFormat . pstDailyNotes <$> settings)

-- | The pieces of the formats a day's name is written in, and the ones that are not read.
testDateFormats :: TestTree
testDateFormats = testCase "a daily note format is read into its pieces" $ do
  assertEqual "bracketed text" (Right [DWeekdayShort, DLiteral ", ", DMonthName, DLiteral " ", DDayOrdinal, DLiteral " of ", DYear4]) (dateFormatTokens "ddd, MMMM Do [of] YYYY")
  assertEqual "a week of the year" True (isLeft $ dateFormatTokens "YYYY-[W]ww")
  assertEqual "a bracket never closed" True (isLeft $ dateFormatTokens "YYYY-[MM")
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | Which day a note is the daily note of, by its path from the daily notes folder.
testNoteDay :: TestTree
testNoteDay = testCase "a daily note names its day" $ do
  let daily = DailyNoteSettings "Daily" defaultDailyNoteFormat
  assertEqual "a daily note" (Just $ fromGregorian 2026 1 3) (noteDay daily "/v" "/v/Daily/2026-01-03.md")
  assertEqual "a note outside the folder" Nothing (noteDay daily "/v" "/v/2026-01-03.md")
  assertEqual "a note naming no day" Nothing (noteDay daily "/v" "/v/Daily/Ideas.md")
  assertEqual "a day that does not exist" Nothing (noteDay daily "/v" "/v/Daily/2026-02-30.md")
  let nested = DailyNoteSettings "" [DYear4, DLiteral "/", DMonthName, DLiteral "/", DDay1]
  assertEqual "a format writing folders" (Just $ fromGregorian 2026 3 7) (noteDay nested "/v" "/v/2026/March/7.md")
  assertEqual "a one-digit day followed by text" Nothing (noteDay nested "/v" "/v/2026/March/7th.md")

-- | The times the plugin's expression reads at the start of a line.
testTimeRanges :: TestTree
testTimeRanges = testCase "a time block is read from the start of a line" $ do
  assertEqual "a range" (Just (WrittenBlock (9 * 60) (Just (10 * 60 + 30)), "Write")) (timeRangeAtStart "09:00 - 10:30 Write")
  assertEqual "no spaces around the dash" (Just (WrittenBlock (9 * 60) (Just (10 * 60)), "Write")) (timeRangeAtStart "9.00-10.00 Write")
  assertEqual "a start alone" (Just (WrittenBlock (14 * 60) Nothing, "Call")) (timeRangeAtStart "14:00 Call")
  assertEqual "on a 12-hour clock" (Just (WrittenBlock (21 * 60 + 30) Nothing, "Read")) (timeRangeAtStart "9:30pm Read")
  assertEqual "without a colon, with a space" (Just (WrittenBlock (11 * 60 + 30) (Just (12 * 60)), "Lunch")) (timeRangeAtStart "1130 AM - 12:00 Lunch")
  assertEqual "midnight on a 12-hour clock" (Just (WrittenBlock 15 Nothing, "Late")) (timeRangeAtStart "12:15am Late")
  assertEqual "no time" Nothing (timeRangeAtStart "Write the report")
  assertEqual "a day is not a time" Nothing (timeRangeAtStart "2026-01-03 review")
  assertEqual "an hour past the day" Nothing (timeRangeAtStart "25:00 Never")
  assertEqual "a 12-hour clock has no thirteenth hour" Nothing (timeRangeAtStart "13:00pm Never")
  -- The 12-hour reading needs its am to end a word, so the expression falls through to its 24-hour
  -- reading, which takes the 9:30 and leaves the rest as what the line says.
  assertEqual "am must end a word" (Just (WrittenBlock (9 * 60 + 30) Nothing, "amble")) (timeRangeAtStart "9:30amble")

-- | The day a line elsewhere is scheduled for, in each way the plugin reads one.
testScheduledDay :: TestTree
testScheduledDay = testCase "a line's scheduled day is read" $ do
  assertEqual "the emoji" (Just $ fromGregorian 2021 8 29) (scheduledDay "08:00 - 10:00 Work \x23F3 2021-08-29")
  assertEqual "an inline field" (Just $ fromGregorian 2021 8 29) (scheduledDay "08:00 Work [scheduled:: 2021-08-29]")
  assertEqual "a keyless inline field" (Just $ fromGregorian 2021 8 29) (scheduledDay "08:00 Work (scheduled:: 2021-08-29)")
  assertEqual "none" Nothing (scheduledDay "08:00 Work \x1F4C5 2021-08-29")

-- | A block written without an end is given one by the vault's settings.
testResolveEnds :: TestTree
testResolveEnds = testCase "a block without an end lasts as the vault says" $ do
  let blocks = [WrittenBlock 600 Nothing, WrittenBlock 540 (Just 560), WrittenBlock 620 Nothing]
  assertEqual "by default" [(600, 630), (540, 560), (620, 650)] (resolveEnds defaultPlannerSettings blocks)
  assertEqual "until the next" [(600, 620), (540, 560), (620, 650)] (resolveEnds defaultPlannerSettings{pstExtendUntilNext = True} blocks)
  assertEqual "past midnight" [(1430, 20)] (resolveEnds defaultPlannerSettings{pstDefaultDuration = 30} [WrittenBlock 1430 Nothing])

-- | A note read by the planner is written back as it was written. The planner's reading is beside
-- the line rather than taken out of it, so the times and the scheduled day are still in the line
-- that is written back.
testPlannedNoteRoundTrip :: TestTree
testPlannedNoteRoundTrip = testCase "a note the planner read is written back as it was" $ do
  let note = unlines [
        "# Day planner",
        "",
        "- [ ] 09:00 - 10:30 Write the report",
        "- [x] 11:00 Stand up",
        "  - [ ] 1:30pm Nested",
        "- [ ] 10:00 Pick up the parcel [scheduled:: 2026-01-05]"
                     ]
  let planner = NotePlanner defaultPlannerSettings (Just $ fromGregorian 2026 1 3) []
  assertEqual "" (Just note) (unparseNote $ RawPrgm [] [notePageStatement Nothing (Just planner) (const Nothing) "2026-01-03" note])

-- | A bullet writing a time is an event to a vault running the planner, and is broken out of the
-- writing around it as a checkbox is, with what is written under it. A bullet writing no time, and a
-- checkbox whatever it writes, are not events; and a bullet nested under a task is an event of that
-- task, as a checkbox nested under it is a task of it.
testEventParts :: TestTree
testEventParts = testCase "a bullet writing a time is an event" $ do
  let note = unlines [
        "Before.",
        "- 12:00 - 13:00 Lunch",
        "  with the team",
        "- Not at any time",
        "- [ ] 14:00 A task",
        "  - 9:30pm Late call"
                     ]
  assertEqual "" [
    NoteWriting "Before.",
    NoteEvent "12:00 - 13:00 Lunch" [NoteWriting "with the team"],
    NoteWriting "- Not at any time",
    NoteTask " " "14:00 A task" [NoteEvent "9:30pm Late call" []]
                 ] (notePartsWith True note)

-- | A vault not running the planner has no events, so its bullets are the writing they were.
testNoEventsWithoutPlanner :: TestTree
testNoEventsWithoutPlanner = testCase "a vault not running the planner has no events" $
  assertEqual "" [NoteWriting "- 12:00 - 13:00 Lunch"] (noteParts "- 12:00 - 13:00 Lunch\n")

-- | A note whose events the planner read is written back as it was written: an event is written
-- back from its line, as a task is, with what is written under it.
testEventRoundTrip :: TestTree
testEventRoundTrip = testCase "a note with events is written back as it was" $ do
  let note = unlines [
        "# Day planner",
        "",
        "- 08:00 - 08:30 Breakfast",
        "- [ ] 09:00 - 10:30 Write the report",
        "  - 9:30pm Late call",
        "- 12:00 Lunch",
        "  with the team",
        "- Not at any time",
        "",
        "A paragraph after the list."
                     ]
  let planner = NotePlanner defaultPlannerSettings (Just $ fromGregorian 2026 1 3) []
  assertEqual "" (Just note) (unparseNote $ RawPrgm [] [notePageStatement Nothing (Just planner) (const Nothing) "2026-01-03" note])

-- | A line moved to another time of day is written with its new block in place of the one it wrote,
-- in front of it where it wrote none, and without one where it is to have none — always with its
-- end and in the vault's clock, as the plugin writes a block it moved.
testWithBlock :: TestTree
testWithBlock = testCase "a line is written with its new block" $ do
  let hhmm = pstTimestampFormat defaultPlannerSettings
  assertEqual "replaced" "14:00 - 14:45 Review the draft" (withBlock hhmm (Just (840, 885)) "1:30pm - 2:15pm Review the draft")
  assertEqual "given an end" "11:00 - 11:30 Stand up" (withBlock hhmm (Just (660, 690)) "11:00 Stand up")
  assertEqual "put in front" "09:00 - 09:30 Read the paper" (withBlock hhmm (Just (540, 570)) "Read the paper")
  assertEqual "taken off" "Stand up" (withBlock hhmm Nothing "11:00 Stand up")
  assertEqual "the 12-hour clock" "01:05 pm - 02:00 pm Call" (withBlock [THour12 2, TLiteral ":", TMinute 2, TLiteral " ", TMeridiem False] (Just (785, 840)) "Call")

-- | A line moved to another day outside a daily note is scheduled for it, in whichever way it was
-- scheduled already, and with an @⏳@ at its end where it was not — before a block link.
testWithScheduledDay :: TestTree
testWithScheduledDay = testCase "a line is scheduled for its new day" $ do
  let day = fromGregorian 2026 1 6
  assertEqual "the hourglass" "Go \x23F3 2026-01-06 #task" (withScheduledDay day "Go \x23F3 2026-01-04 #task")
  assertEqual "the bracketed field" "Pick up [scheduled:: 2026-01-06]" (withScheduledDay day "Pick up [scheduled:: 2026-01-05]")
  assertEqual "the parenthesized field" "Pick up (scheduled::2026-01-06) later" (withScheduledDay day "Pick up (scheduled::2026-01-05) later")
  assertEqual "scheduled where it was not" "Sometime \x23F3 2026-01-06" (withScheduledDay day "Sometime")
  assertEqual "before a block link" "Sometime \x23F3 2026-01-06 ^abc-1" (withScheduledDay day "Sometime ^abc-1")

-- | A day is written in the daily notes' format: the reversion of reading one.
testDayText :: TestTree
testDayText = testCase "a day is written in the daily notes' format" $ do
  let day = fromGregorian 2026 1 3
  assertEqual "the default" "2026-01-03" (dayText defaultDailyNoteFormat day)
  assertEqual "a folder of its own" "2026/Jan/Saturday 3rd" (dayText [DYear4, DLiteral "/", DMonthShort, DLiteral "/", DWeekdayName, DLiteral " ", DDayOrdinal] day)
  assertEqual "where it is" "/vault/Journal/2026-01-03.md" (dailyNotePath (DailyNoteSettings "Journal" defaultDailyNoteFormat) "/vault" day)

-- | A task of a planned note, declared with the block and the day given rather than read off it, as
-- an edit sends one.
taskStatement :: String -> Maybe (Minutes, Minutes) -> Maybe Day -> RawStatementTree RawExpr ()
taskStatement says time day = taskWith says (plannedArgs (time, day))

-- | A task of a planned note declared without the planner's fields at all, as a checkbox checked in
-- the webdocs is sent: with what its box says and whether it is checked, and nothing of its time.
taskWith :: String -> [(Maybe ArgName, RawExpr ())] -> RawStatementTree RawExpr ()
taskWith says planned = RawStatementTree (RawDeclStatement (RawObjArr (Just task) TypeObj Nothing [] Nothing Nothing)) []
  where
    task = rawVal noteSimpleTaskPath `applyRawArgs` ([
      (Just $ partialKey (mkName noteTaskContents), rawStr says),
      (Just $ partialKey (mkName noteTaskState), rawVal falsePrim)
                                                     ] ++ planned)

-- | A note's page holding these statements.
pageOf :: [RawStatementTree RawExpr ()] -> RawPrgm ()
pageOf statements = RawPrgm [] [RawStatementTree page statements]
  where RawStatementTree page _ = notePageStatement Nothing Nothing (const Nothing) "note" ""

-- | A task whose fields say what its line says is written as it was, which is every task an edit did
-- not touch; one whose block or day was changed is written to say so; and one moved to another day
-- out of a daily note is taken out of it and handed back to be written into that day's.
testFieldsWrittenBack :: TestTree
testFieldsWrittenBack = testCase "a changed block and day are written into the line" $ do
  let saturday = fromGregorian 2026 1 3
  let sunday = fromGregorian 2026 1 4
  let daily = Just $ NoteWriting' defaultPlannerSettings (Just saturday)
  let elsewhere = Just $ NoteWriting' defaultPlannerSettings Nothing
  assertEqual "unchanged, with no end written" (Just ("- [ ] 11:00 Stand up\n", []))
    (unparseNoteWith daily $ pageOf [taskStatement "11:00 Stand up" (Just (660, 690)) (Just saturday)])
  assertEqual "with no fields" (Just ("- [ ] 11:00 Stand up\n", []))
    (unparseNoteWith daily $ pageOf [taskWith "11:00 Stand up" []])
  assertEqual "moved in its day" (Just ("- [ ] 12:00 - 12:30 Stand up\n", []))
    (unparseNoteWith daily $ pageOf [taskStatement "11:00 Stand up" (Just (720, 750)) (Just saturday)])
  assertEqual "given no time" (Just ("- [ ] Stand up\n", []))
    (unparseNoteWith daily $ pageOf [taskStatement "11:00 Stand up" Nothing (Just saturday)])
  assertEqual "moved to another day" (Just ("- [ ] Read\n", [(sunday, NoteTask " " "08:00 - 08:30 Stand up" [])]))
    (unparseNoteWith daily $ pageOf [taskStatement "11:00 Stand up" (Just (480, 510)) (Just sunday), taskStatement "Read" Nothing (Just saturday)])
  assertEqual "scheduled for another day" (Just ("- [ ] 10:00 Pick up [scheduled:: 2026-01-04]\n", []))
    (unparseNoteWith elsewhere $ pageOf [taskStatement "10:00 Pick up [scheduled:: 2026-01-05]" (Just (600, 630)) (Just sunday)])

-- | A part moved onto a day goes under the planner's heading of that day's note, after what is
-- already listed there; under a new heading where the note has none; and into a new note where there
-- is none.
testPlacedUnderHeading :: TestTree
testPlacedUnderHeading = testCase "a moved part goes under the planner's heading" $ do
  let moved = [NoteTask " " "08:00 - 08:30 Stand up" []]
  assertEqual "after the list" (unlines ["# Day planner", "", "- [ ] 07:00 Wake", "- [ ] 08:00 - 08:30 Stand up", "", "# Notes", "More."])
    (placedUnderHeading "Day planner" moved (unlines ["# Day planner", "", "- [ ] 07:00 Wake", "", "# Notes", "More."]))
  assertEqual "a new heading" (unlines ["Some writing.", "", "# Day planner", "", "- [ ] 08:00 - 08:30 Stand up"])
    (placedUnderHeading "Day planner" moved "Some writing.\n")
  assertEqual "a new note" (unlines ["# Day planner", "", "- [ ] 08:00 - 08:30 Stand up"])
    (placedUnderHeading "Day planner" moved "")

-- | A block created on a day is written as a line of that day's note would be once moved there:
-- what it says, with its block written in front of it.
testCreatedLine :: TestTree
testCreatedLine = testCase "a created block is written as a line of its day" $ do
  let saturday = fromGregorian 2026 1 3
  assertEqual "" (Just ([], [NoteTask " " "08:00 - 08:30 Stretch" []]))
    (placedPart (Just $ NoteWriting' defaultPlannerSettings (Just saturday)) [] (taskStatement "Stretch" (Just (480, 510)) (Just saturday)))

obsidianPlannerTests :: TestTree
obsidianPlannerTests = testGroup "ObsidianPlanner" [
  testGroup "Settings" [
    testPlannerOff,
    testPlannerDefaults,
    testPlannerSettingsRead,
    testEmptyFormat,
    testDateFormats
                       ],
  testGroup "Reading" [
    testNoteDay,
    testTimeRanges,
    testScheduledDay,
    testResolveEnds,
    testPlannedNoteRoundTrip,
    testEventParts,
    testNoEventsWithoutPlanner,
    testEventRoundTrip
                      ],
  testGroup "Writing" [
    testWithBlock,
    testWithScheduledDay,
    testDayText,
    testFieldsWrittenBack,
    testPlacedUnderHeading,
    testCreatedLine
                      ]
                                                   ]
