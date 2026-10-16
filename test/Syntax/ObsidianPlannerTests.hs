-- | What a line of a vault running the Day Planner is READ as is user space, and is held by the
-- fixture vaults under @test\/Integration\/code\/artifacts@ and their goldens: @catln-planner@ holds
-- a time in each form the plugin reads one and in the forms it refuses, a scheduled day in each way
-- it is written, and notes named for no day; @catln-planner-journal@, @catln-planner-bare@ and
-- @catln-planner-named@ are set the other ways a vault can be (a block extended until the next, a
-- planner never configured, a daily note format spelling names and ordinals). What is left here is
-- writing a note BACK, which has no surface in a Catln program until the Obsidian runtime is Catln
-- code; and a daily note format the reader refuses, which fails the read of the vault rather than
-- reading as anything — a failure a Catln test cannot yet expect.
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
                                           placedPart, placedUnderHeading,
                                           plannedArgs, unparseNote,
                                           unparseNoteWith)
import           Syntax.Obsidian.Planner
import           Syntax.Obsidian.Settings (DailyNoteSettings (..),
                                           DateToken (..), PlannerSettings (..),
                                           TimeToken (..), dateFormatTokens,
                                           defaultDailyNoteFormat,
                                           defaultPlannerSettings)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | The formats a day's name cannot be read with are refused, rather than read as something else.
-- (The pieces a format is read into are held by the daily note of @catln-planner-named@, which is
-- a note only if its name is read as the day its vault's format spells.)
testDateFormats :: TestTree
testDateFormats = testCase "a daily note format the reader cannot read is refused" $ do
  assertEqual "a week of the year" True (isLeft $ dateFormatTokens "YYYY-[W]ww")
  assertEqual "a bracket never closed" True (isLeft $ dateFormatTokens "YYYY-[MM")
  where
    isLeft (Left _) = True
    isLeft _        = False

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
    testDateFormats
                       ],
  testGroup "Writing" [
    testPlannedNoteRoundTrip,
    testEventRoundTrip,
    testWithBlock,
    testWithScheduledDay,
    testDayText,
    testFieldsWrittenBack,
    testPlacedUnderHeading,
    testCreatedLine
                      ]
                                                   ]
