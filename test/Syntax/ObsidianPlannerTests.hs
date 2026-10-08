-- | Tests for reading a vault's Day Planner and Daily Notes settings, and for reading a line, a note's
-- name and a note's blocks by them.
--
-- Covered here rather than through a vault of notes for the reason 'Syntax.ObsidianTaskTests' gives:
-- a fixture vault is set one way, and what a vault set another way reads as — another daily note
-- format, a block extended until the next — has no fixture. What a note of the planner fixture vault
-- is read as is held by the goldens of @test\/Integration\/code\/artifacts\/catln-planner@.
module Syntax.ObsidianPlannerTests where

import           Data.Time.Calendar       (fromGregorian)
import           Syntax.Ct.Prgm           (RawPrgm (..))
import           Syntax.Obsidian.Parser   (NotePlanner (..), notePageStatement,
                                           unparseNote)
import           Syntax.Obsidian.Planner
import           Syntax.Obsidian.Settings (DailyNoteSettings (..),
                                           DateToken (..), PlannerSettings (..),
                                           dailyNotesFile, dateFormatTokens,
                                           dayPlannerPluginId,
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
    testPlannedNoteRoundTrip
                      ]
                                                   ]
