--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Settings
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads what a vault says about the Obsidian Tasks and Day Planner community plugins.
--
-- A checkbox in a vault running that plugin is not the checkbox markdown describes. The plugin
-- writes a task's dates, priority, recurrence and links into the line beside the box, and the vault
-- decides how: which of two notations those are written in, which character in the box means what,
-- and whether a line has to carry a particular word before it counts as a task at all. None of that
-- can be worked out from a note. It is written in the vault's own configuration, so reading a note
-- of such a vault means reading the vault's configuration first.
--
-- It reads what a vault says about the Day Planner community plugin in the same way and for the same
-- reason. That plugin reads a time at the start of a line as a block of the day the line is planned
-- for, and which day that is, and how long a block written without an end lasts, are both the
-- vault's to say: the day is the one the Daily Notes core plugin names the note for, and the length
-- is a setting of the planner's own.
--
-- Every name here is Obsidian's or the plugin's rather than ours — the file names, the keys, the
-- values those keys take. They are written down in this one module so that the reading of a
-- checkbox elsewhere is written in our terms and not in theirs.
--------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax.Obsidian.Settings where

import           Control.Exception (IOException, try)
import           Data.Aeson        (FromJSON (parseJSON), Value, withObject,
                                    withText, (.!=), (.:?))
import qualified Data.Aeson        as A
import           Data.Aeson.Types  (Parser)
import           Data.List         (find, stripPrefix)
import qualified Data.Text         as T
import           System.Directory  (doesFileExist)
import           System.FilePath   ((</>))
import           Text.Printf

-- | The id Obsidian knows the Tasks plugin by, which is the name of the directory it is installed
-- in and the name a vault lists it under when it is turned on.
tasksPluginId :: String
tasksPluginId = "obsidian-tasks-plugin"

-- | Where a vault lists the community plugins it has turned on: a list of their ids, and nothing
-- else. A plugin installed but turned off is not in it, which is the difference this reads.
communityPluginsFile :: FilePath -> FilePath
communityPluginsFile vault = vault </> ".obsidian" </> "community-plugins.json"

-- | Where one community plugin keeps what it is set to.
--
-- There is no file until the plugin has been given a setting to keep, so a vault running a plugin
-- on nothing but its defaults has none — which is why 'readTaskSettings' answers with the defaults
-- for a plugin that is turned on and has no file, rather than with nothing.
pluginDataFile :: FilePath -> String -> FilePath
pluginDataFile vault plugin = vault </> ".obsidian" </> "plugins" </> plugin </> "data.json"

-- | The files a vault says all of this in, whether or not they are there.
--
-- Named as files rather than reached by walking, because a walk of a tree passes over an entry
-- whose name begins with a dot ('Syntax.Ct.Prgm.readsTreeEntry') and these are kept in one. Named
-- whether or not they exist, because a file which is not there is read as not being there — so the
-- plugin being turned on later is a change to what every note of the vault is read as, and is seen
-- as one rather than waiting for each note to be written to.
taskSettingsFiles :: FilePath -> [FilePath]
taskSettingsFiles vault = [communityPluginsFile vault, pluginDataFile vault tasksPluginId]

-- | Which of the two notations a vault writes a task's fields in.
data TaskFormat
  = TasksEmoji
  | Dataview
  deriving (Eq, Show)

-- | One of the seven kinds of status the plugin knows.
--
-- The kind alone and never the symbol: which character means which kind is the vault's to say, and
-- 'TaskStatus' is where the two are put together.
data StatusKind
  = KTodo
  | KDone
  | KInProgress
  | KOnHold
  | KCancelled
  | KNonTask
  | KEmpty
  deriving (Eq, Show, Enum, Bounded)

-- | One status a vault knows: the character written in the box, what the vault calls it, and which
-- kind it is.
data TaskStatus = TaskStatus {
  tsSymbol :: String,
  tsName   :: String,
  tsKind   :: StatusKind
                             }
  deriving (Eq, Show)

-- | What one vault is set to, of the settings which decide how a checkbox is read.
--
-- The plugin keeps a great deal more than this — what it writes when a box is checked, how its
-- queries are shown, which commands it offers. None of that decides what a line SAYS, which is all
-- that reading one needs, so none of it is read.
data TaskSettings = TaskSettings {
  -- | A word every task must carry, and @""@ where a vault asks for none. A checkbox not carrying
  -- it is a checkbox of the note rather than a task of the plugin.
  tstGlobalFilter       :: String,
  -- | Whether the filter is taken out of what a task says, having been found in it.
  tstRemoveGlobalFilter :: Bool,
  tstFormat             :: TaskFormat,
  -- | Every status the vault knows, its own before the ones it was given.
  tstStatuses           :: [TaskStatus]
                                 }
  deriving (Eq, Show)

-- | What the plugin is set to before a vault has said otherwise.
--
-- Written out rather than guessed at, because a vault which has never opened the plugin's settings
-- has no file at all and is running exactly these — so these are not a fallback for a vault we
-- cannot read, they are that vault's actual settings. Taken from the plugin's own
-- @DEFAULT_SETTINGS@ and @StatusSettings@: two statuses of its own, and two more it ships as
-- customs.
defaultTaskSettings :: TaskSettings
defaultTaskSettings = TaskSettings "" False TasksEmoji [
  TaskStatus " " "Todo" KTodo,
  TaskStatus "x" "Done" KDone,
  TaskStatus "/" "In Progress" KInProgress,
  TaskStatus "-" "Cancelled" KCancelled
                                                       ]

-- | What a vault is set to, or nothing where it does not run the plugin at all.
--
-- The two files are read for different questions and answer differently when they are missing.
-- Whether the plugin is on is what the first says, and a vault with no such file has turned no
-- community plugin on — so nothing. What the plugin is set to is what the second says, and a vault
-- with no such file is running the plugin on its defaults — so the defaults.
--
-- A file which is there and is not what it should be is reported rather than passed over. A vault
-- that has not turned the plugin on is a fact about the vault; a @data.json@ which is not JSON is a
-- fact about nothing, and reading every note of that vault as though the plugin were off would be
-- answering a question we failed to ask.
readTaskSettings :: FilePath -> IO (Maybe TaskSettings)
readTaskSettings vault = do
  enabled <- readEnabledPlugins vault
  if tasksPluginId `notElem` enabled
    then return Nothing
    else do
      let path = pluginDataFile vault tasksPluginId
      there <- doesFileExist path
      if not there
        then return $ Just defaultTaskSettings
        else do
          read' <- A.eitherDecodeFileStrict path
          case read' of
            Right settings -> return $ Just settings
            Left err -> fail $ printf "The Obsidian Tasks plugin's settings at %s could not be read: %s" path err

-- | The community plugins a vault has turned on.
--
-- Anything the matter with the file reads as none of them being on: a vault which has never
-- installed a community plugin has no such file, one being written as this is read is a file half
-- there, and neither is a vault running the Tasks plugin. This is the one place here that passes
-- over a bad file, and it does so because a missing one and an unreadable one say the same thing.
readEnabledPlugins :: FilePath -> IO [String]
readEnabledPlugins vault = do
  read' <- try (A.eitherDecodeFileStrict (communityPluginsFile vault)) :: IO (Either IOException (Either String [String]))
  return $ case read' of
    Right (Right plugins) -> plugins
    _                     -> []

-- | The status a box's symbol is, as the vault names it.
--
-- A symbol the vault names no status for is a status of its own, named by the symbol and counted as
-- something still to do. That is what the plugin's own registry makes of an unknown symbol, and it
-- is the reading that keeps a box the vault has not been told about a task rather than nothing.
statusFor :: TaskSettings -> String -> TaskStatus
statusFor TaskSettings{tstStatuses} symbol = case find ((== symbol) . tsSymbol) tstStatuses of
  Just status -> status
  Nothing     -> TaskStatus symbol symbol KTodo

-- | Read as the plugin writes it, a key at a time, so that a file written by an older version of
-- the plugin — or by hand — is read for whatever it does say. The plugin itself lays what it reads
-- over its defaults rather than requiring a whole file, so a key which is not there is that
-- setting's default and not an error.
instance FromJSON TaskSettings where
  parseJSON = withObject "TaskSettings" $ \o -> do
    globalFilter <- o .:? "globalFilter" .!= tstGlobalFilter defaultTaskSettings
    removeFilter <- o .:? "removeGlobalFilter" .!= tstRemoveGlobalFilter defaultTaskSettings
    format <- o .:? "taskFormat" .!= tstFormat defaultTaskSettings
    statuses <- o .:? "statusSettings" >>= maybe (return $ tstStatuses defaultTaskSettings) parseStatusSettings
    return $ TaskSettings globalFilter removeFilter format statuses

-- | The statuses a vault knows, which it keeps as two lists: the plugin's own, and the ones the
-- vault added. They are one list here, the vault's own first, because what reading a box needs is
-- which status wears which symbol and not where that status came from.
parseStatusSettings :: Value -> Parser [TaskStatus]
parseStatusSettings = withObject "statusSettings" $ \o -> do
  core <- o .:? "coreStatuses" .!= []
  custom <- o .:? "customStatuses" .!= []
  return $ core ++ custom

instance FromJSON TaskStatus where
  parseJSON = withObject "TaskStatus" $ \o -> do
    symbol <- o .:? "symbol" .!= ""
    name <- o .:? "name" .!= ""
    kind <- o .:? "type" .!= KTodo
    return $ TaskStatus symbol name kind

instance FromJSON TaskFormat where
  parseJSON = withText "taskFormat" $ \t -> case T.unpack t of
    "tasksPluginEmoji" -> return TasksEmoji
    "dataview"         -> return Dataview
    other              -> fail $ printf "Unknown Obsidian Tasks format %s" (show other)

-- | A status of a kind the plugin does not know is counted as something still to do, rather than
-- failing the vault. The plugin adds kinds as it grows, and a vault written by a newer one than we
-- were written against is a vault to read as best we can — a box we cannot name the kind of is
-- still a box.
instance FromJSON StatusKind where
  parseJSON = withText "StatusType" $ \t -> return $ case T.unpack t of
    "DONE"        -> KDone
    "IN_PROGRESS" -> KInProgress
    "ON_HOLD"     -> KOnHold
    "CANCELLED"   -> KCancelled
    "NON_TASK"    -> KNonTask
    "EMPTY"       -> KEmpty
    _             -> KTodo

-- | The id Obsidian knows the Day Planner plugin by.
dayPlannerPluginId :: String
dayPlannerPluginId = "obsidian-day-planner"

-- | Where a vault keeps what its Daily Notes core plugin is set to: which folder the daily notes are
-- written in, and the format a daily note's name is written in.
dailyNotesFile :: FilePath -> FilePath
dailyNotesFile vault = vault </> ".obsidian" </> "daily-notes.json"

-- | The files a vault says how it plans its days in, whether or not they are there, for the reason
-- 'taskSettingsFiles' names its own.
plannerSettingsFiles :: FilePath -> [FilePath]
plannerSettingsFiles vault = [communityPluginsFile vault, pluginDataFile vault dayPlannerPluginId, dailyNotesFile vault]

-- | What one vault is set to, of the settings which decide what a line of a planned day says.
--
-- The plugin keeps much more — colours, what its timeline shows, the calendars it subscribes to. What
-- is read is what decides what a line says and how a line is written: the two settings deciding the
-- end of a block written without one, and the two the plugin writes a moved block back with.
data PlannerSettings = PlannerSettings {
  -- | How long a block written without an end lasts.
  pstDefaultDuration :: Int,
  -- | Whether a block written without an end lasts until the next block of its note begins instead.
  pstExtendUntilNext :: Bool,
  -- | Where the vault keeps its daily notes, which is what says which day a note plans.
  pstDailyNotes      :: DailyNoteSettings,
  -- | How the plugin writes a time of day when it writes a block back.
  pstTimestampFormat :: [TimeToken],
  -- | The heading of a daily note the plugin puts a block moved onto that day under.
  pstPlannerHeading  :: String
                                       }
  deriving (Eq, Show)

-- | Which notes are daily notes, and which day each is: the folder they are written in, relative to
-- the vault and @""@ for the vault itself, and the format a daily note's name is written in.
data DailyNoteSettings = DailyNoteSettings {
  dnsFolder :: FilePath,
  dnsFormat :: [DateToken]
                                           }
  deriving (Eq, Show)

-- | What the plugins are set to before a vault has said otherwise, which is what a vault with no
-- files for them is running — taken from the Day Planner's own @defaultSettings@ and from the Daily
-- Notes plugin's default format.
defaultPlannerSettings :: PlannerSettings
defaultPlannerSettings = PlannerSettings 30 False defaultDailyNoteSettings defaultTimestampFormat "Day planner"

-- | How the plugin writes a time of day when the vault names no format: @HH:mm@.
defaultTimestampFormat :: [TimeToken]
defaultTimestampFormat = [THour24 2, TLiteral ":", TMinute 2]

defaultDailyNoteSettings :: DailyNoteSettings
defaultDailyNoteSettings = DailyNoteSettings "" defaultDailyNoteFormat

-- | The name the Daily Notes plugin gives a daily note when the vault names no format: @YYYY-MM-DD@.
defaultDailyNoteFormat :: [DateToken]
defaultDailyNoteFormat = [DYear4, DLiteral "-", DMonth2, DLiteral "-", DDay2]

-- | What a vault is set to for planning its days, or nothing where it does not run the Day Planner.
--
-- Read as 'readTaskSettings' reads its plugin, and answering missing files the way it does: a vault
-- which has not turned the plugin on plans nothing, and one which has turned it on and has no file
-- for it, or no file for its daily notes, is running on the defaults. A file which is there and
-- cannot be read is reported, and so is a daily note format this cannot read a day out of — reading
-- every daily note of such a vault as though it planned no day would be answering a question we
-- failed to ask.
readPlannerSettings :: FilePath -> IO (Maybe PlannerSettings)
readPlannerSettings vault = do
  enabled <- readEnabledPlugins vault
  if dayPlannerPluginId `notElem` enabled
    then return Nothing
    else do
      planner <- readJsonOr defaultPlannerSettings "Day Planner plugin's" (pluginDataFile vault dayPlannerPluginId)
      daily <- readJsonOr defaultDailyNoteSettings "Daily Notes plugin's" (dailyNotesFile vault)
      return $ Just planner{pstDailyNotes = daily}

-- | A settings file read as JSON, or the given defaults where there is no such file.
readJsonOr :: (FromJSON a) => a -> String -> FilePath -> IO a
readJsonOr defaults whose path = do
  there <- doesFileExist path
  if not there
    then return defaults
    else do
      read' <- A.eitherDecodeFileStrict path
      case read' of
        Right settings -> return settings
        Left err -> fail $ printf "The %s settings at %s could not be read: %s" whose path err

-- | Read a key at a time over the defaults, as the plugin reads its own file.
instance FromJSON PlannerSettings where
  parseJSON = withObject "PlannerSettings" $ \o -> do
    duration <- o .:? "defaultDurationMinutes" .!= pstDefaultDuration defaultPlannerSettings
    extend <- o .:? "extendDurationUntilNext" .!= pstExtendUntilNext defaultPlannerSettings
    timestamp <- o .:? "timestampFormat" .!= ""
    timestamp' <- if null timestamp
      then return defaultTimestampFormat
      else either fail return $ timeFormatTokens timestamp
    heading <- o .:? "plannerHeading" .!= pstPlannerHeading defaultPlannerSettings
    return $ PlannerSettings duration extend defaultDailyNoteSettings timestamp' heading

-- | An empty format is how the Daily Notes plugin writes that it uses its default, so it is read as
-- the default rather than as a name with nothing in it.
instance FromJSON DailyNoteSettings where
  parseJSON = withObject "DailyNoteSettings" $ \o -> do
    folder <- o .:? "folder" .!= ""
    format <- o .:? "format" .!= ""
    tokens <- if null (trimFormat format)
      then return defaultDailyNoteFormat
      else either fail return $ dateFormatTokens (trimFormat format)
    return $ DailyNoteSettings (trimFolder folder) tokens
    where
      trimFormat = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
      -- A folder is written relative to the vault, with or without slashes around it.
      trimFolder = reverse . dropWhile (== '/') . reverse . dropWhile (== '/') . trimFormat

-- | One piece of a date format, as Moment.js — which the Daily Notes plugin formats names with —
-- reads one.
data DateToken
  = DYear4
  | DYear2
  | DMonth2
  | DMonth1
  | DMonthName
  | DMonthShort
  | DDay2
  | DDay1
  | DDayOrdinal
  | DWeekdayName
  | DWeekdayShort
  | DLiteral String
  deriving (Eq, Show)

-- | The pieces of a Moment.js date format, or why it cannot be read.
--
-- Only the pieces a name naming a single day is written with are read: the year, the month, the
-- day of the month and the day of the week, in each of the ways Moment writes them, and text
-- written between brackets or as anything which is no letter. A format writing anything else —
-- a week of the year, an hour — is reported rather than read past, because Moment would read those
-- letters as pieces of a date too, and reading them as text would take a note's name to name a day
-- it does not.
dateFormatTokens :: String -> Either String [DateToken]
dateFormatTokens = fmap joinLiterals . aux
  where
    joinLiterals (DLiteral a : DLiteral b : rest) = joinLiterals (DLiteral (a ++ b) : rest)
    joinLiterals (t : rest)                       = t : joinLiterals rest
    joinLiterals []                               = []
    aux [] = Right []
    aux ('[':rest) = case break (== ']') rest of
      (lit, _:rest') -> (DLiteral lit :) <$> aux rest'
      _ -> Left "a daily note format opens a bracket it never closes"
    aux s@(c:rest) = case [(tok, rest') | (spelling, tok) <- spellings, Just rest' <- [stripSpelling spelling s]] of
      ((tok, rest'):_) -> (tok :) <$> aux rest'
      []
        | isLetter c -> Left $ printf "the daily note format piece starting at %s is not one a day's name is read with" (show s)
        | otherwise -> (DLiteral [c] :) <$> aux rest
    -- Longest first, so that @MMMM@ is not read as two @MM@s.
    spellings = [("YYYY", DYear4), ("YY", DYear2), ("MMMM", DMonthName), ("MMM", DMonthShort), ("MM", DMonth2), ("M", DMonth1), ("Do", DDayOrdinal), ("DD", DDay2), ("D", DDay1), ("dddd", DWeekdayName), ("ddd", DWeekdayShort)]
    stripSpelling spelling s = if take (length spelling) s == spelling then Just (drop (length spelling) s) else Nothing
    isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | One piece of a Moment.js time format, as the Day Planner writes a time of day with one: an hour
-- of a 24-hour or a 12-hour clock or a minute, at least as many digits wide as given, the half of the
-- day in either case, or writing of its own.
data TimeToken
  = THour24 Int
  | THour12 Int
  | TMinute Int
  | TMeridiem Bool
  | TLiteral String
  deriving (Eq, Show)

-- | The pieces of a Moment.js time format, or why it cannot be read: the reading 'dateFormatTokens'
-- gives a date format, over the pieces a time is written with.
timeFormatTokens :: String -> Either String [TimeToken]
timeFormatTokens = aux
  where
    aux [] = Right []
    aux ('[':rest) = case break (== ']') rest of
      (lit, _:rest') -> (TLiteral lit :) <$> aux rest'
      _              -> Left "a time format opens a bracket it never closes"
    aux s@(c:rest) = case [(tok, rest') | (spelling, tok) <- spellings, Just rest' <- [stripPrefix spelling s]] of
      ((tok, rest'):_) -> (tok :) <$> aux rest'
      []
        | isAsciiLetter c -> Left $ printf "the time format piece starting at %s is not one a time is written with" (show s)
        | otherwise -> (TLiteral [c] :) <$> aux rest
    -- Longest first, so that @HH@ is not read as two @H@s.
    spellings = [("HH", THour24 2), ("H", THour24 1), ("hh", THour12 2), ("h", THour12 1), ("mm", TMinute 2), ("m", TMinute 1), ("A", TMeridiem True), ("a", TMeridiem False)]
    isAsciiLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
