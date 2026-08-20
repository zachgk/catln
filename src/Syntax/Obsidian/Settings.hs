--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Settings
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads what a vault says about the Obsidian Tasks community plugin.
--
-- A checkbox in a vault running that plugin is not the checkbox markdown describes. The plugin
-- writes a task's dates, priority, recurrence and links into the line beside the box, and the vault
-- decides how: which of two notations those are written in, which character in the box means what,
-- and whether a line has to carry a particular word before it counts as a task at all. None of that
-- can be worked out from a note. It is written in the vault's own configuration, so reading a note
-- of such a vault means reading the vault's configuration first.
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
import           Data.List         (find)
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
