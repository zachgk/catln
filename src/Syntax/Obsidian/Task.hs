--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Task
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads one checkbox line the way the Obsidian Tasks community plugin reads it.
--
-- A line the plugin manages says more than what it says. Beside the writing it carries dates, a
-- priority, a recurrence, an id and the ids it waits on, each written after a mark naming which of
-- them it is — in emoji, or as a @dataview@ inline field, as the vault is set
-- ('Syntax.Obsidian.Settings.TaskFormat'). What is left when all of that is taken off is what the
-- task says.
--
-- Read from the END of the line and never from the first mark found, which is how the plugin reads
-- it: a line ending @📅 2026-01-01@ has a due date, and a line saying @📅 lunch@ says @📅 lunch@.
-- Anchoring at the end is what tells the two apart, and it is why this takes one field off at a
-- time and looks again rather than scanning the line once for each mark it knows.
--
-- What is read here is only what a line SAYS. What the line was is carried beside it in
-- 'otContents', unread and unchanged, because the note is written back from that and not from this
-- — so a line carrying something this cannot yet read is still written back carrying it.
--------------------------------------------------------------------

module Syntax.Obsidian.Task where

import           Data.Char                (isAlpha, isAlphaNum, isDigit,
                                           isSpace)
import           Data.List                (dropWhileEnd, isPrefixOf, isSuffixOf)
import           Data.Maybe               (listToMaybe, mapMaybe)
import           Syntax.Obsidian.Settings (TaskFormat (..), TaskSettings (..))

-- | How urgent a task is, as the plugin's six priorities.
--
-- 'PNone' sits between medium and low rather than at either end, which is where a task nobody gave
-- a priority to sorts among ones somebody did.
data TaskPriority
  = PHighest
  | PHigh
  | PMedium
  | PNone
  | PLow
  | PLowest
  deriving (Eq, Show)

-- | One checkbox line, read the way the plugin reads it.
data ObsidianTask = ObsidianTask {
  -- | The line after the box, exactly as it was written. Everything below is a reading of this.
  otContents     :: String,
  -- | What the line says once the plugin's own writing is off it. The tags stay in it, as the
  -- plugin leaves them there, and are listed in 'otTags' as well.
  otDescription  :: String,
  otTags         :: [String],
  otPriority     :: TaskPriority,
  otCreated      :: Maybe String,
  otStart        :: Maybe String,
  otScheduled    :: Maybe String,
  otDue          :: Maybe String,
  otDone         :: Maybe String,
  otCancelled    :: Maybe String,
  otRecurrence   :: Maybe String,
  otOnCompletion :: Maybe String,
  -- | The id a task gives itself, which another task waits on by naming it.
  otTaskId       :: Maybe String,
  otDependsOn    :: [String],
  -- | The @^id@ a note gives the line, so that a link may name the line rather than the note.
  otBlockLink    :: Maybe String
                                 }
  deriving (Eq, Show)

-- | A task saying nothing, which reading a line fills in.
emptyTask :: String -> ObsidianTask
emptyTask contents = ObsidianTask contents "" [] PNone Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

-- | Which field a mark on a line names.
data SigKey
  = SigCreated
  | SigStart
  | SigScheduled
  | SigDue
  | SigDone
  | SigCancelled
  | SigRecurrence
  | SigOnCompletion
  | SigId
  | SigDependsOn
  | SigPriority TaskPriority
  deriving (Eq, Show)

-- | The task a checkbox line is, where the vault's settings make it one.
--
-- Nothing where the vault asks for a word every task carries and the line does not carry it. Such a
-- line is a checkbox of the note rather than a task of the plugin, and is left to be read as the
-- plain checkbox it is.
obsidianTask :: TaskSettings -> String -> Maybe ObsidianTask
obsidianTask TaskSettings{tstGlobalFilter, tstRemoveGlobalFilter, tstFormat} contents
  | not (null tstGlobalFilter) && not (tstGlobalFilter `isInfix` contents) = Nothing
  | otherwise = Just $ (foldr apply (emptyTask contents) found){
      otDescription = described,
      otTags = tags described,
      otBlockLink = link
                                                               }
  where
    (withoutLink, link) = takeBlockLink $ dropWhileEnd isSpace contents
    (left, found) = takeFields tstFormat withoutLink
    -- Taken out of what the task says, having been found in it, and only where the vault asks for
    -- that. It is still what made the line a task; it is just not part of what the line says.
    described = trimEnd $ if tstRemoveGlobalFilter then without tstGlobalFilter left else left
    apply (key, value) = setField key value

-- | The field one mark named, written into the task.
setField :: SigKey -> String -> ObsidianTask -> ObsidianTask
setField SigCreated v t      = t{otCreated = Just v}
setField SigStart v t        = t{otStart = Just v}
setField SigScheduled v t    = t{otScheduled = Just v}
setField SigDue v t          = t{otDue = Just v}
setField SigDone v t         = t{otDone = Just v}
setField SigCancelled v t    = t{otCancelled = Just v}
setField SigRecurrence v t   = t{otRecurrence = Just v}
setField SigOnCompletion v t = t{otOnCompletion = Just v}
setField SigId v t           = t{otTaskId = Just v}
setField SigDependsOn v t    = t{otDependsOn = splitIds v}
setField (SigPriority p) _ t = t{otPriority = p}

-- | Everything the line's marks name, and what is left of the line once they are off it.
--
-- One at a time from the end, looking again after each, because taking one off is what puts the
-- next at the end. A line ending @📅 2026-01-01 ⏫@ gives up its priority first and its due date
-- second, and neither is found by looking at the line as a whole.
takeFields :: TaskFormat -> String -> (String, [(SigKey, String)])
takeFields format = go []
  where
    go acc s = case takeFieldAtEnd format (trimEnd s) of
      Just (rest, field) -> go (field : acc) rest
      Nothing            -> (trimEnd s, acc)

-- | The one field the line ends with, and what is left before it.
takeFieldAtEnd :: TaskFormat -> String -> Maybe (String, (SigKey, String))
takeFieldAtEnd TasksEmoji s = listToMaybe $ mapMaybe try emojiFields
  where
    try (mark, key, valid) = case key of
      SigPriority _ | mark `isSuffixOf` s -> Just (trimEnd $ take (length s - length mark) s, (key, ""))
      SigPriority _ -> Nothing
      _ -> do
        (before, after) <- lastSplitOn mark s
        let value = trimMarked after
        if valid value then Just (trimEnd before, (key, value)) else Nothing
takeFieldAtEnd Dataview s = do
  (before, key, value) <- inlineFieldAtEnd s
  sig <- lookup key dataviewFields
  case sig of
    SigPriority _ -> (\p -> (trimEnd before, (SigPriority p, ""))) <$> lookup value dataviewPriorities
    _             -> Just (trimEnd before, (sig, value))

-- | The marks the plugin writes each field after, with the shape of what it writes there.
--
-- The alternates are the plugin's: a vault may write a scheduled date after either hourglass, and a
-- due date after any of three calendars. A mark is only taken when what follows it is what that
-- field holds, which is what leaves @📅 lunch@ as writing.
--
-- The priorities carry no value and are matched as the end of the line itself.
emojiFields :: [(String, SigKey, String -> Bool)]
emojiFields = [
  ("\x1F53A", SigPriority PHighest, const True),
  ("\x23EB", SigPriority PHigh, const True),
  ("\x1F53C", SigPriority PMedium, const True),
  ("\x1F53D", SigPriority PLow, const True),
  ("\x23EC", SigPriority PLowest, const True),
  ("\x2795", SigCreated, isDate),
  ("\x1F6EB", SigStart, isDate),
  ("\x23F3", SigScheduled, isDate),
  ("\x231B", SigScheduled, isDate),
  ("\x1F4C5", SigDue, isDate),
  ("\x1F4C6", SigDue, isDate),
  ("\x1F5D3", SigDue, isDate),
  ("\x2705", SigDone, isDate),
  ("\x274C", SigCancelled, isDate),
  ("\x1F501", SigRecurrence, isRecurrence),
  ("\x1F3C1", SigOnCompletion, isOnCompletion),
  ("\x1F194", SigId, isTaskId),
  ("\x26D4", SigDependsOn, isTaskIds)
              ]

-- | The keys the plugin writes each field under when a vault is set to @dataview@.
dataviewFields :: [(String, SigKey)]
dataviewFields = [
  ("created", SigCreated),
  ("start", SigStart),
  ("scheduled", SigScheduled),
  ("due", SigDue),
  ("completion", SigDone),
  ("cancelled", SigCancelled),
  ("repeat", SigRecurrence),
  ("onCompletion", SigOnCompletion),
  ("id", SigId),
  ("dependsOn", SigDependsOn),
  ("priority", SigPriority PNone)
                 ]

-- | How a priority is written out where the emoji format writes a mark.
dataviewPriorities :: [(String, TaskPriority)]
dataviewPriorities = [
  ("highest", PHighest),
  ("high", PHigh),
  ("medium", PMedium),
  ("low", PLow),
  ("lowest", PLowest)
                     ]

-- | The inline field the line ends with, as its key and its value.
--
-- Written between brackets or between parentheses, and closed by the one it was opened with — a
-- @[key:: value)@ is neither.
inlineFieldAtEnd :: String -> Maybe (String, String, String)
inlineFieldAtEnd s = listToMaybe $ mapMaybe try [('[', ']'), ('(', ')')]
  where
    try (open, close) = do
      inner <- closedBy close s
      (before, inside) <- lastSplitOn [open] inner
      let (key, value) = splitOnFirst "::" inside
      if null key || null value then Nothing else Just (trimEnd before, trim key, trim value)
    closedBy close str = if [close] `isSuffixOf` str then Just (init str) else Nothing

-- | The @^id@ a line ends with, and the line without it.
--
-- Written as a word of its own, so that a line saying @2^3 is 8@ is a line saying that rather than
-- a line whose id is @3 is 8@.
takeBlockLink :: String -> (String, Maybe String)
takeBlockLink s = case lastSplitOn "^" s of
  Just (before, ident)
    | not (null ident)
    , all isBlockIdChar ident
    , null before || isSpace (last before) -> (trimEnd before, Just ident)
  _ -> (s, Nothing)

-- | The tags a task writes, which stay in what it says as well as being listed.
tags :: String -> [String]
tags = filter isTag . words
  where
    isTag t = "#" `isPrefixOf` t && length t > 1 && any (not . isDigit) (drop 1 t)

-- | The ids a task waits on, which are written one after another between commas.
splitIds :: String -> [String]
splitIds v = filter (not . null) $ map trim $ splitOn ',' v

isDate :: String -> Bool
isDate d = length d == 10 && all digitAt [0, 1, 2, 3, 5, 6, 8, 9] && all dashAt [4, 7]
  where
    digitAt i = isDigit (d !! i)
    dashAt i = d !! i == '-'

isRecurrence :: String -> Bool
isRecurrence v = not (null v) && all (\c -> isAlphaNum c || c `elem` [' ', ',', '!']) v

isOnCompletion :: String -> Bool
isOnCompletion v = not (null v) && all isAlpha v

isTaskId :: String -> Bool
isTaskId v = not (null v) && all isBlockIdChar v

isTaskIds :: String -> Bool
isTaskIds v = not (null ids) && all isTaskId ids
  where ids = splitIds v

isBlockIdChar :: Char -> Bool
isBlockIdChar c = isAlphaNum c || c `elem` ['_', '-']

-- | Splits at the LAST occurrence of a separator, so that what follows it holds no other.
lastSplitOn :: String -> String -> Maybe (String, String)
lastSplitOn sep s = case [i | i <- [length s - length sep, length s - length sep - 1 .. 0], sep `isPrefixOf` drop i s] of
  (i:_) -> Just (take i s, drop (i + length sep) s)
  []    -> Nothing

splitOnFirst :: String -> String -> (String, String)
splitOnFirst sep = go ""
  where
    go acc s
      | sep `isPrefixOf` s = (reverse acc, drop (length sep) s)
      | (c:rest) <- s = go (c : acc) rest
      | otherwise = (reverse acc, "")

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (before, _:rest) -> before : splitOn c rest
  (before, [])     -> [before]

-- | Whether one string is written anywhere within another.
isInfix :: String -> String -> Bool
isInfix needle haystack = any (needle `isPrefixOf`) (suffixes haystack)
  where suffixes s = s : case s of { (_:rest) -> suffixes rest; [] -> [] }

-- | A string with the first occurrence of another taken out of it, and the space that left closed up.
without :: String -> String -> String
without needle haystack = case [i | i <- [0 .. length haystack - length needle], needle `isPrefixOf` drop i haystack] of
  (i:_) -> unwords $ words $ take i haystack ++ drop (i + length needle) haystack
  []    -> haystack

-- | Text after a mark, without the space written between them and without a variation selector
-- written on the mark. A calendar written @🗓️@ is the same mark as one written @🗓@, and the
-- difference belongs to nothing that reads it.
trimMarked :: String -> String
trimMarked = trim . dropWhile isVariationSelector

isVariationSelector :: Char -> Bool
isVariationSelector c = c `elem` ['\xFE0E', '\xFE0F']

trim :: String -> String
trim = trimEnd . dropWhile isSpace

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace
