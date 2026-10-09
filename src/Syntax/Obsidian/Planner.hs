--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Planner
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads a checkbox line the way the Obsidian Day Planner community plugin reads it.
--
-- The plugin reads a time written at the start of a line as a block of the day: @09:00 - 10:30
-- Write@ is writing from nine until half past ten, and @9:30pm Call@ is a call at half past nine at
-- night lasting however long the vault says a block lasts. Which day that is, is not written on the
-- line. A line in a daily note is planned for the day the note is named for, and a line anywhere
-- else is planned for the day it is scheduled for, written after an @⏳@ or as a @scheduled@ inline
-- field.
--
-- The times are read exactly as the plugin's own regular expression reads them
-- (@timeRangeAtStartOfLineRegExp@ in its @src\/regexp.ts@), down to which of its alternatives is
-- tried first, so that a line is a block here exactly where it is a block in the vault.
--------------------------------------------------------------------

module Syntax.Obsidian.Planner where

import           Control.Monad            (guard)
import           Data.Char                (isAlphaNum, isDigit, isSpace,
                                           toLower)
import           Data.List                (isPrefixOf, sort, stripPrefix)
import           Data.Maybe               (fromMaybe, isJust, listToMaybe,
                                           mapMaybe)
import           Data.Time.Calendar       (Day, DayOfWeek (..), dayOfWeek,
                                           fromGregorianValid, showGregorian,
                                           toGregorian)
import           Syntax.Obsidian.Settings (DailyNoteSettings (..),
                                           DateToken (..), PlannerSettings (..),
                                           TimeToken (..))
import           Syntax.Obsidian.Task     (readIsoDay)
import           System.FilePath          (dropExtension, makeRelative,
                                           splitDirectories, (<.>), (</>))
import           Text.Printf              (printf)

-- | A time of day, as the minutes since its midnight.
type Minutes = Int

-- | A block of a day as a line writes it: when it starts, and when it ends if the line says.
data WrittenBlock = WrittenBlock {
  wbStart :: Minutes,
  wbEnd   :: Maybe Minutes
                                 }
  deriving (Eq, Show)

-- | The block a line writes at its start, if it writes one, and what the line says after it.
--
-- A start alone, or a start, a dash and an end. Where the plugin's expression finds a time that is
-- no time — @25:00@, or @13pm@ — the line writes no block, as the plugin cannot place one either.
timeRangeAtStart :: String -> Maybe (WrittenBlock, String)
timeRangeAtStart s = do
  (start, afterStart) <- timeAt s
  case endAfter afterStart of
    Just (end, afterEnd) -> do
      start' <- start
      end' <- end
      return (WrittenBlock start' (Just end'), dropWhile isSpace afterEnd)
    Nothing -> do
      start' <- start
      return (WrittenBlock start' Nothing, dropWhile isSpace afterStart)
  where
    -- @(?:\s?-\s?(end))?@: the end is taken whenever one is written there, as the group is greedy.
    endAfter rest = do
      afterDash <- stripPrefix "-" (optionalSpace rest)
      timeAt (optionalSpace afterDash)
    optionalSpace (c:rest) | isSpace c = rest
    optionalSpace rest = rest

-- | The time a string starts with, if the plugin's expression matches one there, and the string
-- after it.
--
-- What matched is answered apart from whether it names a time, as a 'Maybe' inside the match: the
-- expression decides where a time is written, and only then is it read, so a match which is no
-- time still uses up what it matched.
timeAt :: String -> Maybe (Maybe Minutes, String)
timeAt s = listToMaybe (twelveHour s ++ twentyFourHour s)

-- | @(?:[0-1]?\d)(?:[:.]?(?:[0-5]\d))(?:\s?[apAP][mM](?!\w))@: a 12-hour time, where the hour may
-- be one digit or two and the colon or dot may be left out.
--
-- Every way the expression can match is listed, in the order it tries them, so the first one is the
-- one it takes.
twelveHour :: String -> [(Maybe Minutes, String)]
twelveHour s = do
  (hour, s1) <- hourDigits ['0', '1'] s
  s2 <- optionalSeparator s1
  (minute, s3) <- minuteDigits s2
  s4 <- [drop 1 s3 | startsWithSpace s3] ++ [s3]
  (pm, s5) <- meridiem s4
  guard $ not (startsWithWordChar s5)
  let valid = hour >= 1 && hour <= 12
  return (if valid then Just ((hour `mod` 12 + if pm then 12 else 0) * 60 + minute) else Nothing, s5)
  where
    optionalSeparator str = [rest | (c:rest) <- [str], c `elem` [':', '.']] ++ [str]
    meridiem (a:m:rest) | toLower a `elem` ['a', 'p'], toLower m == 'm' = [(toLower a == 'p', rest)]
    meridiem _ = []

-- | @(?:[0-2]?\d)(?:[:.](?:[0-5]\d))@: a 24-hour time, whose colon or dot is always written.
twentyFourHour :: String -> [(Maybe Minutes, String)]
twentyFourHour s = do
  (hour, s1) <- hourDigits ['0', '1', '2'] s
  s2 <- [rest | (c:rest) <- [s1], c `elem` [':', '.']]
  (minute, s3) <- minuteDigits s2
  return (if hour <= 23 then Just (hour * 60 + minute) else Nothing, s3)

-- | @[first]?\d@: an hour of two digits whose first is one of the given ones, then an hour of one.
hourDigits :: [Char] -> String -> [(Int, String)]
hourDigits firsts s = [(read [a, b], rest) | (a:b:rest) <- [s], a `elem` firsts, isDigit b] ++ [(read [a], rest) | (a:rest) <- [s], isDigit a]

-- | @[0-5]\d@: minutes, always two digits.
minuteDigits :: String -> [(Int, String)]
minuteDigits (a:b:rest) | a `elem` ['0'..'5'], isDigit b = [(read [a, b], rest)]
minuteDigits _ = []

startsWithSpace, startsWithWordChar :: String -> Bool
startsWithSpace (c:_) = isSpace c
startsWithSpace _     = False
startsWithWordChar (c:_) = isAlphaNum c || c == '_'
startsWithWordChar _     = False

-- | Every block of a note given an end: the end it was written with, or the end the vault gives a
-- block written without one.
--
-- A vault set to extend a block until the next one ends it where the next block of the same note
-- to start after it starts, and one set otherwise — or a block with nothing starting after it — has
-- it last the vault's default length. The next block is the next by time rather than by line,
-- because a note's blocks are a day's, and a day is in the order of its hours.
resolveEnds :: PlannerSettings -> [WrittenBlock] -> [(Minutes, Minutes)]
resolveEnds settings blocks = map (resolveEnd settings (map wbStart blocks)) blocks

-- | One block of a note given an end, out of the starts of every block of that note.
--
-- An end past midnight is the time it is on the next day, as a @\/Data\/Time\/TimeRange@ whose end
-- is before its start runs on past midnight.
resolveEnd :: PlannerSettings -> [Minutes] -> WrittenBlock -> (Minutes, Minutes)
resolveEnd _ _ (WrittenBlock start (Just end)) = (start, end)
resolveEnd settings starts (WrittenBlock start Nothing) = (start, fromMaybe byDefault next `mod` 1440)
  where
    byDefault = start + pstDefaultDuration settings
    next = if pstExtendUntilNext settings then listToMaybe (sort (filter (> start) starts)) else Nothing

-- | The day a line outside a daily note is scheduled for, as the plugin reads one: after an @⏳@, or
-- as a @scheduled@ inline field between brackets or between parentheses.
scheduledDay :: String -> Maybe Day
scheduledDay line = listToMaybe $ mapMaybe dayAfter (suffixesAfter "\x23F3" line ++ inlineValues "[" "]" ++ inlineValues "(" ")")
  where
    dayAfter rest = readIsoDay (take 10 (dropWhile isSpace rest))
    inlineValues open close = [v | rest <- suffixesAfter open line, Just v <- [scheduledField close rest]]
    scheduledField close rest = do
      afterKey <- stripPrefix "scheduled" rest
      afterColons <- stripPrefix "::" (dropWhile isSpace afterKey)
      let value = dropWhile isSpace afterColons
      guard $ close `isPrefixOf` drop 10 value
      return value

-- | Every rest of a string after an occurrence of a mark.
suffixesAfter :: String -> String -> [String]
suffixesAfter mark = go
  where
    go [] = []
    go str@(_:rest) = case stripPrefix mark str of
      Just after -> after : go rest
      Nothing    -> go rest

-- | The day a note is the daily note of, if it is one.
--
-- A daily note is a note within the vault's daily notes folder whose path from that folder, without
-- its extension, is written in the vault's daily note format. The format may write folders of its
-- own, as @YYYY/MM/YYYY-MM-DD@ does, which is why the whole path from the folder is read rather than
-- only the note's name.
noteDay :: DailyNoteSettings -> FilePath -> FilePath -> Maybe Day
noteDay DailyNoteSettings{dnsFolder, dnsFormat} vault note = do
  let folder = if null dnsFolder then vault else vault </> dnsFolder
  let relative = makeRelative folder note
  guard $ relative /= note && ".." `notElem` splitDirectories relative
  dayInFormat dnsFormat (dropExtension relative)

-- | The day a name written in a format names, if it is written in it exactly.
--
-- Every way of reading the name is tried, as a one-digit month may be followed by more digits, and
-- the name must be used up by the format. Where the format writes the same piece twice, as
-- @YYYY/YYYY-MM-DD@ does, the two must agree. A name naming no day that exists names none.
dayInFormat :: [DateToken] -> String -> Maybe Day
dayInFormat tokens name = listToMaybe $ do
  (pieces, "") <- readTokens tokens name
  [y] <- [distinct [v | ("y", v) <- pieces]]
  [m] <- [distinct [v | ("m", v) <- pieces]]
  [d] <- [distinct [v | ("d", v) <- pieces]]
  maybe [] return $ fromGregorianValid (fromIntegral y) m d
  where
    distinct = foldr (\v acc -> if v `elem` acc then acc else v : acc) []

-- | Every way a name is read by a run of format pieces, as what each piece read and what is left.
readTokens :: [DateToken] -> String -> [([(String, Int)], String)]
readTokens [] s = [([], s)]
readTokens (tok:toks) s = do
  (piece, rest) <- readToken tok s
  (pieces, rest') <- readTokens toks rest
  return (maybe pieces (: pieces) piece, rest')

-- | Every way one format piece is read at the start of a name.
readToken :: DateToken -> String -> [(Maybe (String, Int), String)]
readToken DYear4 s = [(Just ("y", read ds), rest) | (ds, rest) <- [splitAt 4 s], length ds == 4, all isDigit ds]
readToken DYear2 s = [(Just ("y", 2000 + read ds), rest) | (ds, rest) <- [splitAt 2 s], length ds == 2, all isDigit ds]
readToken DMonth2 s = [(Just ("m", read ds), rest) | (ds, rest) <- [splitAt 2 s], length ds == 2, all isDigit ds]
readToken DMonth1 s = [(Just ("m", n), rest) | (n, rest) <- oneOrTwoDigits s]
readToken DDay2 s = [(Just ("d", read ds), rest) | (ds, rest) <- [splitAt 2 s], length ds == 2, all isDigit ds]
readToken DDay1 s = [(Just ("d", n), rest) | (n, rest) <- oneOrTwoDigits s]
readToken DDayOrdinal s = [(Just ("d", n), rest') | (n, rest) <- oneOrTwoDigits s, suffix <- ["st", "nd", "rd", "th"], Just rest' <- [stripPrefix suffix rest]]
readToken DMonthName s = [(Just ("m", n), rest) | (n, name) <- zip [1 ..] monthNames, Just rest <- [stripPrefix name s]]
readToken DMonthShort s = [(Just ("m", n), rest) | (n, name) <- zip [1 ..] monthNames, Just rest <- [stripPrefix (take 3 name) s]]
readToken DWeekdayName s = [(Nothing, rest) | name <- weekdayNames, Just rest <- [stripPrefix name s]]
readToken DWeekdayShort s = [(Nothing, rest) | name <- weekdayNames, Just rest <- [stripPrefix (take 3 name) s]]
readToken (DLiteral lit) s = [(Nothing, rest) | Just rest <- [stripPrefix lit s]]

oneOrTwoDigits :: String -> [(Int, String)]
oneOrTwoDigits s = [(read ds, rest) | n <- [2, 1], (ds, rest) <- [splitAt n s], length ds == n, all isDigit ds]

-- | The names Moment writes months and weekdays with in English, which is what a vault's daily
-- notes are named with unless the vault set Obsidian to another language.
monthNames, weekdayNames :: [String]
monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
weekdayNames = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

-- | Where the daily note of a day is: the reversion of 'noteDay', naming the note in the vault's
-- daily notes folder with the day written in its format.
dailyNotePath :: DailyNoteSettings -> FilePath -> Day -> FilePath
dailyNotePath DailyNoteSettings{dnsFolder, dnsFormat} vault day = folder </> dayText dnsFormat day <.> "md"
  where folder = if null dnsFolder then vault else vault </> dnsFolder

-- | A day written in a daily note format: the reversion of 'dayInFormat'.
dayText :: [DateToken] -> Day -> String
dayText tokens day = concatMap piece tokens
  where
    (year, month, dayOfMonth) = toGregorian day
    piece DYear4        = printf "%04d" year
    piece DYear2        = printf "%02d" (year `mod` 100)
    piece DMonth2       = printf "%02d" month
    piece DMonth1       = show month
    piece DMonthName    = monthNames !! (month - 1)
    piece DMonthShort   = take 3 (monthNames !! (month - 1))
    piece DDay2         = printf "%02d" dayOfMonth
    piece DDay1         = show dayOfMonth
    piece DDayOrdinal   = show dayOfMonth ++ ordinal dayOfMonth
    piece DWeekdayName  = weekdayName
    piece DWeekdayShort = take 3 weekdayName
    piece (DLiteral l)  = l
    weekdayName = weekdayNames !! (fromEnum (dayOfWeek day) - fromEnum Monday)
    ordinal n
      | n `mod` 100 `elem` [11, 12, 13] = "th"
      | n `mod` 10 == 1 = "st"
      | n `mod` 10 == 2 = "nd"
      | n `mod` 10 == 3 = "rd"
      | otherwise = "th"

-- | A time of day written in the format the plugin writes one with.
timeText :: [TimeToken] -> Minutes -> String
timeText tokens minutes = concatMap piece tokens
  where
    (hour, minute) = (minutes `mod` 1440) `divMod` 60
    hour12 = if hour `mod` 12 == 0 then 12 else hour `mod` 12
    piece (THour24 width)    = padded width hour
    piece (THour12 width)    = padded width hour12
    piece (TMinute width)    = padded width minute
    piece (TMeridiem upper)  = (if upper then id else map toLower) (if hour < 12 then "AM" else "PM")
    piece (TLiteral l)       = l
    padded width n = let shown = show n in replicate (width - length shown) '0' ++ shown

-- | A line with the block of the day it writes at its start replaced, as the plugin rewrites a line
-- whose block it moved: the block written in place of the one the line wrote, or in front of what
-- the line says where it wrote none, and the block taken off where it is to have none.
--
-- A block is always written with its end, in the vault's format, as the plugin writes one. So a
-- line written without an end, or in the other clock, is written back the plugin's way once it is
-- moved, and left exactly as it was until then.
withBlock :: [TimeToken] -> Maybe (Minutes, Minutes) -> String -> String
withBlock format block line = case (block, timeRangeAtStart line) of
  (Just range, Just (_, rest)) -> written range rest
  (Just range, Nothing)        -> written range line
  (Nothing, Just (_, rest))    -> rest
  (Nothing, Nothing)           -> line
  where
    written (start, end) rest = timeText format start ++ " - " ++ timeText format end ++ (if null rest then "" else ' ' : rest)

-- | A line scheduled for another day: the day it schedules rewritten where it schedules one, in
-- whichever of the ways 'scheduledDay' reads it was written, and @⏳@ and the day written at its end
-- where it schedules none — before a block link, which has to stay the last thing on a line.
withScheduledDay :: Day -> String -> String
withScheduledDay day line = fromMaybe appended (rewritten line)
  where
    iso = showGregorian day
    rewritten [] = Nothing
    rewritten str@(c:rest)
      | Just after <- stripPrefix "\x23F3" str
      , (space, value) <- span isSpace after
      , isDay value = Just ("\x23F3" ++ space ++ iso ++ drop 10 value)
      | Just field <- inlineField "[" "]" str = Just field
      | Just field <- inlineField "(" ")" str = Just field
      | otherwise = (c :) <$> rewritten rest
    inlineField open close str = do
      afterOpen <- stripPrefix open str
      let (space1, keyed) = span isSpace afterOpen
      afterKey <- stripPrefix "scheduled" keyed
      let (space2, colons) = span isSpace afterKey
      afterColons <- stripPrefix "::" colons
      let (space3, value) = span isSpace afterColons
      guard $ isDay value && close `isPrefixOf` drop 10 value
      return (open ++ space1 ++ "scheduled" ++ space2 ++ "::" ++ space3 ++ iso ++ drop 10 value)
    isDay value = isJust (readIsoDay (take 10 value))
    appended = case blockLinkAtEnd line of
      Just (before, link) -> before ++ " \x23F3 " ++ iso ++ " " ++ link
      Nothing             -> line ++ " \x23F3 " ++ iso
    blockLinkAtEnd l = case break (== ' ') (reverse l) of
      (link@(_:_), ' ':before) | '^' == last link, all (\c -> isAlphaNum c || c == '-') (init link) -> Just (reverse (dropWhile isSpace before), reverse link)
      _ -> Nothing
