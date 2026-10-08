--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Query
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module reads the query language the Obsidian Tasks community plugin reads, which a note
-- writes in a block of code fenced as @tasks@ to ask about the tasks of its vault.
--
-- A query is a list of instructions, one to a line, and a line is one of three things: something a
-- task must be for the query to find it, an order to give what it finds, or a count to stop at.
-- Lines saying what a task must be are joined by @and@ — a query naming two things wants both.
--
-- Read line by line and by the words each line opens with, as 'Syntax.Obsidian.Task' reads a
-- checkbox by the marks written on it. Every instruction the plugin knows is a phrase written out
-- in full, so what reads one is a list of phrases and not a grammar; and what a reader has to be
-- told about a line they typed is which line it was, not which column of it.
--
-- What is read here and what a task IS are apart. 'QueryTask' is everything a filter can look at,
-- as plain Haskell, and whoever has tasks builds one per task — so this module reads the language
-- and answers about it while knowing nothing of how a note is compiled or what a task is compiled
-- to.
--
-- A relative date is resolved as the query is read rather than as it is run, against the day the
-- reading is given. So a query is a question about particular days by the time it is a query at
-- all, and the day it was asked on is asked for once.
--
-- What the plugin has and this does not is written down in @stack\/core\/note.ct@ beside the
-- annotation, since that is where a reader of the language meets it.
--------------------------------------------------------------------

module Syntax.Obsidian.Query where

import           Data.Char                (isDigit, isSpace, toLower)
import           Data.List                (isPrefixOf, sortOn)
import           Data.Maybe               (fromMaybe, isJust, listToMaybe,
                                           mapMaybe)
import           Data.Ord                 (Down (..))
import           Data.Time.Calendar       (Day, DayOfWeek (..), addDays,
                                           dayOfWeek, fromGregorian,
                                           gregorianMonthLength, toGregorian)
import           Syntax.Obsidian.Settings (StatusKind (..))
import           Syntax.Obsidian.Task     (TaskPriority (..), isDate, isInfix,
                                           readIsoDay, trim)

-- | One query: what a task must be to be found, the order to give what is found, and how much of it
-- to show.
--
-- No filter at all is every task, which is what an empty query asks for. No order is the order the
-- tasks were gathered in, which is page by page and, within a page, the order each note wrote them
-- — the plugin's own default is by urgency, which is not read here.
data TasksQuery = TasksQuery {
  tqFilter :: Maybe Filter,
  -- | The keys to order by, the first of them the one that decides and the rest breaking its ties.
  tqSort   :: [SortKey],
  tqLimit  :: Maybe Int
                             }
  deriving (Eq, Show)

-- | Everything a task must be for a query to find it.
--
-- Every leaf carries whether it is the thing or the absence of it, rather than being wrapped in
-- 'FNot', because that is how the plugin writes them — @does not include@ is one instruction and
-- not a negated one.
data Filter
  = FAnd Filter Filter
  | FOr Filter Filter
  | FXor Filter Filter
  | FNot Filter
  -- | @done@, which is a task finished, dropped or never a task; and @not done@, which is the rest.
  | FDone Bool
  | FStatusType Bool StatusKind
  | FStatusName Bool String
  | FDate DateField DateOp
  | FHasDate Bool DateField
  | FPriority PriorityOp TaskPriority
  | FRecurring Bool
  | FDescription Bool String
  | FTags Bool String
  | FHasTags Bool
  | FPath PathField Bool String
  -- | @exclude sub-items@, which keeps the tasks a note wrote at its own level.
  | FTopLevel
  deriving (Eq, Show)

-- | Which of a task's dates a filter or an order is about. 'DHappens' is not a date a task carries
-- but the soonest of the three that say when it is to be done.
data DateField = DDue | DScheduled | DStart | DCreated | DDone | DCancelled | DHappens
  deriving (Eq, Show)

-- | A span of days a date is asked about, both ends within it. A query naming one day is a span of
-- that day alone, which is what makes @due before 2026-01-04@ and @due before this week@ the one
-- question asked of two spans.
data DateRange = DateRange Day Day
  deriving (Eq, Show)

-- | Where a task's date must fall against the span asked about.
--
-- Named as the plugin names them for a single day. The same five, written @in@ for @on@, are what
-- it offers for a span, and they mean this against either.
data DateOp
  = DateOn DateRange
  | DateBefore DateRange
  | DateAfter DateRange
  | DateOnOrBefore DateRange
  | DateOnOrAfter DateRange
  deriving (Eq, Show)

data PriorityOp = PrioIs | PrioIsNot | PrioAbove | PrioBelow
  deriving (Eq, Show)

-- | Which reading of where a task was written a filter is about, as the plugin names them: the whole
-- path within the vault, the folder holding it, the name of the file, or the first folder of the
-- path.
data PathField = PathWhole | PathFolder | PathFilename | PathRoot
  deriving (Eq, Show)

-- | One key to order by, and whether the order it gives is to be turned around.
--
-- Reversing turns the whole of the result around rather than only the key, so a key which sorts the
-- tasks lacking it last sorts them first when it is reversed. That is the plugin's own reading of
-- @reverse@ and is what a reader writing it expects.
data SortKey = SortKey SortField Bool
  deriving (Eq, Show)

data SortField
  = SortStatus
  | SortStatusName
  | SortStatusType
  | SortPriority
  | SortRecurring
  | SortDescription
  | SortPath
  | SortFilename
  | SortDate DateField
  -- | Which of a task's tags to order by, counted from one as the plugin counts them.
  | SortTag Int
  deriving (Eq, Show)

-- | Everything about one task that a query can ask about.
--
-- Plain Haskell and not a value of the program the task was compiled from: what a task is compiled
-- to is the business of whoever gathers them, and what a query may ask is this. A task read without
-- the plugin's own reading of its line fills in what it can — what it says, whether it is done, and
-- where it was written — and carries no dates, no priority and no recurrence, so a query asking
-- about those finds it as a task which has none rather than not at all.
data QueryTask = QueryTask {
  qtStatusKind  :: StatusKind,
  -- | What the vault calls the status the box wears, which is its own to name.
  qtStatusName  :: String,
  qtDescription :: String,
  qtTags        :: [String],
  qtPriority    :: TaskPriority,
  qtDue         :: Maybe Day,
  qtScheduled   :: Maybe Day,
  qtStart       :: Maybe Day,
  qtCreated     :: Maybe Day,
  qtDone        :: Maybe Day,
  qtCancelled   :: Maybe Day,
  qtRecurring   :: Bool,
  -- | Where the note holding it is written, as the path within the vault.
  qtPath        :: FilePath,
  -- | Whether it is written under another task rather than at the note's own level.
  qtSubItem     :: Bool
                           }
  deriving (Eq, Show)

-- | A query saying nothing, which finds every task in the order they were gathered.
emptyQuery :: TasksQuery
emptyQuery = TasksQuery Nothing [] Nothing

-- ---------------------------------------------------------------------------------------------
-- Reading a query
-- ---------------------------------------------------------------------------------------------

-- | The query a block of instructions is, or every line of it which was not understood.
--
-- Every bad line is answered rather than the first, because a reader who typed two mistakes wants
-- to be told about two. A line saying nothing, and a line the plugin reads as a comment, are passed
-- over.
--
-- The day is what every relative date in the query is resolved against, so a query is a question
-- about particular days once it has been read at all.
parseQuery :: Day -> String -> Either [String] TasksQuery
parseQuery today src = case [l | (l, Nothing) <- read'] of
  []  -> Right $ foldl addInstruction emptyQuery [i | (_, Just i) <- read']
  bad -> Left $ map notUnderstood bad
  where
    read' = [(l, parseInstruction today l) | l <- lines src, not (isComment l)]
    isComment l = null (trim l) || "#" `isPrefixOf` trim l

-- | What a reader is told about a line of theirs which was not understood.
notUnderstood :: String -> String
notUnderstood l = "do not understand the query instruction: " ++ trim l

-- | One instruction of a query.
data Instruction = IFilter Filter | ISort SortKey | ILimit Int
  deriving (Eq, Show)

-- | The query a query and one more instruction make.
--
-- Filters are joined by @and@: a query naming two things a task must be wants a task which is both.
-- Orders are kept in the order they were written, the first deciding and the rest breaking its ties.
-- A second count replaces the first, since a query can only stop once.
addInstruction :: TasksQuery -> Instruction -> TasksQuery
addInstruction q (IFilter f) = q{tqFilter = Just $ maybe f (`FAnd` f) (tqFilter q)}
addInstruction q (ISort k)   = q{tqSort = tqSort q ++ [k]}
addInstruction q (ILimit n)  = q{tqLimit = Just n}

parseInstruction :: Day -> String -> Maybe Instruction
parseInstruction today l =
  (ISort <$> parseSort l) `orElse` (ILimit <$> parseLimit l) `orElse` (IFilter <$> parseFilter today l)

-- | How much of what it finds a query shows. @limit@ and @limit to N tasks@ are the one instruction.
parseLimit :: String -> Maybe Int
parseLimit l = do
  rest <- stripPhrase "limit" l
  let count = fromMaybe rest $ stripPhrase "to" rest
  case words count of
    [n]              | all isDigit n, not (null n) -> Just (read n)
    [n, "task"]      | all isDigit n, not (null n) -> Just (read n)
    [n, "tasks"]     | all isDigit n, not (null n) -> Just (read n)
    _                                              -> Nothing

-- | The order an instruction asks for, if it asks for one.
parseSort :: String -> Maybe SortKey
parseSort l = do
  rest <- stripPhrase "sort by" l
  let (field, reversed) = case stripLastWord "reverse" rest of
        Just before -> (before, True)
        Nothing     -> (rest, False)
  (`SortKey` reversed) <$> parseSortField (trim field)

parseSortField :: String -> Maybe SortField
parseSortField f = lookup (map toLower f) named `orElse` taggedField
  where
    named = [
      ("status", SortStatus), ("status.name", SortStatusName), ("status.type", SortStatusType),
      ("priority", SortPriority), ("recurring", SortRecurring),
      ("description", SortDescription), ("path", SortPath), ("filename", SortFilename),
      ("tag", SortTag 1)
             ] ++ [(name, SortDate field) | (name, field) <- sortDateFields]
    -- @sort by tag 2@ orders by the second tag a task wrote, and so on.
    taggedField = case words (map toLower f) of
      ["tag", n] | all isDigit n, not (null n), read n > (0 :: Int) -> Just $ SortTag (read n)
      _ -> Nothing

-- | What a task's dates are called in an order, which is not quite what they are called in a filter
-- — the plugin writes @starts before@ but @sort by start@.
sortDateFields :: [(String, DateField)]
sortDateFields = [("due", DDue), ("scheduled", DScheduled), ("start", DStart), ("created", DCreated), ("done", DDone), ("cancelled", DCancelled), ("happens", DHappens)]

-- ---------------------------------------------------------------------------------------------
-- Reading what a task must be
-- ---------------------------------------------------------------------------------------------

-- | What a task must be for a line to find it.
--
-- The joining words are read before the leaves, so that a line joining two of them is read as the
-- joining and never as a leaf which happens to hold the word @and@. Each side of a joining is
-- written between delimiters, which is what tells the words of the language from the words of a
-- search.
parseFilter :: Day -> String -> Maybe Filter
parseFilter today = parseOr . trim
  where
    -- Weakest first, so that the weakest joining is the outermost: @a AND b OR c@ is @(a AND b) OR
    -- c@. Each level looks for its own word outside every delimiter and falls to the next.
    --
    -- @a AND NOT b@ needs nothing of its own: what follows the joining is read by the level below,
    -- which is the one that reads a negation.
    parseOr s = joinedAt "OR" s (\a b -> FOr <$> parseOr a <*> parseXor b) `orElse` parseXor s
    parseXor s = joinedAt "XOR" s (\a b -> FXor <$> parseXor a <*> parseAnd b) `orElse` parseAnd s
    parseAnd s = joinedAt "AND" s (\a b -> FAnd <$> parseAnd a <*> parseNot b) `orElse` parseNot s
    parseNot s = case stripWord "NOT" s of
      Just rest -> FNot <$> parseNot (trim rest)
      Nothing   -> parseGroup s

    -- A side written between delimiters is a query of its own; one written without them is a leaf.
    parseGroup s = case delimited s of
      Just inner -> parseFilter today inner
      Nothing    -> parseLeaf today s

    joinedAt op s build = listToMaybe [r | (before, after) <- splitsAt op s
                                         , Just r <- [build (trim before) (trim after)]]

-- | Every way a line divides at a joining word written outside its delimiters, rightmost first so
-- that a line joining three things is read as joining the last of them onto the rest.
--
-- Rightmost is what makes the joinings read left to right, which is how the plugin reads them.
splitsAt :: String -> String -> [(String, String)]
splitsAt op s = reverse [(take i s, drop (i + length op) s) | (i, depth) <- positions
                                                            , depth == (0 :: Int)
                                                            , wordAt i]
  where
    positions = zip [0 ..] $ scanl step 0 s
    step d c | c `elem` ['(', '[', '{'] = d + 1
             | c `elem` [')', ']', '}'] = d - 1
             | otherwise = d
    -- A joining word is written as a word of its own, in capitals, so that a search for the word
    -- @and@ is not read as one.
    wordAt i = op `isPrefixOf` drop i s
               && (i == 0 || isSpace (s !! (i - 1)))
               && maybe True isSpace (listToMaybe $ drop (i + length op) s)

-- | What is written between a matched pair of delimiters, where the whole of a line is written
-- between one pair.
--
-- Every pair on a line is of the one kind, which is the plugin's own rule and is what lets a search
-- holding a bracket be written between quotes.
delimited :: String -> Maybe String
delimited s = listToMaybe [inner | (open, close) <- [('(', ')'), ('[', ']'), ('{', '}'), ('"', '"')]
                                 , Just inner <- [between open close]]
  where
    between open close = case s of
      (c:rest) | c == open, not (null rest), last rest == close, closesAtEnd open close (init rest) -> Just (init rest)
      _ -> Nothing
    -- The delimiter which opened the line is the one that closes it, rather than an earlier one:
    -- @(a) AND (b)@ is not one side written between brackets.
    closesAtEnd open close inner
      | open == close = close `notElem` inner
      | otherwise = all (>= 1) $ scanl step 1 inner
      where step d c | c == open = d + 1
                     | c == close = d - 1
                     | otherwise = d :: Int

-- | What a task must be, for a line naming one thing.
parseLeaf :: Day -> String -> Maybe Filter
parseLeaf today l = listToMaybe $ mapMaybe ($ trim l) [
  exact "done" (FDone True),
  exact "not done" (FDone False),
  exact "is recurring" (FRecurring True),
  exact "is not recurring" (FRecurring False),
  exact "no tags" (FHasTags False),
  exact "has tags" (FHasTags True),
  exact "exclude sub-items" FTopLevel,
  statusTypeLeaf,
  textLeaf "status.name" FStatusName,
  textLeaf "description" FDescription,
  tagsLeaf,
  pathLeaf,
  priorityLeaf,
  hasDateLeaf,
  dateLeaf today
                                                     ]
  where
    exact phrase f s = if map toLower (trim s) == phrase then Just f else Nothing

-- | @status.type is TODO@, and the six other kinds the plugin knows.
--
-- The seventh kind a status may be here is a box holding nothing at all, which the plugin has no
-- name for and so no query can name.
statusTypeLeaf :: String -> Maybe Filter
statusTypeLeaf l = do
  rest <- stripPhrase "status.type" l
  (wanted, kind') <- negatable "is" "is not" rest
  FStatusType wanted <$> lookup (map toLower $ trim kind') statusKindNames

-- | What each kind of status is written as in a query.
statusKindNames :: [(String, StatusKind)]
statusKindNames = [("todo", KTodo), ("done", KDone), ("in_progress", KInProgress), ("on_hold", KOnHold), ("cancelled", KCancelled), ("non_task", KNonTask)]

-- | @description includes something@, and the other searches written that way.
textLeaf :: String -> (Bool -> String -> Filter) -> String -> Maybe Filter
textLeaf field build l = do
  rest <- stripPhrase field l
  (wanted, needle) <- negatable "includes" "does not include" rest
  let needle' = trim needle
  if null needle' then Nothing else Just $ build wanted needle'

-- | @tags include #work@, which the plugin also lets a reader write in the singular.
tagsLeaf :: String -> Maybe Filter
tagsLeaf l = plural `orElse` singular
  where
    plural = do
      rest <- stripPhrase "tags" l
      (wanted, needle) <- negatable "include" "do not include" rest
      wanting wanted needle
    singular = do
      rest <- stripPhrase "tag" l
      (wanted, needle) <- negatable "includes" "does not include" rest
      wanting wanted needle
    wanting wanted needle = if null (trim needle) then Nothing else Just $ FTags wanted (trim needle)

-- | @path includes Projects@, and the three other readings of where a task was written.
pathLeaf :: String -> Maybe Filter
pathLeaf l = listToMaybe $ mapMaybe field [("path", PathWhole), ("folder", PathFolder), ("filename", PathFilename), ("root", PathRoot)]
  where
    field (name, reading) = textLeaf name (FPath reading) l

-- | @priority is above low@, and the same without a word between.
priorityLeaf :: String -> Maybe Filter
priorityLeaf l = do
  rest <- stripPhrase "priority is" l
  let (op, named) = case [(o, r) | (phrase, o) <- ops, Just r <- [stripPhrase phrase rest]] of
        ((o, r):_) -> (o, r)
        []         -> (PrioIs, rest)
  FPriority op <$> lookup (map toLower $ trim named) priorityNames
  where
    ops = [("above", PrioAbove), ("below", PrioBelow), ("not", PrioIsNot)]

-- | What each priority is written as in a query, from the most urgent down.
priorityNames :: [(String, TaskPriority)]
priorityNames = [("highest", PHighest), ("high", PHigh), ("medium", PMedium), ("none", PNone), ("low", PLow), ("lowest", PLowest)]

-- | @has due date@ and @no due date@.
hasDateLeaf :: String -> Maybe Filter
hasDateLeaf l = listToMaybe [FHasDate wanted field | (wanted, opener) <- [(True, "has"), (False, "no")]
                                                   , Just rest <- [stripPhrase opener l]
                                                   , (name, field) <- sortDateFields
                                                   , Just after <- [stripPhrase name rest]
                                                   , map toLower (trim after) == "date"]

-- | @due before 2026-01-04@, and every other way of asking where one of a task's dates falls.
dateLeaf :: Day -> String -> Maybe Filter
dateLeaf today l = listToMaybe [f | (name, field) <- dateFilterFields
                                  , Just rest <- [stripPhrase name l]
                                  , Just f <- [FDate field <$> dateOp today rest]]

-- | What a task's dates are called in a filter. @starts@ rather than @start@, which is what the
-- plugin writes there and only there.
dateFilterFields :: [(String, DateField)]
dateFilterFields = [("due", DDue), ("scheduled", DScheduled), ("starts", DStart), ("created", DCreated), ("done", DDone), ("cancelled", DCancelled), ("happens", DHappens)]

-- | Where a date must fall, out of the words after the name of the date.
--
-- The longer phrases are read before the shorter ones, so @on or before@ is never read as @on@
-- followed by a span nobody could resolve. A line naming no word at all asks for the span itself,
-- which is what @on@ and @in@ both mean and is why the plugin lets either be left out.
dateOp :: Day -> String -> Maybe DateOp
dateOp today rest = listToMaybe [op range | (phrase, op) <- ops
                                          , Just after <- [stripPhrase phrase rest]
                                          , Just range <- [resolveRange today after]]
                    `orElse` (DateOn <$> resolveRange today rest)
  where
    ops = [("on or before", DateOnOrBefore), ("on or after", DateOnOrAfter),
           ("in or before", DateOnOrBefore), ("in or after", DateOnOrAfter),
           ("before", DateBefore), ("after", DateAfter),
           ("on", DateOn), ("in", DateOn)]

-- ---------------------------------------------------------------------------------------------
-- Reading a date
-- ---------------------------------------------------------------------------------------------

-- | The span of days a query names, resolved against the day it was asked on.
--
-- A day named on its own is a span of that day alone, so everything asking about a span asks the
-- same question of one.
--
-- A small vocabulary and not the plugin's whole one. What is missing is written down in
-- @stack\/core\/note.ct@; a word from outside it is a line the reader is told about rather than a
-- day quietly guessed at.
resolveRange :: Day -> String -> Maybe DateRange
resolveRange today s = case words (map toLower $ trim s) of
  [from, to] | isDate from, isDate to -> DateRange <$> readDate from <*> readDate to
  ws                                  -> resolveWords today ws

resolveWords :: Day -> [String] -> Maybe DateRange
resolveWords today ws = case ws of
  [d] | isDate d -> only <$> readDate d
  ["today"]      -> Just $ only today
  ["tomorrow"]   -> Just $ only (addDays 1 today)
  ["yesterday"]  -> Just $ only (addDays (-1) today)
  [day] | Just dow <- weekday day -> Just $ only $ nearestWeekday today dow
  ["next", day] | Just dow <- weekday day -> Just $ only $ onwardWeekday 1 today dow
  ["last", day] | Just dow <- weekday day -> Just $ only $ onwardWeekday (-1) today dow
  [which, span'] | Just shift <- lookup which shifts -> spanOf shift span'
  _ -> Nothing
  where
    only d = DateRange d d
    shifts = [("last", -1), ("this", 0), ("next", 1 :: Integer)]
    spanOf shift "week" = Just $ let start = addDays (7 * shift) (weekStart today) in DateRange start (addDays 6 start)
    spanOf shift "month" = Just $ monthSpan (shiftMonths shift today)
    spanOf shift "quarter" = Just $ quarterSpan (shiftMonths (3 * shift) today)
    spanOf shift "year" = Just $ yearSpan (year today + shift)
    spanOf _ _ = Nothing

-- | A date written out as @YYYY-MM-DD@, which is how a task writes one and how a query is best
-- written.
readDate :: String -> Maybe Day
readDate = readIsoDay

-- | Which day of the week a word names, if it names one.
weekday :: String -> Maybe DayOfWeek
weekday w = lookup (map toLower w) [
  ("monday", Monday), ("tuesday", Tuesday), ("wednesday", Wednesday), ("thursday", Thursday),
  ("friday", Friday), ("saturday", Saturday), ("sunday", Sunday)
                                   ]

-- | The nearest day of that name, which is today where today is one.
--
-- Nearest rather than next, because that is what the plugin's own reading does: written on a
-- Wednesday, @tuesday@ is yesterday, and a reader who meant the one ahead writes @next tuesday@.
nearestWeekday :: Day -> DayOfWeek -> Day
nearestWeekday today dow
  | back == 0 = today
  | back <= forward = addDays (negate back) today
  | otherwise = addDays forward today
  where
    back = toInteger $ (fromEnum (dayOfWeek today) - fromEnum dow) `mod` 7
    forward = toInteger $ (fromEnum dow - fromEnum (dayOfWeek today)) `mod` 7

-- | The next day of that name after today, or the last one before it, never today itself.
onwardWeekday :: Integer -> Day -> DayOfWeek -> Day
onwardWeekday step today dow = head [d | n <- [1 ..], let d = addDays (step * n) today, dayOfWeek d == dow]

-- | The Monday of the week holding a day, weeks running Monday to Sunday.
weekStart :: Day -> Day
weekStart d = addDays (negate $ toInteger $ (fromEnum (dayOfWeek d) - fromEnum Monday) `mod` 7) d

monthSpan :: Day -> DateRange
monthSpan d = DateRange (fromGregorian y m 1) (fromGregorian y m (gregorianMonthLength y m))
  where (y, m, _) = toGregorian d

-- | The three months a day's quarter is, which run from January, April, July and October.
quarterSpan :: Day -> DateRange
quarterSpan d = DateRange from to
  where
    (y, m, _) = toGregorian d
    first = 1 + 3 * ((m - 1) `div` 3)
    DateRange from _ = monthSpan (fromGregorian y first 1)
    DateRange _ to = monthSpan (fromGregorian y (first + 2) 1)

yearSpan :: Integer -> DateRange
yearSpan y = DateRange (fromGregorian y 1 1) (fromGregorian y 12 31)

-- | The year a day is in.
year :: Day -> Integer
year d = let (y, _, _) = toGregorian d in y

-- | The day some months away, clipped to the end of the month it lands in.
shiftMonths :: Integer -> Day -> Day
shiftMonths n d = fromGregorian y' m' 1
  where
    (y, m, _) = toGregorian d
    total = toInteger (m - 1) + n
    y' = y + total `div` 12
    m' = fromInteger (total `mod` 12) + 1

-- ---------------------------------------------------------------------------------------------
-- Answering a query
-- ---------------------------------------------------------------------------------------------

-- | The tasks a query finds, in the order it asks for, cut to the count it asks for.
--
-- Each task is given with whatever it was carried alongside, so that whoever gathered them keeps
-- what they know about each. Filtering, then ordering, then cutting: a count is of what was found
-- and ordered, so a query asking for the ten soonest gets the soonest ten rather than ten of them
-- sorted.
applyQuery :: TasksQuery -> [(QueryTask, a)] -> [(QueryTask, a)]
applyQuery TasksQuery{tqFilter, tqSort, tqLimit} = cut . order . filter (matches . fst)
  where
    matches t = maybe True (`matchesFilter` t) tqFilter
    -- 'sortOn' is stable, so the keys are applied last to first: the last key breaks the ties of the
    -- one before it, and the first decides. Which is what 'foldr' does, the last key being the
    -- innermost.
    order ts = foldr byKey ts tqSort
    byKey (SortKey field reversed) ts
      | reversed = sortOn (Down . sortValue field . fst) ts
      | otherwise = sortOn (sortValue field . fst) ts
    cut = maybe id take tqLimit

-- | Whether one task is what a filter asks for.
matchesFilter :: Filter -> QueryTask -> Bool
matchesFilter f t = case f of
  FAnd a b -> matchesFilter a t && matchesFilter b t
  FOr a b -> matchesFilter a t || matchesFilter b t
  FXor a b -> matchesFilter a t /= matchesFilter b t
  FNot a -> not (matchesFilter a t)
  FDone wanted -> isDoneKind (qtStatusKind t) == wanted
  FStatusType wanted kind -> (qtStatusKind t == kind) == wanted
  FStatusName wanted needle -> holds needle (qtStatusName t) == wanted
  FDate field op -> maybe False (withinOp op) (dateOf field t)
  FHasDate wanted field -> isJust (dateOf field t) == wanted
  FPriority op wanted -> priorityMatches op (qtPriority t) wanted
  FRecurring wanted -> qtRecurring t == wanted
  FDescription wanted needle -> holds needle (qtDescription t) == wanted
  FTags wanted needle -> any (holds needle) (qtTags t) == wanted
  FHasTags wanted -> not (null (qtTags t)) == wanted
  FPath reading wanted needle -> holds needle (pathReading reading (qtPath t)) == wanted
  FTopLevel -> not (qtSubItem t)
  where
    -- Every search of a query's is for what is written anywhere within, whatever it was capitalised
    -- as. That is the plugin's own reading of @includes@ and is what has a reader find what they
    -- meant without knowing how they wrote it.
    holds needle hay = map toLower needle `isInfix` map toLower hay

-- | Whether a kind of status is one the plugin counts as done, which is a task finished, one
-- dropped, and one that was never a task. The rest — to do, begun, and waiting — are not done.
--
-- A box holding nothing at all is the kind the plugin has no name for, and is not done.
isDoneKind :: StatusKind -> Bool
isDoneKind k = k `elem` [KDone, KCancelled, KNonTask]

-- | Which of a task's dates a field names. @happens@ is the soonest of the three saying when a task
-- is to be done, which is what the plugin means by it.
dateOf :: DateField -> QueryTask -> Maybe Day
dateOf DDue       = qtDue
dateOf DScheduled = qtScheduled
dateOf DStart     = qtStart
dateOf DCreated   = qtCreated
dateOf DDone      = qtDone
dateOf DCancelled = qtCancelled
dateOf DHappens   = \t -> soonest [qtStart t, qtScheduled t, qtDue t]

soonest :: [Maybe Day] -> Maybe Day
soonest ds = case [d | Just d <- ds] of
  [] -> Nothing
  xs -> Just (minimum xs)

-- | Whether a date falls where an op asks it to.
withinOp :: DateOp -> Day -> Bool
withinOp (DateOn (DateRange from to)) d       = d >= from && d <= to
withinOp (DateBefore (DateRange from _)) d    = d < from
withinOp (DateAfter (DateRange _ to)) d       = d > to
withinOp (DateOnOrBefore (DateRange _ to)) d  = d <= to
withinOp (DateOnOrAfter (DateRange from _)) d = d >= from

-- | Whether a task's priority is what a filter asks for. @above@ is more urgent and @below@ less,
-- which is the way round a reader means them and the opposite of the way the priorities are counted.
priorityMatches :: PriorityOp -> TaskPriority -> TaskPriority -> Bool
priorityMatches PrioIs have wanted    = have == wanted
priorityMatches PrioIsNot have wanted = have /= wanted
priorityMatches PrioAbove have wanted = priorityRank have < priorityRank wanted
priorityMatches PrioBelow have wanted = priorityRank have > priorityRank wanted

-- | How urgent a priority is, counted from the most urgent. The order the priorities are declared in
-- is the order they rank in, with none between medium and low, so this is where each sits in it.
priorityRank :: TaskPriority -> Int
priorityRank p = length $ takeWhile (/= p) [PHighest, PHigh, PMedium, PNone, PLow, PLowest]

-- | Which reading of a path a filter is about, each as the plugin gives it: the whole path within
-- the vault, the folder holding the file with a slash after it, the name of the file with its
-- extension, and the first folder of the path. A file at the top of the vault has @\/@ for both its
-- folder and its root.
pathReading :: PathField -> FilePath -> String
pathReading PathWhole p = p
pathReading PathFilename p = reverse $ takeWhile ('/' /=) (reverse p)
pathReading PathFolder p = case break ('/' ==) (reverse p) of
  (_, _:folder) -> reverse folder ++ "/"
  _             -> "/"
pathReading PathRoot p = case break ('/' ==) p of
  (root, _:_) -> root ++ "/"
  _           -> "/"

-- | What a task is worth under one key of an order.
--
-- One key gives one shape for every task, so two of them are never compared against each other.
data SortValue
  = SVRank Int
  | SVText (Maybe String)
  | SVDate (Maybe Day)
  deriving (Eq, Show)

-- | A task with nothing to give a key sorts after every task that has something. Reversing an order
-- turns the whole of the result around, so reversed it sorts them first — which is the plugin's own
-- reading and what a reader writing @reverse@ expects.
--
-- Text is compared however it was capitalised, since a reader ordering by what a task says means
-- the words rather than the capitals.
instance Ord SortValue where
  compare (SVRank a) (SVRank b) = compare a b
  compare (SVText a) (SVText b) = compare (folded a) (folded b)
    where folded = missingLast . fmap (map toLower)
  compare (SVDate a) (SVDate b) = compare (missingLast a) (missingLast b)
  compare a b = compare (shape a) (shape b)
    where
      shape SVRank{} = 0 :: Int
      shape SVText{} = 1
      shape SVDate{} = 2

-- | Nothing after everything, which is the opposite of how a 'Maybe' compares.
missingLast :: Maybe a -> (Int, Maybe a)
missingLast = maybe (1, Nothing) (\v -> (0, Just v))

sortValue :: SortField -> QueryTask -> SortValue
sortValue SortStatus t = SVRank $ if isDoneKind (qtStatusKind t) then 1 else 0
sortValue SortStatusName t = SVText $ Just $ qtStatusName t
sortValue SortStatusType t = SVRank $ statusKindRank (qtStatusKind t)
sortValue SortPriority t = SVRank $ priorityRank (qtPriority t)
sortValue SortRecurring t = SVRank $ if qtRecurring t then 0 else 1
sortValue SortDescription t = SVText $ Just $ qtDescription t
sortValue SortPath t = SVText $ Just $ qtPath t
sortValue SortFilename t = SVText $ Just $ pathReading PathFilename (qtPath t)
sortValue (SortDate field) t = SVDate $ dateOf field t
-- A task which wrote fewer tags than that has nothing to give the key.
sortValue (SortTag n) t = SVText $ listToMaybe $ drop (n - 1) (qtTags t)

-- | Where each kind of status sorts, which is the plugin's own order rather than the order they are
-- declared in: what is being done, then what is to be done, then what is waiting, then what is
-- finished, dropped, or was never a task. A task worth acting on comes first, which is what makes an
-- order by status worth asking for at all.
statusKindRank :: StatusKind -> Int
statusKindRank k = length $ takeWhile (/= k) [KInProgress, KTodo, KOnHold, KDone, KCancelled, KNonTask, KEmpty]

-- ---------------------------------------------------------------------------------------------
-- Reading words
-- ---------------------------------------------------------------------------------------------

-- | What is left of a line after the words a phrase is written as, if it opens with them.
--
-- Word by word rather than character by character, so that a phrase is never found inside a longer
-- word: a line saying @tasksmore@ does not open with @tasks@. What is left is given back as it was
-- written, without the space before it, so a search keeps whatever the reader typed.
stripPhrase :: String -> String -> Maybe String
stripPhrase phrase = go (words phrase)
  where
    go [] s = Just $ dropWhile isSpace s
    go (w:ws) s = case splitAt (length w) (dropWhile isSpace s) of
      (found, rest) | map toLower found == map toLower w
                    , null rest || isSpace (head rest) -> go ws rest
      _ -> Nothing

-- | What is left of a line before a word, if it ends with it. The word an order is turned around by
-- is written at the end of the instruction, so it is taken off from there.
stripLastWord :: String -> String -> Maybe String
stripLastWord w s = reverse <$> stripPhrase (reverse w) (reverse s)

-- | The same for a word which has to be written as it is written here.
--
-- Which is every word that joins two things a task must be. The plugin writes them in capitals so
-- that a search for the word @not@ is a search rather than a negation, and reading one however it
-- was capitalised would take @not done@ — an instruction of its own — for a negated @done@.
stripWord :: String -> String -> Maybe String
stripWord w s = case splitAt (length w) s of
  (found, rest) | found == w, null rest || isSpace (head rest) -> Just $ dropWhile isSpace rest
  _ -> Nothing

-- | The two ways an instruction is written — the thing, and the absence of it — read as which was
-- written and what was written after it.
--
-- The longer phrase is read first, since the shorter is written inside it.
negatable :: String -> String -> String -> Maybe (Bool, String)
negatable positive negative s =
  ((False,) <$> stripPhrase negative s) `orElse` ((True,) <$> stripPhrase positive s)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse a b = maybe b Just a
