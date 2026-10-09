--------------------------------------------------------------------
-- |
-- Module    :  Eval.Runtime.Calendar
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the primitive backing the calendar, which is declared in
-- @stack\/core\/calendar.ct@.
--
-- What a calendar shows is not written down here. It gathers the values which are members of
-- @\/Data\/Calendar\/CalendarEntry@, whatever those turn out to be, and asks each of them
-- @calendarDay@ — so a kind of thing joins a calendar by saying in @.ct@ that it belongs and
-- answering that question, and nothing in this module changes. That is the difference between this
-- and "Eval.Runtime.Tasks", which names the two readings of a task because it is the module of the
-- library that knows what a task is.
--------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Eval.Runtime.Calendar where

import           CRes                 (runCResT)
import           CtConstants          (calendarDayMethod, calendarEntryClass,
                                       calendarTimeMethod, eventClass,
                                       eventDayMethod, eventSpanMethod)
import           Data.Aeson           (ToJSON)
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.List            (find, nub, sortOn)
import           Data.Maybe           (mapMaybe)
import           Data.Time.Calendar   (Day, showGregorian)
import qualified Data.UUID            as UUID
import           Eval.Common
import           Eval.Runtime.WebDocs (askService, encodeJsonText, noService,
                                       orNotes, traverseUntilLeft)
import           GHC.Generics         (Generic)
import           Semantics.Prgm
import           Semantics.Types
import           Text.Printf
import           Utils

-- | The primitives backing the @Data/Calendar@ displays.
calendarPrims :: [EPrim]
calendarPrims = [calendarEntries, plannerEntries]

-- | One thing a calendar shows: the day it sits on, the value it displays as, and where it was
-- written.
--
-- 'ciValue' is the entry itself and not a rendering of it, so whatever shows that kind of value
-- shows it here too — a task on a calendar and the same task on its note are one value shown one
-- way. It is also what makes a calendar show a kind of thing nothing here knows about: the value
-- goes out as it is and whatever knows how to draw one draws it.
--
-- 'ciPage' and 'ciExpr' are what @\/api\/write-expr@ replaces, as 'Eval.Runtime.Tasks.PageTask'
-- carries them and for its reason: an entry shown on a calendar is not shown on its own page, so it
-- carries the page it came from and a change to it is written there.
--
-- 'ciDay' is absent for an entry which sits on no day. Such an entry is carried rather than dropped,
-- because the gather found it and a calendar quietly showing less than it was handed would answer a
-- narrower question than the one asked.
--
-- 'ciTime' is the span of its day an entry takes up, and is absent for one which takes up the whole
-- day it sits on.
data CalendarItem = CalendarItem { ciPage  :: String
                                 , ciExpr  :: String
                                 , ciDay   :: Maybe String
                                 , ciTime  :: Maybe CalendarTime
                                 , ciValue :: Val
                                 } deriving (Generic, ToJSON)

-- | The span of a day an entry takes up, as the @HH:mm@ times it starts and ends at. An end at or
-- before the start runs on past midnight, as a @\/Data\/Time\/TimeRange@ does.
data CalendarTime = CalendarTime { ctStart :: String
                                 , ctEnd   :: String
                                 } deriving (Generic, ToJSON)

-- | What a calendar found, or why it found nothing.
--
-- Both in the one answer, as @Show/TaskQuery@ carries them and for its reason: a calendar which
-- cannot be answered is an ordinary answer and not a failure of the page around it.
--
-- 'crToday' is the day the evaluation was given, which travels with the answer rather than being
-- left to whoever draws the calendar. A browser's own clock is a different clock in a different
-- place, and the day a calendar marks has to be the day the answer it is drawing was computed on.
data CalendarResult = CalendarResult { crError :: Maybe String
                                     , crToday :: Day
                                     , crItems :: [CalendarItem]
                                     } deriving (Generic, ToJSON)

-- | Backs @/Data/Calendar/calendarEntries@, the data carried by a @Show/Calendar@ display. It
-- renders everything the evaluation covers which sits on a day, as JSON: each the value it displays
-- as, the day it sits on, and the page and expression it was written as.
--
-- Which values those are is asked of the type environment rather than listed here, the way
-- 'Eval.Runtime.WebDocs.docPagesFor' asks which annotations are pages of their own. The class is
-- open, so its members are however many @.ct@ files have said they belong — which is the whole
-- point of the class and is why this asks rather than knows.
--
-- Like 'Eval.Runtime.Tasks.taskList' it reports on which pages there are rather than on the program
-- it was evaluated against, so evaluated unscoped it is every entry the doc server holds, taken page
-- by page in the order the pages are opened by.
calendarEntries :: EPrim
calendarEntries = EEnvPrim "calendarEntries" $ gatherPrim [
  Reading calendarEntryClass calendarDayMethod (TimeAsked calendarTimeMethod)
                                                            ]

-- | Backs @/Data/Calendar/plannerEntries@, the data carried by a @Show/DayPlanner@ display: the
-- calendar's answer as a day planner lays it out.
--
-- A planner's blocks are the members of @\/Data\/Calendar\/Event@, each asked the span of its day
-- it takes up, which every event has. What else sits on a day is an entry of the calendar and is
-- carried without a time, to be drawn apart from the blocks — a task with no time yet, which a
-- reader schedules by giving it one. A value which is both is an event, and is carried once.
plannerEntries :: EPrim
plannerEntries = EEnvPrim "plannerEntries" $ gatherPrim [
  Reading eventClass eventDayMethod (SpanAsked eventSpanMethod),
  Reading calendarEntryClass calendarDayMethod TimeNotAsked
                                                          ]

-- | One way a display reads the values of a class: the class, and what each value of it is asked
-- for the day it sits on and the time of it it takes up.
data Reading = Reading { rdClass :: String
                       , rdDay   :: String
                       , rdTime  :: TimeAsk
                       }

-- | How the time of its day an entry takes up is asked: as a span it may not have, as one every
-- value read this way has, or not at all — an entry drawn across its day whatever it would say.
data TimeAsk = TimeAsked String | SpanAsked String | TimeNotAsked

-- | A primitive gathering every value the evaluation covers that some reading reads, each read by
-- the first reading whose class it is in.
--
-- One gather of every type any reading names, rather than a gather per reading, because what comes
-- back is in the order the page wrote it, and two gatherings joined would be two runs of that order
-- rather than one.
gatherPrim :: [Reading] -> Env -> H.HashMap String Val -> Either String Val
gatherPrim _ Env{evService = Nothing} _ = Left noService
gatherPrim readings Env{evService = Just svc, evTypeEnv} _ = askService $ do
  today <- esToday svc
  let scope = esScope svc
  graph <- esBuilt svc scope
  -- One gatherer and one asker for every page, not one per page: each holds the scope's
  -- typechecked programs, so taking one per page would pay for the scope once per page — over a
  -- vault of notes, quadratic in the notes.
  gather <- esValues svc scope
  ask <- esAsk svc scope
  -- Each reading with the types that joined its class, which a gathered value is checked against.
  let members = [(r, classMembers evTypeEnv (rdClass r)) | r <- readings]
  let pages = map snd3 $ graphToNodes $ maybe graph (`graphFilterReaches` graph) scope
  fmap (answered today) <$> traverseUntilLeft (pageEntries members gather ask) (sortOn impDisp pages)
  where
    answered today found = encodeJsonText $ CalendarResult Nothing today (concat found)

    -- A class nobody joined is a calendar with nothing on it rather than a failure, as
    -- 'docPageAnnots' answers a program which never reached its class with no pages.
    pageEntries members gather ask prgmName = case nub $ map ptName $ concatMap snd members of
      [] -> return $ Right []
      names -> do
        gathered <- runCResT $ gather names prgmName
        case orNotes gathered of
          Left err -> return $ Left err
          Right values -> case (mapMaybe (readBy members) values, impDisp prgmName) of
            ([], _) -> return $ Right []
            -- As in 'Eval.Runtime.Tasks.taskList', an entry with no page to be written back to is
            -- reported rather than dropped.
            (_, Nothing) -> return $ Left $ printf "%s has no file path for %s, so its entries have nothing to be written back to" (show prgmName) (show prgmName)
            (read', Just disp) -> traverseUntilLeft (entry ask prgmName disp) read'

    -- Each value is read by the first reading whose class it is in. A value gathered by its name
    -- and in none of them is one whose type joined only where it is narrower than this value is —
    -- a task is an event only where it has a time — and is no entry of these readings.
    readBy members (e, value) = (\(r, _) -> (r, e, value)) <$> find (memberOf value . snd) members
    memberOf value = any (isSubtypeOf evTypeEnv (getValType value) . singletonType)

    entry ask prgmName disp (Reading{rdDay, rdTime}, e, value) = do
      dayOf <- askFor ask prgmName rdDay value
      timeOf <- case rdTime of
        TimeAsked method -> (>>= valTime) <$> askFor ask prgmName method value
        SpanAsked method -> (>>= valSpan) <$> askFor ask prgmName method value
        TimeNotAsked     -> return $ Right Nothing
      return $ do
        day <- valDay =<< dayOf
        time <- timeOf
        return $ CalendarItem disp (UUID.toString $ getMetaId $ getExprMeta e) day time value

    -- An asker is built per question asked rather than per page, as a page holding no event is
    -- never asked what span one takes up.
    askFor ask prgmName method value = case orNotes (ask (mkName method) prgmName) of
      Left err     -> return $ Left err
      Right asker' -> orNotes <$> runCResT (asker' value)

-- | The types a class holds: its members, and the members of any of those which is itself a class,
-- down to the types a value is actually declared as — each as the type it joined as.
--
-- Read off the class graph, as 'Eval.Runtime.WebDocs.docPageAnnots' reads which annotations are
-- pages of their own. The class is not sealed and does not need to be: an @every ... isa@ anywhere
-- in what the page reaches is another entry in the one class node, so the members are however many
-- have joined.
--
-- Taken down to concrete types because a value is declared as the member it is, and what
-- 'CtServiceApi.csValues' gathers is values by the path they were declared at — the same fact that
-- has 'Eval.Runtime.Tasks.taskList' name the two readings of a task rather than the class over them.
-- A class joins as a class, though: @every(Task, isa= [CalendarEntry])@ says that a task sits on a
-- day however its vault has it read, which is one statement where naming each reading would be one
-- per reading and would be silently short of a reading added later.
--
-- Kept as the types they joined as and not only their names, because a type may join narrower than
-- its name: @every(Task/Simple..(time -> /Data/Maybe/Some..), isa= [Event])@ is a task with a time,
-- and a task without one, gathered by the same name, is no event. The narrowing is what a gathered
-- value is checked against.
--
-- The walk carries what it has already been through, so a class which reaches itself is followed
-- once. A type which is no class is a type a value can be declared as, which is where it stops.
classMembers :: (TypeGraph tg) => TypeEnv tg -> String -> [PartialType]
classMembers typeEnv cls = nub $ declaredAs S.empty (partialVal (mkName cls))
  where
    classes = scClasses (teSatCache typeEnv)
    declaredAs seen p
      | ptName p `S.member` seen = []
      | otherwise = case H.lookup (ptName p) classes of
          -- A program which never reached the class joined nothing to it, which is a calendar with
          -- nothing on it rather than an error, as 'docPageAnnots' answers such a program with no
          -- pages.
          Nothing -> [p | ptName p /= mkName cls]
          Just ClassSatInfo{csiTypes} -> concatMap (declaredAs (S.insert (ptName p) seen)) (mapMaybe member csiTypes)
    -- A member is written either as the type it is or as a class over several, and the two are not
    -- the same shape: a class joins as a predicate over everything in it where a type joins as
    -- itself. A class is followed by its name, and a type is kept whole.
    member t = maybe (partialVal . ptName <$> maybeGetClassSingleton t) Just (maybeGetSingleton t)

-- | The day an answer of @calendarDay@ names, and nothing where it names none.
--
-- Handed on as the @YYYY-MM-DD@ it is written as, because that is how a day travels as JSON and how
-- whatever draws the calendar reads one.
valDay :: Val -> Either String (Maybe String)
valDay v = case valMaybe valDate v of
  Right day -> Right $ showGregorian <$> day
  Left err  -> Left $ printf "%s is not the day a calendar entry sits on: %s" (show v) err

-- | The span of its day an answer of @calendarTime@ names, and nothing where it names none.
valTime :: Val -> Either String (Maybe CalendarTime)
valTime v = case valMaybe valTimeRange v of
  Right range -> Right $ calendarTime <$> range
  Left err    -> Left $ printf "%s is not the span of a day a calendar entry takes up: %s" (show v) err

-- | A span of a day, as the @HH:mm@ times it is handed on as.
calendarTime :: ((Integer, Integer), (Integer, Integer)) -> CalendarTime
calendarTime (start, end) = CalendarTime (clock start) (clock end)
  where clock (hour, minute) = printf "%02d:%02d" hour minute

-- | The span of its day an answer of @eventSpan@ names, which every event has.
valSpan :: Val -> Either String (Maybe CalendarTime)
valSpan v = case valTimeRange v of
  Right range -> Right $ Just $ calendarTime range
  Left err    -> Left $ printf "%s is not the span of a day an event takes up: %s" (show v) err
