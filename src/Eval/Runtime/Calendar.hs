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
                                       calendarTimeMethod)
import           Data.Aeson           (ToJSON)
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.List            (nub, sortOn)
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
calendarPrims = [calendarEntries]

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
calendarEntries = EEnvPrim "calendarEntries" prim
  where
    prim Env{evService = Nothing} _ = Left noService
    prim Env{evService = Just svc, evTypeEnv} _ = askService $ do
      today <- esToday svc
      let scope = esScope svc
      graph <- esBuilt svc scope
      -- One gatherer and one asker for every page, not one per page: each holds the scope's
      -- typechecked programs, so taking one per page would pay for the scope once per page — over a
      -- vault of notes, quadratic in the notes.
      gather <- esValues svc scope
      ask <- esAsk svc scope
      let members = entryMembers evTypeEnv
      let pages = map snd3 $ graphToNodes $ maybe graph (`graphFilterReaches` graph) scope
      fmap (answered today) <$> traverseUntilLeft (pageEntries members gather ask) (sortOn impDisp pages)

    answered today found = encodeJsonText $ CalendarResult Nothing today (concat found)

    -- A class nobody joined is a calendar with nothing on it rather than a failure, as
    -- 'docPageAnnots' answers a program which never reached its class with no pages.
    pageEntries [] _ _ _ = return $ Right []
    pageEntries members gather ask prgmName = do
      gathered <- runCResT $ gather members prgmName
      case orNotes gathered of
        Left err -> return $ Left err
        Right values -> case (values, impDisp prgmName) of
          ([], _) -> return $ Right []
          -- As in 'Eval.Runtime.Tasks.taskList', an entry with no page to be written back to is
          -- reported rather than dropped.
          (_, Nothing) -> return $ Left $ printf "calendarEntries has no file path for %s, so its entries have nothing to be written back to" (show prgmName)
          (_, Just disp) -> case (,) <$> orNotes (ask (mkName calendarDayMethod) prgmName) <*> orNotes (ask (mkName calendarTimeMethod) prgmName) of
            Left err              -> return $ Left err
            Right (askDay, askTime) -> traverseUntilLeft (entry askDay askTime disp) values

    entry askDay askTime disp (e, value) = do
      dayOf <- runCResT $ askDay value
      timeOf <- runCResT $ askTime value
      return $ do
        day <- valDay =<< orNotes dayOf
        time <- valTime =<< orNotes timeOf
        return $ CalendarItem disp (UUID.toString $ getMetaId $ getExprMeta e) day time value

-- | The types a calendar can show: the members of @\/Data\/Calendar\/CalendarEntry@, and the members
-- of any of those which is itself a class, down to the types a value is actually declared as.
--
-- Read off the class graph, as 'Eval.Runtime.WebDocs.docPageAnnots' reads which annotations are
-- pages of their own. The class is not sealed and does not need to be: an @every ... isa@ anywhere
-- in what the page reaches is another entry in the one class node, so the members are however many
-- have joined.
--
-- Taken down to concrete types because a value is declared as the member it is, and what
-- 'CtServiceApi.csValues' gathers is values by the path they were declared at — the same fact that
-- has 'Eval.Runtime.Tasks.taskList' name the two readings of a task rather than the class over them.
-- A class joins a calendar as a class, though: @every(Task, isa= [CalendarEntry])@ says that a task
-- sits on a day however its vault has it read, which is one statement where naming each reading
-- would be one per reading and would be silently short of a reading added later.
--
-- The walk carries what it has already been through, so a class which reaches itself is followed
-- once. A name which is no class is a type a value can be declared as, which is where it stops.
entryMembers :: (TypeGraph tg) => TypeEnv tg -> [Name]
entryMembers typeEnv = nub $ declaredAs S.empty (mkName calendarEntryClass)
  where
    classes = scClasses (teSatCache typeEnv)
    declaredAs seen name
      | name `S.member` seen = []
      | otherwise = case H.lookup name classes of
          -- A program which never reached the class joined nothing to it, which is a calendar with
          -- nothing on it rather than an error, as 'docPageAnnots' answers such a program with no
          -- pages.
          Nothing -> [name | name /= mkName calendarEntryClass]
          Just ClassSatInfo{csiTypes} -> concatMap (declaredAs (S.insert name seen)) (members csiTypes)
    -- A member is written either as the type it is or as a class over several, and the two are not
    -- the same shape: a class joins as a predicate over everything in it where a type joins as
    -- itself. Both are read for their name, and it is the walk above that tells them apart.
    members = mapMaybe memberName
    memberName t = ptName <$> maybe (maybeGetClassSingleton t) Just (maybeGetSingleton t)

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
  Right range -> Right $ spanOf <$> range
  Left err    -> Left $ printf "%s is not the span of a day a calendar entry takes up: %s" (show v) err
  where
    spanOf (start, end) = CalendarTime (clock start) (clock end)
    clock (hour, minute) = printf "%02d:%02d" hour minute
