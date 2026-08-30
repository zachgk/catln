-- | Tests for the calendar: what a @\/Data\/Calendar\/#calendar@ finds when the compiler answers it.
--
-- Answered through a real compilation service rather than by calling the primitive, because what is
-- worth testing is the whole way round: the class is asked which types have joined it, the values of
-- those types are gathered from every page the calendar reaches, and each of them is asked
-- @calendarDay@ in the program of the page it came from. Calling the primitive with values built
-- here would test none of those three.
--
-- The day every calendar below is answered on is the day 'Semantics.testCtssConfig' pins, so a
-- fixture asks one thing forever.
module Syntax.CalendarTests where

import           CRes                    (CRes (..), prettyCNotes, runCResT)
import           CtService               (CTSS, ctssBaseFiles, ctssKeys,
                                          getEvalAnnots)
import           Data.Aeson              (FromJSON (..), Value (..), decode,
                                          withObject, (.:), (.:?))
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.HashMap.Strict     as H
import           Data.List               (isInfixOf, nub, sort)
import           Data.Maybe              (listToMaybe, mapMaybe)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Semantics               (testCtssConfig)
import           Semantics.Prgm          (FileImport, impDisp)
import           Semantics.Val           hiding (Val)
import           System.Directory        (makeAbsolute)
import           Test.Tasty
import           Test.Tasty.HUnit        (assertBool, assertEqual,
                                          assertFailure, testCase)

-- | The page the calendar below is written in, which reaches the vault it is a calendar of.
calendarPage :: FilePath
calendarPage = "test/Integration/code/calendar.ct"

-- | What the display of a calendar carries, read back out of the JSON it carries it in.
data Calendared = Calendared { calendaredError :: Maybe String
                             , calendaredToday :: String
                             , calendaredItems :: [Value]
                             }

instance FromJSON Calendared where
  parseJSON = withObject "Calendared" $ \o -> Calendared
    <$> o .:? "crError"
    <*> o .: "crToday"
    <*> o .: "crItems"

-- | One service for every case below, which is what keeps the cost of these tests to one compile of
-- the standard library rather than one per case.
calendaringPages :: IO CTSS
calendaringPages = ctssBaseFiles testCtssConfig [calendarPage]

-- | The calendar a page wrote, answered by the compiler the way a doc server asks for it.
--
-- The whole of the way, as @ObsidianQueryTests.answeredBy@ takes a query: the pages are compiled,
-- the annotation is evaluated, and the value it displays as is what comes back.
calendaredBy :: CTSS -> FilePath -> IO Calendared
calendaredBy ctss page = do
  prgmName <- pageAt page ctss
  annots <- runCResT $ getEvalAnnots ctss prgmName
  case annots of
    CErr notes -> assertFailure $ "the page did not compile: " ++ prettyCNotes notes
    CRes _ vals -> case [args | (_, TupleVal name args) <- vals, name == "/Data/Calendar/Show/Calendar"] of
      [args] -> case H.lookup "/result" args of
        Just (StrVal json) -> maybe (assertFailure $ "the answer is not JSON: " ++ json) return $
          decode (TLE.encodeUtf8 (TL.pack json))
        carried -> assertFailure $ "the display carries " ++ show carried ++ " rather than a result"
      displays -> assertFailure $ "the page displays " ++ show (length displays) ++ " calendars"

-- | The page a service holds at a path, asked for by the path it resolves to.
pageAt :: FilePath -> CTSS -> IO FileImport
pageAt path ctss = do
  pages <- ctssKeys ctss
  resolved <- makeAbsolute path
  case [page | page <- pages, impDisp page `elem` [Just path, Just resolved]] of
    [page] -> return page
    held   -> assertFailure $ "the service holds " ++ show (length held) ++ " pages shown at " ++ path

-- | The day an entry of the answer sits on, and nothing for one which sits on no day.
dayOf :: Value -> Maybe String
dayOf (Object o) | Just (String d) <- KM.lookup "ciDay" o = Just (T.unpack d)
dayOf _ = Nothing

-- | What an entry of the answer says, which is what a reader sees beside its box.
saysOf :: Value -> Maybe String
saysOf v = case v of
  Object o | Just (Object shown) <- KM.lookup "ciValue" o
           , Just (Object args) <- KM.lookup "args" shown
           -> firstString [KM.lookup k args | k <- ["/description", "/contents"]]
  _ -> Nothing
  where
    firstString vs = listToMaybe [T.unpack s | Just (Object arg) <- vs, Just (String s) <- [KM.lookup "contents" arg]]

-- | Which page an entry of the answer was written in.
pageOf :: Value -> Maybe String
pageOf (Object o) | Just (String p) <- KM.lookup "ciPage" o = Just (T.unpack p)
pageOf _ = Nothing

-- | The expression an entry of the answer was declared by, which is what a change to it replaces.
exprOf :: Value -> Maybe String
exprOf (Object o) | Just (String e) <- KM.lookup "ciExpr" o = Just (T.unpack e)
exprOf _ = Nothing

-- | What a task of the fixture vault says, beside the day it sits on.
--
-- Written out rather than derived, so that a change to how a day is chosen shows up here as a
-- change to what is expected rather than as the same rule computed twice.
saidDays :: [(Maybe String, Maybe String)]
saidDays = [
  (Just "Read the note", Nothing),
  (Just "Write this note", Nothing),
  (Just "Plan the rest", Nothing),
  (Just "Drop the old plan", Nothing),
  (Just "Ship it #work", Just "2026-01-04"),
  (Just "Water the plants", Just "2026-01-02"),
  (Just "Do this first", Nothing),
  (Just "Do this after", Nothing),
  (Just "Answer the question", Just "2026-01-03"),
  (Just "Book 📅 lunch", Nothing)
           ]

testCalendaring :: TestTree
testCalendaring = withResource calendaringPages (const $ return ()) $ \held -> testGroup "the calendar a page wrote, answered by the compiler" [

  testCase "over everything the page reaches which sits on a day" $ do
    ctss <- held
    calendar <- calendaredBy ctss calendarPage
    assertEqual "the calendar could not be answered" Nothing (calendaredError calendar)
    assertEqual "the calendar did not lay the vault's tasks on the days they sit on"
      saidDays (map (\v -> (saysOf v, dayOf v)) (calendaredItems calendar)),

  testCase "on the first of the three days a task may name" $ do
    ctss <- held
    calendar <- calendaredBy ctss calendarPage
    -- Water the plants names only a day it may be begun, which is the first of the three; Answer the
    -- question names only a day it is scheduled for; Ship it names only a day it is due. Each sits
    -- on the one it named, which is what makes the rule visible for all three positions.
    assertEqual "a task did not sit on the day it named"
      ["2026-01-02", "2026-01-03", "2026-01-04"]
      (sort $ mapMaybe dayOf [v | v <- calendaredItems calendar, saysOf v /= Nothing]),

  testCase "carrying the tasks which sit on no day rather than dropping them" $ do
    ctss <- held
    calendar <- calendaredBy ctss calendarPage
    assertEqual "the tasks which sit on no day were dropped"
      7 (length [v | v <- calendaredItems calendar, dayOf v == Nothing]),

  testCase "each beside the page it was written in and the expression it was declared by" $ do
    ctss <- held
    calendar <- calendaredBy ctss calendarPage
    let items = calendaredItems calendar
    assertEqual "an entry has no page to be written back to"
      [] [v | v <- items, pageOf v == Nothing]
    -- The calendar spans the pages the one it is written in reaches, so the tasks carry the note
    -- they were written in rather than the page showing them.
    assertBool "a task was not carried back to the note it was written in" $
      all ("catln-tasks/Tasks.md" `isInfixOf`)
        (mapMaybe pageOf [v | v <- items, saysOf v /= Nothing])
    assertEqual "two entries name the one expression, so writing to one would write the other"
      (length items) (length (nub (mapMaybe exprOf items))),

  testCase "marking the day the question was asked on and not the day it is read on" $ do
    ctss <- held
    calendar <- calendaredBy ctss calendarPage
    -- The day the service was given, which testCtssConfig pins. A calendar reading its own clock
    -- would answer differently every day the suite is run.
    assertEqual "the calendar did not carry the day it was answered on"
      "2026-01-03" (calendaredToday calendar)
                                                                                               ]

calendarTests :: TestTree
calendarTests = testGroup "CalendarTests" [testCalendaring]
