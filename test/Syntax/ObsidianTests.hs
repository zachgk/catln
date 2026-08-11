-- | Tests for reading a note of an Obsidian vault.
--
-- What a note is read as is covered by the golden tests of the vault in
-- @test\/Integration\/code\/artifacts\/catln-test@, but only as far as the declarations it makes: a
-- page's golden shows neither the imports of the page nor the comments inside it. So the two things
-- a note holds that a golden cannot show are tested here — which notes it links to, and how its
-- writing is broken.
module Syntax.ObsidianTests where

import           Syntax.Obsidian.Parser (NotePart (..), noteParts, wikiLinks)
import           Test.Tasty
import           Test.Tasty.HUnit       (assertEqual, testCase)

-- | A checkbox is a reference to a definition the note never gives, exactly as a wiki link is, so a
-- note holding one used to parse as no markdown at all and to link to nothing at all.
testLinksThroughTask :: TestTree
testLinksThroughTask = testCase "a checkbox does not cost a note its links" $
  assertEqual "" ["Welcome", "Nested"] $ wikiLinks "note" $ unlines [
    "A note.",
    "",
    "- [ ] Read [[Welcome]]",
    "- [x] Written up in [[Nested]]"
                                                                   ]

-- | The reading that leaves code out runs over the note the boxes were taken out of, so taking them
-- out must not have made code of the note into writing.
testNoLinksInCode :: TestTree
testNoLinksInCode = testCase "a link written inside code is not a link" $
  assertEqual "" ["Welcome"] $ wikiLinks "note" $ unlines [
    "Links to [[Welcome]].",
    "",
    "```",
    "- [ ] Not a link to [[Nested]]",
    "```"
                                                          ]

-- | The break itself: what is written between two checkboxes is a comment of the note, what is
-- written under one is a comment of its task, and what is only blank lines is written nowhere.
testNoteParts :: TestTree
testNoteParts = testCase "a note is broken at each of its checkboxes" $
  assertEqual "" [
    NoteWriting "A note.",
    NoteTask "Read it" False "",
    NoteTask "Write it" True "Which is written\n\nunder the task.",
    NoteWriting "The note again."
                 ] $ noteParts $ unlines [
    "A note.",
    "",
    "- [ ] Read it",
    "- [x] Write it",
    "      Which is written",
    "",
    "      under the task.",
    "",
    "The note again."
                                         ]

-- | A checkbox indented deeper than the one above it is a task of the note like any other, rather
-- than writing about the task above it.
testNestedTask :: TestTree
testNestedTask = testCase "a checkbox written under a checkbox is a task of its own" $
  assertEqual "" [NoteTask "Plan it" False "", NoteTask "A step of the plan" False ""] $ noteParts $ unlines [
    "- [ ] Plan it",
    "  - [ ] A step of the plan"
                                                                                                            ]

-- | A checkbox is a list item, and a list is written with numbers as readily as with bullets.
testNumberedTask :: TestTree
testNumberedTask = testCase "a numbered checkbox is a task" $
  assertEqual "" [NoteTask "Read it" False "", NoteTask "Write it" True ""] $ noteParts $ unlines [
    "1. [ ] Read it",
    "2) [x] Write it"
                                                                                                 ]

obsidianTests :: TestTree
obsidianTests = testGroup "ObsidianTests" [
  testLinksThroughTask,
  testNoLinksInCode,
  testNoteParts,
  testNestedTask,
  testNumberedTask
                                          ]
