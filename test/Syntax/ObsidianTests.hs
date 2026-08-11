-- | Tests for reading a note of an Obsidian vault, and writing one back.
--
-- What a note is read as is covered by the golden tests of the vault in
-- @test\/Integration\/code\/artifacts\/catln-test@, but only as far as the declarations it makes: a
-- page's golden shows neither the imports of the page nor the comments inside it. So the things a
-- note holds that a golden cannot show are tested here — which notes it links to, how its writing is
-- broken, and what it is written back as.
module Syntax.ObsidianTests where

import           Syntax.Ct.Prgm         (RawPrgm (RawPrgm))
import           Syntax.Obsidian.Parser (NotePart (..), notePageStatement,
                                         noteParts, unparseNote, wikiLinks)
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
    NoteTask "Read it" False [],
    NoteTask "Write it" True [NoteWriting "Which is written\n\nunder the task."],
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

-- | A checkbox indented deeper than the one above it is written under it, as the writing under it
-- is, so it is a task of that task rather than one of the note beside it.
testNestedTask :: TestTree
testNestedTask = testCase "a checkbox written under a checkbox is a task of it" $
  assertEqual "" [NoteTask "Plan it" False [NoteTask "A step of the plan" False []]] $ noteParts $ unlines [
    "- [ ] Plan it",
    "  - [ ] A step of the plan"
                                                                                                          ]

-- | A checkbox is a list item, and a list is written with numbers as readily as with bullets.
testNumberedTask :: TestTree
testNumberedTask = testCase "a numbered checkbox is a task" $
  assertEqual "" [NoteTask "Read it" False [], NoteTask "Write it" True []] $ noteParts $ unlines [
    "1. [ ] Read it",
    "2) [x] Write it"
                                                                                                 ]

-- | A note written back out of the page it was read as.
rewrite :: String -> Maybe String
rewrite note = unparseNote $ RawPrgm [] [notePageStatement "A note" note]

-- | The whole of the reversion on a note holding every shape at once: writing before and after the
-- tasks, a checked box and an unchecked one, writing under a task, a task under a task, and a
-- checkbox inside a fence which is writing rather than a task.
--
-- The note is written in the form the reversion writes, so what it proves is that a note already in
-- that form is written back exactly as it was, and so that reading a note and saving it unchanged
-- leaves the file alone.
testRewrite :: TestTree
testRewrite = testCase "a note is written back as the note it was read from" $
  assertEqual "" (Just note) (rewrite note)
  where
    note = unlines [
      "A note.",
      "",
      "- [ ] Read it",
      "- [x] Write it",
      "  Which is written under the task.",
      "",
      "  It goes on for as long as it is indented.",
      "- [ ] Plan it",
      "  - [ ] A step of the plan",
      "",
      "The note again.",
      "",
      "```",
      "- [ ] A checkbox inside code is writing about a task",
      "```"
                   ]

-- | What the reversion writes rather than what it was given: a checkbox is written with a bullet
-- however it was written, and a task is written at the depth it is declared at.
testRewriteNormalizes :: TestTree
testRewriteNormalizes = testCase "a checkbox is written back with a bullet marker" $
  assertEqual "" (Just written) $ rewrite $ unlines [
    "1. [ ] Read it",
    "2) [x] Write it",
    "        - [ ] A step of it"
                                                    ]
  where
    written = unlines [
      "- [ ] Read it",
      "- [x] Write it",
      "  - [ ] A step of it"
                      ]

-- | Writing a note back is writing what it says, so writing back what was already written back
-- writes the same thing again.
testRewriteIdempotent :: TestTree
testRewriteIdempotent = testCase "writing a note back twice writes it the same way" $
  assertEqual "" (rewrite note) (rewrite =<< rewrite note)
  where
    note = unlines [
      "1. [ ] Read it",
      "   Written under it.",
      "2. [x] Write it",
      "        - [ ] A step of it",
      "Trailing writing."
                   ]

-- | The reversion is given a whole page, which is not always a note. One that is not is written as
-- nothing rather than as an empty note, so that a page it does not recognise is never written over.
testRewriteOnlyNotes :: TestTree
testRewriteOnlyNotes = testCase "a page which is not a note is not written back" $
  assertEqual "" Nothing $ unparseNote $ RawPrgm [] []

obsidianTests :: TestTree
obsidianTests = testGroup "ObsidianTests" [
  testLinksThroughTask,
  testNoLinksInCode,
  testNoteParts,
  testNestedTask,
  testNumberedTask,
  testRewrite,
  testRewriteNormalizes,
  testRewriteIdempotent,
  testRewriteOnlyNotes
                                          ]
