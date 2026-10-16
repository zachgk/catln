-- | What a note is READ as is user space, and is held by the fixture vaults under
-- @test\/Integration\/code\/artifacts@ and their goldens: @catln-test@ holds a note per shape a
-- note takes (how it breaks at checkboxes and fences, which blocks are queries, what its links and
-- properties name). What is left here is writing a note BACK, which has no surface in a Catln
-- program until the Obsidian runtime is Catln code, and so cannot yet be a test of the Catln suite.
-- (The convert golden of a note shows the Catln it was read as, imports and writing included, which
-- is why the reading cases could move.)
module Syntax.ObsidianTests where

import           Data.Char                (isSpace)
import           Data.List                (dropWhileEnd)
import           Syntax.Ct.Prgm           (RawPrgm (RawPrgm))
import           Syntax.Obsidian.Parser   (NotePart (..), mkVaultIndex,
                                           notePageStatement, noteParts,
                                           restoreWikiLinks, unparseNote,
                                           wikiHref, wikiLinks)
import           Syntax.Obsidian.Settings (TaskSettings (..),
                                           defaultTaskSettings)
import           Test.Tasty
import           Test.Tasty.HUnit         (assertEqual, testCase)

-- | A query is written in whatever it holds a fence of, so that writing back what a reader typed
-- cannot close the block in the middle of it.
testQueryHoldingFence :: TestTree
testQueryHoldingFence = testCase "a query holding a fence is written between a longer one" $ do
  assertEqual "read" [NoteQuery "not done\n```\nsort by due"] $ noteParts note
  assertEqual "written" (Just note) $ rewrite note
  where
    note = unlines [
      "````tasks",
      "not done",
      "```",
      "sort by due",
      "````"
                   ]

-- | Only a block at the width the note is written at is a query. One indented is written inside
-- something — a list item, most often — and pulling it out to write it back would take it out of
-- whatever it was written in.
testQueryIndented :: TestTree
testQueryIndented = testCase "an indented tasks fence is code, not a query" $ do
  assertEqual "read" [NoteWriting $ dropWhileEnd isSpace note] $ noteParts note
  assertEqual "written" (Just note) $ rewrite note
  where
    note = unlines [
      "  ```tasks",
      "  not done",
      "  ```"
                   ]

-- | What is written under a checkbox is the note again, so a query written there is a query — and
-- is written back where it was written, indented under its task.
testQueryWithinTask :: TestTree
testQueryWithinTask = testCase "a query written under a checkbox is a query of it" $ do
  assertEqual "read" [NoteTask " " "Plan it" [NoteQuery "not done"]] $ noteParts note
  assertEqual "written" (Just note) $ rewrite note
  where
    note = unlines [
      "- [ ] Plan it",
      "  ```tasks",
      "  not done",
      "  ```"
                   ]

-- | A block which is never closed is the rest of the note, which is what markdown reads one as. It
-- is written back closed, as a checkbox is written back with a bullet however it was written.
testQueryUnclosed :: TestTree
testQueryUnclosed = testCase "a query never closed is the rest of the note" $ do
  assertEqual "read" [NoteQuery "not done\nsort by due"] $ noteParts note
  assertEqual "written" (Just closed) $ rewrite note
  where
    note = unlines ["```tasks", "not done", "sort by due"]
    closed = unlines ["```tasks", "not done", "sort by due", "```"]

-- | A query says nothing, which the plugin reads as every task there is. It is a query all the
-- same, rather than an empty block of code.
testQueryEmpty :: TestTree
testQueryEmpty = testCase "a query saying nothing is a query" $ do
  assertEqual "read" [NoteQuery ""] $ noteParts note
  assertEqual "written" (Just note) $ rewrite note
  where
    note = unlines ["```tasks", "```"]

-- | The reversion of the reading: a note holding a query is written back as the note it was read
-- from, which is what keeps such a note saveable at all.
testRewriteQuery :: TestTree
testRewriteQuery = testCase "a note holding a query is written back as it was read" $
  assertEqual "" (Just note) (rewrite note)
  where
    note = unlines [
      "A note.",
      "",
      "- [ ] Read it",
      "",
      "```tasks",
      "not done",
      "sort by due",
      "```",
      "",
      "The note again."
                   ]

-- | And writing back what was already written back writes the same thing again.
testRewriteQueryIdempotent :: TestTree
testRewriteQueryIdempotent = testCase "writing a note's query back twice writes it the same way" $
  assertEqual "" (rewrite note) (rewrite =<< rewrite note)
  where
    note = unlines [
      "A note.",
      "~~~tasks",
      "not done",
      "~~~",
      "The note again."
                   ]

-- | A link written inside a query is inside a fenced block, so it is writing about a link rather
-- than one. It travels in the query as the reader typed it.
testQueryKeepsItsLinks :: TestTree
testQueryKeepsItsLinks = testCase "a link written inside a query is left as it was written" $ do
  assertEqual "linked" [] $ wikiLinks "note" note
  assertEqual "written" (Just note) $ rewrite note
  where
    note = unlines ["```tasks", "path includes [[Welcome]]", "```"]

-- | The vault every note below is read as a note of, at the paths a walk of it would find them.
--
-- One note at the top of the vault, one within a folder of it so that a link may name it by two
-- names, and one whose name holds a space so that the address a link is written as has to escape
-- one. No vault on disk: what a link resolves to is decided by the note paths alone.
vault :: [FilePath]
vault = ["/v/Welcome.md", "/v/Notes/Nested.md", "/v/test link.md"]

-- | Where a wiki link written in a note of that vault points.
vaultHref :: String -> Maybe String
vaultHref = wikiHref "/v" (mkVaultIndex "/v" vault)

-- | A note written back out of the page it was read as, in a vault running no Tasks plugin.
rewrite :: String -> Maybe String
rewrite = rewriteWith Nothing

-- | The same, in a vault set however the settings say. Every case below reads a note of a vault
-- running no plugin, because how a checkbox of a vault running one is read is pinned by the goldens
-- of the fixture vaults (test/Integration/code/artifacts/catln-tasks*) and what a note holding one
-- is written back as is 'testRewriteTasks'.
rewriteWith :: Maybe TaskSettings -> String -> Maybe String
rewriteWith settings note = unparseNote $ RawPrgm [] [notePageStatement settings Nothing vaultHref "A note" note]

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

-- | The properties are written back between the delimiters they were found between, and the note
-- begins after them.
testRewriteFrontmatter :: TestTree
testRewriteFrontmatter = testCase "a note is written back with the properties it opened with" $
  assertEqual "" (Just note) (rewrite note)
  where
    note = unlines [
      "---",
      "tags:",
      "  - reading",
      "title: A note about a page",
      "---",
      "",
      "The note.",
      "",
      "- [ ] Read it"
                   ]

-- | The regression this exists for: the page a note is written over is a statement the note itself
-- never wrote, and a statement the reversion does not know is a note which cannot be saved at all
-- ('Syntax.Parsers.unparseImport'). So naming a page must not cost a note its saving.
testRewriteWithOverlay :: TestTree
testRewriteWithOverlay = testCase "naming a page to be written over does not stop a note being written back" $
  assertEqual "" (Just note) (rewrite note)
  where
    note = unlines [
      "---",
      "url: https://example.com",
      "---",
      "",
      "Notes written over a page.",
      "",
      "- [x] Read it"
                   ]

-- | Writing the properties back is writing what they said, so writing back what was already written
-- back writes the same thing again — including the blank line the note begins after.
testRewriteFrontmatterIdempotent :: TestTree
testRewriteFrontmatterIdempotent = testCase "writing a note's properties back twice writes them the same way" $
  assertEqual "" (rewrite note) (rewrite =<< rewrite note)
  where
    note = unlines [
      "---",
      "url: https://example.com",
      "---",
      "- [ ] A checkbox written straight after the properties"
                   ]

-- | And the reversion: a link to a note of the vault is written back as the wiki link it was read
-- from, so a note read and saved unchanged leaves the file as it was.
--
-- Only a link naming a markdown file by a path of its own, which is what 'wikiHref' writes and what
-- a note of a vault names another note by. Everything else a note may link to is left alone.
testRestoreWikiLinks :: TestTree
testRestoreWikiLinks = testGroup "a link to a note is written back as a wiki link" [
  restores "naming it by the vault's path for it" "Links to [Welcome](Welcome.md)." "Links to [[Welcome]].",
  restores "naming a place within it" "Links to [Welcome#Getting started](Welcome.md#Getting%20started)." "Links to [[Welcome#Getting started]].",
  restores "naming a note whose name holds a space" "Links to [test link](test%20link.md)." "Links to [[test link]].",
  -- The words are kept as the link's alias wherever they are not already what it names, which is
  -- what happens to a link a note wrote naming a note in a folder by its name alone.
  restores "shown as words of its own" "Links to [Nested](Notes/Nested.md)." "Links to [[Notes/Nested|Nested]].",
  restores "on the web" "Try [the Importer](https://example.com/Importer)." "Try [the Importer](https://example.com/Importer).",
  restores "naming a file from the root of the filesystem" "See [a](/elsewhere/Other.md)." "See [a](/elsewhere/Other.md).",
  restores "naming something which is not a note" "See [a](other.ct)." "See [a](other.ct).",
  restores "written inside code" "```\nSee [a](Welcome.md).\n```" "```\nSee [a](Welcome.md).\n```"
                                                                                  ]
  where restores what written expected = testCase what $ assertEqual "" expected $ restoreWikiLinks written

-- | The round trip the two of them make: a note holding every shape of link at once is read as its
-- page and written back as the note it was read from.
--
-- A link naming a note by a name which is not the vault's path for it is written back naming that
-- path, and shown as the words it named. It points at the note it always pointed at and is shown as
-- the words it was always shown as; what it does not keep is the spelling, which is what
-- 'unparseNote' says of a note's writing generally.
testRewriteLinks :: TestTree
testRewriteLinks = testCase "a note of links is written back as the note it was read from" $
  assertEqual "" (Just written) (rewrite note)
  where
    note = unlines [
      "Links to [[Welcome]], [[Notes/Nested]] and [[test link]].",
      "",
      "Naming a place within one, [[Welcome#Getting started]], and words of its own,",
      "[[Welcome|the first note]].",
      "",
      "A note yet to be written, [[create a link]], and a note named by its name alone,",
      "[[Nested]].",
      "",
      "Shows ![[Welcome]] within it, and [the Importer](https://example.com/Importer).",
      "",
      "```",
      "Not [[Nested]].",
      "```"
                   ]
    written = unlines [
      "Links to [[Welcome]], [[Notes/Nested]] and [[test link]].",
      "",
      "Naming a place within one, [[Welcome#Getting started]], and words of its own,",
      "[[Welcome|the first note]].",
      "",
      "A note yet to be written, [[create a link]], and a note named by its name alone,",
      "[[Notes/Nested|Nested]].",
      "",
      "Shows ![[Welcome]] within it, and [the Importer](https://example.com/Importer).",
      "",
      "```",
      "Not [[Nested]].",
      "```"
                      ]

-- | Writing a note back is writing what it says, so a note already written the way the reversion
-- writes it is written back exactly as it was — links included.
testRewriteLinksIdempotent :: TestTree
testRewriteLinksIdempotent = testCase "writing a note of links back twice writes it the same way" $
  assertEqual "" (rewrite note) (rewrite =<< rewrite note)
  where
    note = unlines [
      "Links to [[Nested]] and [[Welcome#Getting started]].",
      "",
      "- [ ] Read [[Welcome]]",
      "  Which is in [[test link]]."
                   ]

-- | A note of a vault running the Tasks plugin is written back exactly as it was found — the
-- character in every box, every mark the plugin wrote, and a block link at the end of a line.
--
-- This is the whole point of a task carrying the line it was read from. Everything the plugin wrote
-- has been read apart into fields by this point, and none of those fields is consulted here: the
-- note is written back from the box and the line, so a line carrying something the reading does not
-- yet know is carried through it untouched.
testRewriteTasks :: TestTree
testRewriteTasks = testCase "a note of tasks the plugin manages is written back as it was found" $
  assertEqual "" (Just note) (rewriteWith (Just defaultTaskSettings) note)
  where
    note = unlines [
      "- [ ] Read the note",
      "- [x] Write this note \x2705 2026-01-05",
      "- [/] Plan the rest",
      "- [-] Drop the old plan \x274C 2026-01-06",
      "- [ ] Ship it #work \x23EB \x1F4C5 2026-01-04",
      "- [ ] Answer the question \x23F3 2026-01-03 \x1F53D ^answer-it",
      "- [ ] Book \x1F4C5 lunch"
                   ]

-- | And a note the plugin does not manage every line of. A vault asking that every task carry a word
-- reads the lines not carrying it as plain checkboxes, and those are written back the way a plain
-- checkbox is — so one note writes back through both readings at once.
testRewriteMixedTasks :: TestTree
testRewriteMixedTasks = testCase "a note read through both readings is written back through both" $
  assertEqual "" (Just note) (rewriteWith (Just filtered) note)
  where
    filtered = defaultTaskSettings{tstGlobalFilter = "#task"}
    note = unlines [
      "- [ ] #task Ship it \x1F4C5 2026-01-04",
      "- [x] Buy milk",
      "- [ ] #task Plan the rest"
                   ]

obsidianTests :: TestTree
obsidianTests = testGroup "ObsidianTests" [
  testQueryHoldingFence,
  testQueryIndented,
  testQueryWithinTask,
  testQueryUnclosed,
  testQueryEmpty,
  testRewriteQuery,
  testRewriteQueryIdempotent,
  testQueryKeepsItsLinks,
  testRewrite,
  testRewriteNormalizes,
  testRewriteIdempotent,
  testRewriteOnlyNotes,
  testRewriteFrontmatter,
  testRewriteWithOverlay,
  testRewriteFrontmatterIdempotent,
  testRestoreWikiLinks,
  testRewriteLinks,
  testRewriteLinksIdempotent,
  testRewriteTasks,
  testRewriteMixedTasks
                                          ]
