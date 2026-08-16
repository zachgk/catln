-- | Tests for reading a note of an Obsidian vault, and writing one back.
--
-- What a note is read as is covered by the golden tests of the vault in
-- @test\/Integration\/code\/artifacts\/catln-test@, but only as far as the declarations it makes: a
-- page's golden shows neither the imports of the page nor the comments inside it. So the things a
-- note holds that a golden cannot show are tested here — which notes it links to, how its writing is
-- broken, and what it is written back as.
module Syntax.ObsidianTests where

import           CtConstants              (overlayAnnot, overlayAnnotUrl)
import           Syntax.Ct.Prgm           (RawPrgm (RawPrgm),
                                           RawStatement (RawAnnot),
                                           RawStatementTree (RawStatementTree))
import           Syntax.Obsidian.Parser   (NotePart (..), frontmatterUrl,
                                           mkVaultIndex, named,
                                           notePageStatement, noteParts,
                                           rawArgStr, resolveWikiLink,
                                           unparseNote, wikiLinks)
import           Syntax.Obsidian.Settings (TaskSettings (..),
                                           defaultTaskSettings)
import           Test.Tasty
import           Test.Tasty.HUnit         (assertEqual, testCase)

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
    NoteTask " " "Read it" [],
    NoteTask "x" "Write it" [NoteWriting "Which is written\n\nunder the task."],
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
  assertEqual "" [NoteTask " " "Plan it" [NoteTask " " "A step of the plan" []]] $ noteParts $ unlines [
    "- [ ] Plan it",
    "  - [ ] A step of the plan"
                                                                                                          ]

-- | A checkbox is a list item, and a list is written with numbers as readily as with bullets.
testNumberedTask :: TestTree
testNumberedTask = testCase "a numbered checkbox is a task" $
  assertEqual "" [NoteTask " " "Read it" [], NoteTask "x" "Write it" []] $ noteParts $ unlines [
    "1. [ ] Read it",
    "2) [x] Write it"
                                                                                                 ]

-- | A note written back out of the page it was read as, in a vault running no Tasks plugin.
rewrite :: String -> Maybe String
rewrite = rewriteWith Nothing

-- | The same, in a vault set however the settings say. Every case below reads a note of a vault
-- running no plugin, because how a checkbox of a vault running one is read is
-- 'Syntax.ObsidianTaskTests' and what a note holding one is written back as is 'testRewriteTasks'.
rewriteWith :: Maybe TaskSettings -> String -> Maybe String
rewriteWith settings note = unparseNote $ RawPrgm [] [notePageStatement settings "A note" note]

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

-- | The properties a note opens with are a part of their own rather than writing of it, so that
-- what they say can be read without reading the writing apart.
testFrontmatterPart :: TestTree
testFrontmatterPart = testCase "a note opening with properties holds them as a part of their own" $
  assertEqual "" [NoteFrontmatter "url: https://example.com", NoteWriting "The note."] $ noteParts $ unlines [
    "---",
    "url: https://example.com",
    "---",
    "",
    "The note."
                                                                                                            ]

-- | A block is the properties of a note only where Obsidian writes one: at the very start, and
-- closed. Anywhere else a @---@ is a rule of the writing, and the writing is where it belongs.
testFrontmatterOnlyAtTheStart :: TestTree
testFrontmatterOnlyAtTheStart = testGroup "a --- which does not open a closed block at the start is writing" [
  testCase "unclosed" $ assertEqual "" [NoteWriting "---\nurl: https://example.com"] $ noteParts $ unlines [
      "---",
      "url: https://example.com"
                                                                                                           ],
  testCase "after the first line" $ assertEqual "" [NoteWriting "The note.\n\n---\nurl: https://example.com\n---"] $ noteParts $ unlines [
      "The note.",
      "",
      "---",
      "url: https://example.com",
      "---"
                                                                                                                                        ],
  testCase "under a task" $ assertEqual "" [NoteTask " " "Read it" [NoteWriting "---\nurl: https://example.com\n---"]] $ noteParts $ unlines [
      "- [ ] Read it",
      "  ---",
      "  url: https://example.com",
      "  ---"
                                                                                                                                              ]
                                                                                                             ]

-- | The block is carried as it was found rather than read apart, because the one place which has to
-- give it back exactly as it was written is the one writing the note back.
testFrontmatterVerbatim :: TestTree
testFrontmatterVerbatim = testCase "the properties are carried as they were written" $
  assertEqual "" [NoteFrontmatter written, NoteWriting "The note."] $ noteParts $ unlines [
    "---",
    "tags:",
    "  - reading",
    "  - web",
    "",
    "title: A title: with a colon in it",
    "---",
    "",
    "The note."
                                                                                          ]
  where
    written = "tags:\n  - reading\n  - web\n\ntitle: A title: with a colon in it"

-- | Which property names the page a note is written over, and what counts as naming one.
testFrontmatterUrl :: TestTree
testFrontmatterUrl = testGroup "the page a note's properties name" [
  names "url" (Just "https://example.com") "url: https://example.com",
  names "a quoted url" (Just "https://example.com") "url: \"https://example.com\"",
  names "roam_refs alone" (Just "https://example.com") "roam_refs: https://example.com",
  names "url before roam_refs" (Just "https://first.com") "url: https://first.com\nroam_refs: https://second.com",
  -- A url naming no page on the web is passed over rather than taken as the answer, which is what
  -- "falling back" to roam_refs means when both are written.
  names "a url which is not an address" (Just "https://example.com") "url: A book I am reading\nroam_refs: https://example.com",
  names "the first entry of a list" (Just "https://first.com") "roam_refs: [https://first.com, https://second.com]",
  -- A note is written over a page on the web. A reference to another note is a link between two
  -- pages of the doc server, which is not something to frame beneath the writing.
  names "a quoted wiki link" Nothing "roam_refs: \"[[Another note]]\"",
  names "a bare wiki link" Nothing "roam_refs: [[Another note]]",
  names "a wiki link written first in a list" Nothing "roam_refs: [[[Another note]], https://example.com]",
  names "no property naming a page" Nothing "tags: reading",
  -- A key indented under another is a part of that one rather than a property of the note.
  names "a url indented under another property" Nothing "source:\n  url: https://example.com"
                                                                   ]
  where
    names what expected properties = testCase what $ assertEqual "" expected $ frontmatterUrl properties

-- | The page a note is written over is a reading of its properties rather than a part of the note,
-- so the page holds it as a statement of its own beside them.
testOverlayStatement :: TestTree
testOverlayStatement = testGroup "the page a note is written over is declared beside its properties" [
  testCase "when the properties name one" $ assertEqual "" [Just "https://example.com"] $ overlaysOf $ unlines [
      "---",
      "url: https://example.com",
      "---",
      "",
      "The note."
                                                                                                               ],
  testCase "and not when they do not" $ assertEqual "" [] $ overlaysOf $ unlines [
      "---",
      "tags: reading",
      "---",
      "",
      "The note."
                                                                                 ],
  testCase "and not when there are no properties" $ assertEqual "" [] $ overlaysOf "The note.\n"
                                                                                                     ]
  where
    overlaysOf note = case notePageStatement Nothing "A note" note of
      RawStatementTree _ parts -> [rawArgStr overlayAnnotUrl annot | RawStatementTree (RawAnnot annot) [] <- parts, named overlayAnnot annot]

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

-- | Which links a note holding properties has. The links are read from the whole of the note, which
-- the properties are read out of separately, so breaking them off must not have cost the note any
-- link it had.
--
-- A link written in a property is not one of them, and was not one before the properties were read
-- as properties: the markdown a note's links are read out of reads a @---@ block as a rule and a
-- heading rather than as prose, and 'blockText' reads the prose. That is worth pinning rather than
-- fixing here — reading a property's links would be a reading of properties, which is what
-- 'frontmatterUrl' does for the one property that names a page.
testLinksInProperties :: TestTree
testLinksInProperties = testGroup "the links of a note which opens with properties" [
  testCase "are the links written in the writing" $ assertEqual "" ["Nested"] $ wikiLinks "note" $ unlines [
      "---",
      "related: \"[[Welcome]]\"",
      "---",
      "",
      "Links to [[Nested]]."
                                                                                                           ],
  testCase "and not the links written in the properties" $ assertEqual "" [] $ wikiLinks "note" $ unlines [
      "---",
      "related: \"[[Welcome]]\"",
      "---",
      "",
      "The note."
                                                                                                          ]
                                                                                    ]

-- | The note a wiki link names, out of the notes a vault holds.
--
-- Written against the notes in the order a walk of the vault finds them, because which note a link
-- names where two could answer is decided by that order and by nothing else. The walk is sorted
-- ('Syntax.Ct.Prgm.walkTree'), so which one that is, is a property of the vault rather than of the
-- order a filesystem happened to answer a listing in.
--
-- No vault on disk: what a link resolves to is decided by the note paths alone, so a test which
-- made files would be testing the walk as well and would say less about either.
testResolveWikiLink :: TestTree
testResolveWikiLink = testGroup "the note a wiki link names" [
  testCase "is found by its name, wherever it is written" $
    assertEqual "" (Just "/v/sub/Two.md") $ resolve ["/v/One.md", "/v/sub/Two.md"] "Two",
  testCase "is found by its path within the vault" $
    assertEqual "" (Just "/v/sub/Two.md") $ resolve ["/v/One.md", "/v/sub/Two.md"] "sub/Two",
  testCase "is found with the markdown extension written out" $
    assertEqual "" (Just "/v/sub/Two.md") $ resolve ["/v/sub/Two.md"] "Two.md",
  testCase "is found by a path with the markdown extension written out" $
    assertEqual "" (Just "/v/sub/Two.md") $ resolve ["/v/sub/Two.md"] "sub/Two.md",
  -- Both readings answer here, with different notes, so this says which is asked first rather than
  -- only that one of them works.
  testCase "is the one its path names before the one its name names" $
    assertEqual "" (Just "/v/a/b.md") $ resolve ["/v/b.md", "/v/a/b.md"] "a/b",
  -- Both notes are written under a directory, so that neither is named by its path alone and the
  -- reading by name is what answers. A note at the top of the vault is named by its path as well as
  -- by its name, and would be answered with whichever way round the two were written.
  --
  -- The pair is what makes this a test of "the first the walk found wins" rather than of "this
  -- particular note wins".
  testCase "is the first of two of the name the walk found" $
    assertEqual "" (Just "/v/a/One.md") $ resolve ["/v/a/One.md", "/v/b/One.md"] "One",
  testCase "is the first of two of the name the walk found, the other way round" $
    assertEqual "" (Just "/v/b/One.md") $ resolve ["/v/b/One.md", "/v/a/One.md"] "One",
  testCase "is nothing for a link naming only a place, which is within the note writing it" $
    assertEqual "" Nothing $ resolve ["/v/One.md"] "#Heading",
  testCase "is nothing for a link naming only the words to show it as" $
    assertEqual "" Nothing $ resolve ["/v/One.md"] "|shown as",
  testCase "is found past the place and the words a link goes on to name" $
    assertEqual "" (Just "/v/sub/Two.md") $ resolve ["/v/sub/Two.md"] "Two#Section|shown as",
  -- Writing a link to a note yet to be written is how one is planned, so this is not an error.
  testCase "is nothing where the vault holds no such note" $
    assertEqual "" Nothing $ resolve ["/v/One.md"] "Three",
  -- Only the markdown extension is dropped, and not whatever extension a name happens to end with:
  -- filing this note under "Version 1" would file it under a name no link writes.
  testCase "is found where its name ends in something which is not the markdown extension" $
    assertEqual "" (Just "/v/Version 1.2.md") $ resolve ["/v/Version 1.2.md"] "Version 1.2"
                                                             ]
  where resolve notes link = resolveWikiLink (mkVaultIndex "/v" notes) link

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
  testLinksThroughTask,
  testNoLinksInCode,
  testNoteParts,
  testNestedTask,
  testNumberedTask,
  testRewrite,
  testRewriteNormalizes,
  testRewriteIdempotent,
  testRewriteOnlyNotes,
  testFrontmatterPart,
  testFrontmatterOnlyAtTheStart,
  testFrontmatterVerbatim,
  testFrontmatterUrl,
  testOverlayStatement,
  testRewriteFrontmatter,
  testRewriteWithOverlay,
  testRewriteFrontmatterIdempotent,
  testLinksInProperties,
  testResolveWikiLink,
  testRewriteTasks,
  testRewriteMixedTasks
                                          ]
