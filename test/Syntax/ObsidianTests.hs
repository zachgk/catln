-- | Tests for reading a note of an Obsidian vault, and writing one back.
--
-- What a note is read as is covered by the golden tests of the vault in
-- @test\/Integration\/code\/artifacts\/catln-test@, but only as far as the declarations it makes: a
-- page's golden shows neither the imports of the page nor the comments inside it. So the things a
-- note holds that a golden cannot show are tested here — which notes it links to, how its writing is
-- broken, and what it is written back as.
module Syntax.ObsidianTests where

import           CtConstants              (mdAnnot, mdAnnotText, overlayAnnot,
                                           overlayAnnotUrl)
import           Data.Char                (isSpace)
import           Data.List                (dropWhileEnd, intercalate)
import           Data.Maybe               (maybeToList)
import           Semantics                (testCtssConfig)
import           Syntax.Ct.Prgm           (RawPrgm (RawPrgm),
                                           RawStatement (RawAnnot),
                                           RawStatementTree (RawStatementTree))
import           Syntax.Obsidian.Parser   (NotePart (..), frontmatterUrl,
                                           mkVaultIndex, named,
                                           notePageStatement, noteParts,
                                           rawArgStr, resolveWikiLink,
                                           restoreWikiLinks, unparseNote,
                                           wikiHref, wikiLinks)
import           Syntax.Obsidian.Settings (TaskSettings (..),
                                           defaultTaskSettings)
import           Syntax.Parsers           (mkDesCanonicalImportStr, pageLinkDir)
import           System.Directory         (createDirectoryIfMissing,
                                           getTemporaryDirectory,
                                           removePathForcibly)
import           System.FilePath          ((</>))
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

-- | A block of markdown is written between more delimiters than the fences inside it, so a fence of
-- the block is not the end of the block. What is written after the inner one is still code, and a
-- checkbox written there is still writing about a task rather than a task.
testFenceWithinFence :: TestTree
testFenceWithinFence = testCase "a fence inside a longer fence does not close it" $
  assertEqual "" [NoteWriting $ dropWhileEnd isSpace note] $ noteParts note
  where
    note = unlines [
      "````",
      "```",
      "- [ ] A checkbox inside code",
      "```",
      "````"
                   ]

-- | A block is closed by the character it was opened with. A note writing one of each has two
-- blocks rather than one block holding the other's delimiters.
testFenceOfOtherCharacter :: TestTree
testFenceOfOtherCharacter = testCase "a fence is not closed by the other character" $
  assertEqual "" [NoteWriting $ dropWhileEnd isSpace note] $ noteParts note
  where
    note = unlines [
      "~~~",
      "```",
      "- [ ] A checkbox inside code",
      "```",
      "~~~"
                   ]

-- | A line of a block which begins with the delimiter and says more is a line of the block. Only a
-- line saying nothing after it closes one.
testFenceClosedBySelfAlone :: TestTree
testFenceClosedBySelfAlone = testCase "a fence line saying more does not close a block" $
  assertEqual "" [NoteWriting $ dropWhileEnd isSpace note] $ noteParts note
  where
    note = unlines [
      "```python",
      "```haskell",
      "- [ ] A checkbox inside code",
      "```"
                   ]

-- | The state of a fence is carried across what is written under a checkbox, so a block opened and
-- closed there leaves the note out of code and a checkbox after it is a task.
testFenceWithinTask :: TestTree
testFenceWithinTask = testCase "a block opened and closed under a task ends there" $
  assertEqual "" [
    NoteTask " " "Read it" [NoteWriting "```\n- [ ] A checkbox inside code\n```"],
    NoteTask " " "Write it" []
                 ] $ noteParts $ unlines [
    "- [ ] Read it",
    "  ```",
    "  - [ ] A checkbox inside code",
    "  ```",
    "- [ ] Write it"
                                         ]

-- | And a block opened under a checkbox and never closed goes on being a block after it, so what
-- follows is code rather than the note again.
testFenceOpenedWithinTask :: TestTree
testFenceOpenedWithinTask = testCase "a block opened under a task and not closed goes on after it" $
  assertEqual "" [
    NoteTask " " "Read it" [NoteWriting "```"],
    NoteWriting "- [ ] A checkbox inside code"
                 ] $ noteParts $ unlines [
    "- [ ] Read it",
    "  ```",
    "- [ ] A checkbox inside code"
                                         ]

-- | A checkbox is a list item, and a list is written with numbers as readily as with bullets.
testNumberedTask :: TestTree
testNumberedTask = testCase "a numbered checkbox is a task" $
  assertEqual "" [NoteTask " " "Read it" [], NoteTask "x" "Write it" []] $ noteParts $ unlines [
    "1. [ ] Read it",
    "2) [x] Write it"
                                                                                                 ]

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
-- running no plugin, because how a checkbox of a vault running one is read is
-- 'Syntax.ObsidianTaskTests' and what a note holding one is written back as is 'testRewriteTasks'.
rewriteWith :: Maybe TaskSettings -> String -> Maybe String
rewriteWith settings note = unparseNote $ RawPrgm [] [notePageStatement settings vaultHref "A note" note]

-- | The writing of a note as its page holds it: the comments its runs of writing are read as, at
-- whatever depth they are written, since the writing under a checkbox is writing of the note too.
--
-- Read out of the page rather than from 'Syntax.Obsidian.Parser.embedWikiLinks' directly, so that
-- what is tested is what a reader of the page sees rather than a function the page might not be
-- using.
writingOf :: String -> [String]
writingOf note = aux [notePageStatement Nothing vaultHref "A note" note]
  where
    aux trees = concat [written tree ++ aux under | tree@(RawStatementTree _ under) <- trees]
    written (RawStatementTree (RawAnnot annot) []) | named mdAnnot annot = maybeToList $ rawArgStr mdAnnotText annot
    written _ = []

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
    overlaysOf note = case notePageStatement Nothing vaultHref "A note" note of
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

-- | The writing of a note holds its wiki links as the links they are: a link naming a note of the
-- vault is the markdown link to the page that note is read as, which is a link the webdocs already
-- follow.
--
-- The address is the note's path within the vault however the link named the note, since that is the
-- one spelling the writing can be read back as a wiki link from ('wikiHref').
testEmbedWikiLinks :: TestTree
testEmbedWikiLinks = testGroup "a wiki link is written as the link to the note it names" [
  embeds "naming a note by its name" "Links to [[Welcome]]." "Links to [Welcome](Welcome.md).",
  embeds "naming a note by its path within the vault" "Links to [[Notes/Nested]]." "Links to [Notes/Nested](Notes/Nested.md).",
  embeds "naming a note in a folder by its name alone" "Links to [[Nested]]." "Links to [Nested](Notes/Nested.md).",
  embeds "naming the words to show it as" "Links to [[Welcome|the first note]]." "Links to [the first note](Welcome.md).",
  embeds "naming a place within a note" "Links to [[Welcome#Getting started]]." "Links to [Welcome#Getting started](Welcome.md#Getting%20started).",
  -- Both the address and the words have a character in them a markdown link cannot hold as it is.
  embeds "naming a note whose name holds a space" "Links to [[test link]]." "Links to [test link](test%20link.md).",
  -- Writing a link to a note yet to be written is how one is planned, and Obsidian shows it as
  -- unresolved rather than refusing it, so it stays the writing it was.
  embeds "naming no note of the vault" "Links to [[create a link]]." "Links to [[create a link]].",
  embeds "written more than once on a line" "[[Welcome]] and [[Nested]]." "[Welcome](Welcome.md) and [Nested](Notes/Nested.md).",
  -- An embed shows the note it names within this one, which a link cannot do.
  embeds "which is an embed" "Shows ![[Welcome]] within it." "Shows ![[Welcome]] within it.",
  testCase "and not one written inside code" $
    assertEqual "" [intercalate "\n" ["Links to [Welcome](Welcome.md).", "", "```", "Not [[Nested]].", "```"]] $ writingOf $ unlines [
      "Links to [[Welcome]].",
      "",
      "```",
      "Not [[Nested]].",
      "```"
                                                                                                                          ],
  -- A task carries the line it was written as and is shown as the words of it rather than as
  -- markdown, so a link written on a checkbox line is not writing this rewrites.
  testCase "and not one written on a checkbox line" $
    assertEqual "" (Just "- [ ] Read [[Welcome]]\n") $ rewrite "- [ ] Read [[Welcome]]\n",
  -- The writing under a checkbox is writing of the note like any other.
  testCase "including one written under a checkbox" $
    assertEqual "" ["Which is in [Welcome](Welcome.md)."] $ writingOf "- [ ] Read it\n  Which is in [[Welcome]].\n"
                                                                                        ]
  where
    -- Trailing newline dropped, as the writing of a note is the writing itself ('noteParts').
    embeds what note expected = testCase what $ assertEqual "" [expected] $ writingOf note

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

-- | Where a relative link written in a page's documentation resolves, which for a note is the vault
-- holding it and for everything else is the directory the page sits in.
--
-- This is the whole of what the rest of the compiler knows about a vault. A note names another note
-- by that note's path within the vault ('Syntax.Obsidian.Parser.wikiHref'), so a note whose links
-- resolved against its own folder would follow every link written in a subfolder to the wrong place.
--
-- The one case here that makes files, because a vault is a directory holding a directory and nothing
-- but the filesystem can answer whether one is. The rest of this module decides what a link means
-- from the note paths alone, which says more about either.
--
-- The program written inside the vault is the point of the second case rather than a curiosity:
-- someone may keep one there, and what makes a page a note is the syntax which read it, not where
-- the file happens to sit. Both files are written in the same folder so the two answers differ.
testPageLinkDir :: TestTree
testPageLinkDir = testCase "a note's links resolve against its vault, and a program's against its folder" $ do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "catln-obsidian-links"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> ".obsidian")
  createDirectoryIfMissing True (dir </> "Notes")
  writeFile (dir </> "Notes" </> "Nested.md") "A note.\n"
  writeFile (dir </> "Notes" </> "prog.ct") "main = 42\n"

  note <- mkDesCanonicalImportStr testCtssConfig (dir </> "Notes" </> "Nested.md")
  assertEqual "the note" (Just dir) =<< pageLinkDir note

  prgm <- mkDesCanonicalImportStr testCtssConfig (dir </> "Notes" </> "prog.ct")
  assertEqual "the program beside it" (Just (dir </> "Notes")) =<< pageLinkDir prgm

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
  testFenceWithinFence,
  testFenceOfOtherCharacter,
  testFenceClosedBySelfAlone,
  testFenceWithinTask,
  testFenceOpenedWithinTask,
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
  testEmbedWikiLinks,
  testRestoreWikiLinks,
  testPageLinkDir,
  testRewriteLinks,
  testRewriteLinksIdempotent,
  testRewriteTasks,
  testRewriteMixedTasks
                                          ]
