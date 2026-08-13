--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is for importing an Obsidian vault and the notes it holds.
--------------------------------------------------------------------

module Syntax.Obsidian.Parser where

import           Control.Applicative  ((<|>))
import           CtConstants
import           Data.Char            (isDigit, isSpace)
import           Data.Foldable        (toList)
import           Data.List            (dropWhileEnd, intercalate, isPrefixOf,
                                       isSuffixOf, nub, sort)
import           Data.Maybe           (listToMaybe, mapMaybe)
import qualified Data.Text            as T
import           Semantics.Prgm
import           Semantics.Types      (Constant (CStr), makeAbsoluteName,
                                       mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           System.Directory
import           System.FilePath      (makeRelative, takeBaseName,
                                       takeDirectory, (</>))
import qualified Text.MMark           as MMark
import           Text.MMark.Extension (Block (..), Bni, Inline (..), scanner)
import           Text.Printf

-- | The name this parser is registered under in 'Syntax.Parsers.importParsers'.
--
-- Shared with the parser rather than written at the registration because a markdown file within a
-- vault is inferred to be a note of it, by 'Syntax.InferImport.inferFileParser'.
obsidian :: String
obsidian = "obsidian"

-- | The directory in which Obsidian keeps a vault's own configuration, and by which a vault is
-- recognized.
obsidianConfigDir :: FilePath
obsidianConfigDir = ".obsidian"

-- | Reads an Obsidian vault, and writes a note of one back.
--
-- Only a note is written back. A vault is a directory, whose notes are pages of their own and are
-- each written back as themselves, so it has no source text of its own for a reversion to produce.
obsidianParser :: ImportParser
obsidianParser = ImportParser obsidianParse unparseNote

-- | Imports an Obsidian vault, or one note of one.
--
-- A vault is a page of its own whose program is empty, holding every note as an import. A note is a
-- page holding a 'CtConstants.notePagePath' for itself, whose comment is the note as it was
-- written, and importing each of the notes it links to. So the graph the notes of a vault form by
-- linking to each other is the import graph of the pages they are read as. The one part of a note
-- that says something the program can act on is a checkbox, which is read as a
-- 'CtConstants.noteTaskPath' declared where it was written.
--
-- Which of the two an import is comes from what its path names, a directory or a file, so that a
-- vault and its notes are read by the one parser. Neither is named a vault by the import: a vault is
-- the directory holding an 'obsidianConfigDir', as Obsidian itself decides it, and a note finds its
-- own by looking upwards for one. Being able to answer that from the path alone is what lets
-- 'Syntax.InferImport.inferFileParser' route a note here without being told to, which is in turn
-- what makes a note the same page whether the vault, another note or a reader of the doc server
-- named it.
--
-- Importing one directory as both a vault and a plain @dir@ makes two pages at the one display
-- path. 'Eval.Runtime.WebDocs.sidebarLevel' shows a directory holding pages as a single node, so
-- the two are shown as one; an empty one has no node to collapse into and is an error there.
obsidianParse :: Expr () -> ImportParseResult
obsidianParse imp = case [arg | arg@ObjArr{oaObj=Nothing} <- exprAppliedArgs imp] of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr path)), _)}:_) -> do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    case (isDir, isFile) of
      (True, _) -> do
        isVault <- doesDirectoryExist $ path </> obsidianConfigDir
        if isVault
          then vaultPrgm path
          else fail $ printf "The directory %s is not an Obsidian vault, as it holds no %s configuration" path obsidianConfigDir
      (_, True) -> do
        vault <- noteVault path
        case vault of
          Just vault' -> notePrgm vault' path
          Nothing -> fail $ printf "The note %s is in no Obsidian vault, as no directory holding it holds an %s configuration" path obsidianConfigDir
      _ -> fail $ printf "Could not find the Obsidian vault or note %s" path
  _ -> fail $ printf "An obsidian import must name a vault directory or a note within one: %s" (show imp)

-- | The vault a note sits in: the closest directory holding an 'obsidianConfigDir' at or above it.
noteVault :: FilePath -> IO (Maybe FilePath)
noteVault note = aux $ takeDirectory note
  where
    aux dir = do
      isVault <- doesDirectoryExist $ dir </> obsidianConfigDir
      case () of
        _ | isVault                  -> return $ Just dir
        _ | takeDirectory dir /= dir -> aux $ takeDirectory dir
        _                            -> return Nothing

-- | The page of a vault: every note it holds, as an import.
--
-- The notes are imported by their path alone, leaving 'Syntax.InferImport.inferFileParser' to
-- read each one as the note of a vault it is, so that a note the vault imports and the same note a
-- reader asks for by path are the one page.
-- The whole vault is what this page was read from, not the notes: which notes a vault holds is read
-- out of the listings under it, and a note's own writing says nothing about whether the vault holds
-- it. A note added anywhere under the vault is an entry written into one of those listings.
vaultPrgm :: FilePath -> ImportParseResult
vaultPrgm vault = do
  notes <- vaultNotes vault
  return $ ParsedImport (RawPrgm (map (mkRawFileImport . rawStr) notes) []) [] Nothing (Just [ReadTree vault])

-- | The page of one note: what it is, and what it links to.
--
-- The vault is walked again here rather than by the vault page, because a note is a page whether it
-- was reached through its vault or by a link from another note, and either way its links resolve
-- against every note of the vault.
--
-- Read from the note itself and from the whole vault, because both decide what the page says: the
-- note's writing is the page, and which notes the vault holds is what its wiki links resolve
-- against. 'resolveWikiLink' matches a link by basename anywhere under the vault, so a note added
-- three directories away can turn a link that resolved to nothing into an import — which is why the
-- vault is read from as a tree rather than as the directory the note is in.
notePrgm :: FilePath -> FilePath -> ImportParseResult
notePrgm vault note = do
  contents <- readFile note
  notes <- vaultNotes vault
  let linked = nub $ filter (/= note) $ mapMaybe (resolveWikiLink vault notes) $ wikiLinks note contents
  return $ ParsedImport (RawPrgm (map (mkRawFileImport . rawStr) linked) [notePageStatement (takeBaseName note) contents]) [] (Just contents) (Just [ReadFile note, ReadTree vault])

-- | The note itself: the page it is, titled as it is linked to, holding what was written in it.
--
-- The writing is a comment inside the page rather than a field of it, so that a reader of the page
-- reads the note. A comment written under a declaration becomes both its doc comment and a
-- 'CtConstants.mdAnnot' annotation on it, which is the same annotation a whole markdown page is read
-- as by 'Syntax.Md.Parser.mdParser' and which the webdocs already display.
--
-- A task is a break in that writing: the comment ends where the checkbox was written, the task is
-- declared in its place, and what follows goes on as another comment. So the page holds the tasks of
-- the note in the order and the place they were written, and reading it is still reading the note.
-- What is written under a checkbox is written inside its task by the same reading that makes the
-- note the writing inside its page — including a checkbox indented under it, which is a task of that
-- task and is declared inside it.
notePageStatement :: String -> String -> RawStatementTree RawExpr ()
notePageStatement title contents = RawStatementTree (RawDeclStatement page) (map partStatement $ noteParts contents)
  where
    page = RawObjArr (Just pageExpr) TypeObj Nothing [] Nothing Nothing
    pageExpr = rawVal notePagePath `applyRawArgs` [(Just $ partialKey (mkName notePageTitle), rawStr title)]

partStatement :: NotePart -> RawStatementTree RawExpr ()
partStatement (NoteWriting writing) = RawStatementTree (RawAnnot (rawComment writing)) []
partStatement (NoteTask says done under) = RawStatementTree (RawDeclStatement task) (map partStatement under)
  where
    task = RawObjArr (Just taskExpr) TypeObj Nothing [] Nothing Nothing
    taskExpr = rawVal noteTaskPath `applyRawArgs` [
      (Just $ partialKey (mkName noteTaskContents), rawStr says),
      (Just $ partialKey (mkName noteTaskState), rawVal (if done then truePrim else falsePrim))
                                                 ]

-- | Writes a note back as the markdown it was read from: the reversion of 'notePageStatement'.
--
-- The note is written out of the program rather than out of the file it was read from, so what it
-- said survives and how it said it does not. Every checkbox is written with a @-@ marker however it
-- was written, and every task is written at the depth it is declared at rather than the column it
-- was found at. The writing between the tasks is written back as it was found, which is what leaves
-- the rest of the note — its headings, its code, its links — alone.
--
-- A program which is not a note is written as nothing rather than as an empty note, so a page the
-- reversion does not recognise is never written over.
unparseNote :: RawPrgm () -> Maybe String
unparseNote (RawPrgm _ [RawStatementTree (RawDeclStatement page) parts])
  | Just pageExpr <- roaObj page, named notePagePath pageExpr = do
      parts' <- mapM notePart parts
      return $ unlines $ writeNoteParts parts'
unparseNote _ = Nothing

-- | The part of a note a statement of its page was read from, if it was read from one.
notePart :: RawStatementTree RawExpr () -> Maybe NotePart
notePart (RawStatementTree (RawAnnot annot) [])
  | named mdAnnot annot = NoteWriting <$> rawArgStr mdAnnotText annot
notePart (RawStatementTree (RawDeclStatement task) under)
  | Just taskExpr <- roaObj task, named noteTaskPath taskExpr = do
      says <- rawArgStr noteTaskContents taskExpr
      done <- rawArgState taskExpr
      NoteTask says done <$> mapM notePart under
notePart _ = Nothing

-- | Whether an expression is the value of the given name.
--
-- The names are compared as absolute ones, because whether a name is written absolute is a
-- difference between how a page was written and how the desugar leaves it, not a difference in what
-- is named. A page built by 'notePageStatement' names @\/contents@ where one parsed from the source
-- an edit sent names @contents@, and they are the one argument.
named :: String -> RawExpr () -> Bool
named name e = fmap makeAbsoluteName (maybeExprPath e) == Just (makeAbsoluteName $ mkName name)

-- | The expression a named argument of an applied expression is given, if it is given one.
rawArg :: String -> RawExpr () -> Maybe (RawExpr ())
rawArg name e = listToMaybe [v | RawObjArr{roaObj=Just o, roaArr=Just (Just v, _)} <- rawExprAppliedArgs e, named name o]

-- | The string a named argument is given, if it is given one written out as a string.
rawArgStr :: String -> RawExpr () -> Maybe String
rawArgStr name e = case rawArg name e of
  Just (RawCExpr _ (CStr s)) -> Just s
  _                          -> Nothing

-- | Whether a task's box is checked, read from the state it was declared with.
rawArgState :: RawExpr () -> Maybe Bool
rawArgState e = case rawArg noteTaskState e of
  Just state | named truePrim state  -> Just True
             | named falsePrim state -> Just False
  _                                  -> Nothing

-- | The lines a note's parts are written as.
--
-- A blank line goes between the writing of a note and a task of it, as the writing is a paragraph
-- and a paragraph is written apart from what surrounds it. None goes between two tasks, which are
-- items of the one list and are written one after another.
writeNoteParts :: [NotePart] -> [String]
writeNoteParts parts = concat $ zipWith separated (Nothing : map Just parts) parts
  where
    separated before part = ["" | maybe False (apartFrom part) before] ++ writeNotePart part
    apartFrom NoteWriting{} _ = True
    apartFrom _ NoteWriting{} = True
    apartFrom _ _             = False

-- | The lines one part of a note is written as: its writing as it was found, or its checkbox
-- followed by whatever is written inside it, indented under it.
writeNotePart :: NotePart -> [String]
writeNotePart (NoteWriting writing) = lines writing
writeNotePart (NoteTask says done under) = dropWhileEnd isSpace (box ++ says) : map indentLine (writeNoteParts under)
  where
    box = if done then "- [x] " else "- [ ] "
    -- A blank line is written with nothing on it rather than with the indentation of the block it
    -- sits in, so that writing a note back does not leave trailing spaces through it.
    indentLine l = if isBlank l then l else "  " ++ l

-- | What a note is written as: the writing of it, and the tasks that break the writing.
data NotePart
  = NoteWriting String
  -- | What the checkbox says, whether it is checked, and what is written under it — which is the
  -- note again: the writing under the checkbox, and the checkboxes indented under it.
  | NoteTask String Bool [NotePart]
  deriving (Eq, Show)

-- | Breaks a note into the writing and the tasks it holds, in the order they were written.
--
-- Writing that is only blank lines is left out, as two checkboxes written one after the other have
-- nothing written between them, and the blank lines around a run of writing are dropped so that the
-- comment is the writing itself. What is left is written as it was: the indentation of a line is
-- markdown of its own and is not touched.
--
-- Code is passed over, as a checkbox written inside a fenced block is writing about a task rather
-- than making one, which is the reading 'blockText' already gives a wiki link inside code. The state
-- of a fence is carried through what is written under a task as well, so that a block opened there
-- is still open after it.
--
-- What is written under a checkbox is broken the same way, so the parts of a note are a tree: a task
-- holds the writing under it and the tasks indented under it, each broken from the block as this
-- breaks the note.
noteParts :: String -> [NotePart]
noteParts = noteLineParts . lines

-- | 'noteParts' over lines, which is what the writing under a checkbox is already broken into.
noteLineParts :: [String] -> [NotePart]
noteLineParts = go False []
  where
    go _ acc [] = writingPart acc
    go inCode acc (l:ls)
      | isFence l = go (not inCode) (l:acc) ls
      | not inCode, Just (says, done) <- taskLine l = let (under, inCode', ls') = taskWriting (indentation l) inCode ls in
          writingPart acc ++ (NoteTask says done (noteLineParts under) : go inCode' [] ls')
      | otherwise = go inCode (l:acc) ls

    writingPart acc = [NoteWriting writing | let writing = joinLines (reverse acc), not (null writing)]

-- | What is written under a checkbox, where the note goes on, and whether that is inside a fence.
--
-- A line is written within the task while it is indented deeper than the checkbox itself, whether it
-- is a checkbox or not: a checkbox indented deeper is a task of the one above it, written under it as
-- the writing under it is. A checkbox at the same indentation is the next task rather than part of
-- this one, and ends the block.
--
-- What is found is written back out without the indentation it was written at, as markdown reads
-- four spaces of indentation as code and what is written under a task is writing rather than code.
taskWriting :: Int -> Bool -> [String] -> ([String], Bool, [String])
taskWriting ind inCode ls = (dedent within, inCode', rest)
  where
    (within, inCode', rest) = aux [] inCode ls
    aux acc code (l:ls')
      | isBlank l || indentation l > ind = aux (l:acc) (if isFence l then not code else code) ls'
    aux acc code ls' = (reverse acc, code, ls')

-- | What a checkbox line says and whether it is checked, if the line is a checkbox at all.
--
-- A checkbox is a list item whose writing begins with a box: @- [ ] a task@, @1. [x] a task done@.
-- Obsidian writes a state of its own within the box as any one character, such as @[\/]@ for a task
-- begun, so any character is a box. The checked one is @x@, as markdown itself reads it, and every
-- other box, the empty one included, is unchecked.
taskLine :: String -> Maybe (String, Bool)
taskLine line = do
  afterMarker <- listMarker $ dropWhile isSpace line
  case afterMarker of
    ('[':box:']':rest) | box /= ']', null rest || isSpace (head rest) -> Just (trim rest, box `elem` ['x', 'X'])
    _ -> Nothing

-- | What a list item is written after, if a line begins one: a bullet or a number, and the space it
-- is followed by.
listMarker :: String -> Maybe String
listMarker (bullet:c:rest) | bullet `elem` ['-', '*', '+'], isSpace c = Just $ dropWhile isSpace rest
listMarker s = case span isDigit s of
  (digits, marker:c:rest) | not (null digits), marker `elem` ['.', ')'], isSpace c -> Just $ dropWhile isSpace rest
  _ -> Nothing

-- | The writing a run of lines is, without the blank lines around it.
joinLines :: [String] -> String
joinLines = intercalate "\n" . dropWhileEnd isBlank . dropWhile isBlank

-- | Writes a run of lines without the indentation the whole of it was written at.
dedent :: [String] -> [String]
dedent ls = map (drop ind) ls
  where
    ind = minimum $ maxBound : map indentation (filter (not . isBlank) ls)

-- | Whether a line opens or closes a fenced code block.
isFence :: String -> Bool
isFence l = any (`isPrefixOf` dropWhile isSpace l) ["```", "~~~"]

-- | Whether a line is written on at all.
isBlank :: String -> Bool
isBlank = all isSpace

-- | The width a line is written at.
indentation :: String -> Int
indentation = length . takeWhile isSpace

-- | The notes of a vault: every markdown file under it, at any depth.
--
-- This is the reading a 'Syntax.Ct.Prgm.ReadTree' of the vault stands for, so what it walks into is
-- 'Syntax.Ct.Prgm.readsTreeEntry' rather than a rule of its own — the whole point of that being one
-- rule is that this is the reading everything else has to agree with. Directory symlinks are not
-- followed, as a vault holding one that points at an ancestor would otherwise be walked forever.
--
-- Sorted, because 'listDirectory' answers in whatever order the filesystem holds it and the notes
-- become the vault page's imports — a part of the program that re-reading an unchanged vault must
-- reproduce, or 'CtService.ctssRead' takes every read for an edit and rebuilds the vault.
vaultNotes :: FilePath -> IO [FilePath]
vaultNotes dir = do
  entries <- listDirectory dir
  notes <- mapM aux $ sort $ filter readsTreeEntry entries
  return $ concat notes
  where
    aux entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      isLink <- pathIsSymbolicLink path
      case () of
        _ | isDir && not isLink                   -> vaultNotes path
        _ | not isDir && ".md" `isSuffixOf` entry -> return [path]
        _                                         -> return []

-- | The wiki links a note writes, in the order it writes them.
--
-- A @[[wiki link]]@ is not markdown, and is not a link to the markdown parser either: it is a
-- reference to a definition that a note never gives, which is an error rather than a link. So each
-- one is taken out of the note and replaced by a placeholder before the note is parsed, and the
-- links are read back out of the placeholders that survive into the parsed note's prose. What
-- markdown is being parsed for is which of them are prose at all: a @[[link]]@ inside code is
-- writing about a link rather than making one, and 'blockText' leaves code out.
--
-- A note whose markdown cannot be parsed has no links. It is still a page, holding what was written
-- in it, so the vault it belongs to still reads. Reporting the parse failure rather than passing
-- over it needs an import parser that can answer with notes, which is the TODO on
-- 'Syntax.Parsers.readImport'.
wikiLinks :: FilePath -> String -> [String]
wikiLinks note contents = case MMark.parse note (T.pack masked) of
  Left _    -> []
  Right doc -> unmaskWikiLinks links $ intercalate "\n" $ map blockText $ reverse $ MMark.runScanner doc (scanner [] (flip (:)))
  where
    (masked, links) = maskWikiLinks $ unboxed contents

-- | The note with the box of each checkbox taken out of it.
--
-- A box is a reference to a definition that a note never gives, which is exactly what a wiki link is
-- to the markdown parser, so a note holding a single checkbox parses as no markdown at all and would
-- be read as a note that links to nothing. A checkbox is written back as the list item it is,
-- indented where it was and holding what it says, as what a checkbox says is writing of the note
-- like any other.
unboxed :: String -> String
unboxed = intercalate "\n" . map aux . lines
  where
    aux l = case taskLine l of
      Just (says, _) -> takeWhile isSpace l ++ "- " ++ says
      Nothing        -> l

-- | Replaces every wiki link of a note with a placeholder, answering the note and the links taken
-- out of it.
--
-- The placeholder is written with characters from the private use area, which no markdown syntax
-- gives a meaning to, so that a link target holding a @|@, a @*@ or a @#@ is neither read as
-- markdown nor has to be escaped as though it were.
--
-- A wiki link is written on one line, so an unclosed @[[@ is left alone rather than swallowing the
-- rest of the note.
maskWikiLinks :: String -> (String, [String])
maskWikiLinks = aux (0 :: Int)
  where
    aux _ [] = ([], [])
    aux i s@(c:cs) = case wikiLinkAt s of
      Just (link, rest) -> let (rest', links) = aux (i + 1) rest in (maskOpen : show i ++ maskClose : rest', link : links)
      Nothing           -> let (cs', links) = aux i cs in (c : cs', links)

    -- An embed @![[note]]@ shows another note within this one, which is a link to it
    wikiLinkAt ('!':'[':'[':rest) = closedOnLine rest
    wikiLinkAt ('[':'[':rest)     = closedOnLine rest
    wikiLinkAt _                  = Nothing

    closedOnLine s = do
      (link, rest) <- splitOnFirst "]]" s
      if '\n' `elem` link then Nothing else Just (link, rest)

-- | Reads the links back out of the placeholders 'maskWikiLinks' left, in the order they are found.
unmaskWikiLinks :: [String] -> String -> [String]
unmaskWikiLinks links = aux
  where
    aux s = case dropWhile (/= maskOpen) s of
      []       -> []
      (_:rest) -> case span isDigit rest of
        (i, c:rest') | c == maskClose, not (null i) -> maybe id (:) (lookup (read i) indexed) $ aux rest'
        _                                           -> aux rest
    indexed = zip [(0 :: Int) ..] links

-- | The characters a masked wiki link is written between.
maskOpen, maskClose :: Char
maskOpen = '\xE000'
maskClose = '\xE001'

-- | Splits at the first occurrence of a separator, if there is one.
splitOnFirst :: String -> String -> Maybe (String, String)
splitOnFirst sep = aux ""
  where
    aux _ [] = Nothing
    aux acc s@(c:rest)
      | sep `isPrefixOf` s = Just (reverse acc, drop (length sep) s)
      | otherwise = aux (c : acc) rest

-- | The prose of a block: the text of it a reader reads.
--
-- Code is left out, as writing @[[a link]]@ inside code is writing about a link rather than making
-- one. The blocks a block holds are walked here because 'MMark.runScanner' answers only the blocks
-- of the note itself.
blockText :: Bni -> String
blockText ThematicBreak       = ""
blockText (Heading1 is)       = inlinesText is
blockText (Heading2 is)       = inlinesText is
blockText (Heading3 is)       = inlinesText is
blockText (Heading4 is)       = inlinesText is
blockText (Heading5 is)       = inlinesText is
blockText (Heading6 is)       = inlinesText is
blockText (CodeBlock _ _)     = ""
blockText (Naked is)          = inlinesText is
blockText (Paragraph is)      = inlinesText is
blockText (Blockquote bs)     = intercalate "\n" $ map blockText bs
blockText (OrderedList _ bss) = intercalate "\n" $ map (intercalate "\n" . map blockText) $ toList bss
blockText (UnorderedList bss) = intercalate "\n" $ map (intercalate "\n" . map blockText) $ toList bss
blockText (Table _ rows)      = intercalate "\n" $ map inlinesText $ concatMap toList $ toList rows

-- | The prose of a run of inline markdown. A link's own text is prose; the address it points at is
-- not, and neither is code.
inlinesText :: Foldable f => f Inline -> String
inlinesText = concatMap inlineText . toList

inlineText :: Inline -> String
inlineText (Plain t)        = T.unpack t
inlineText LineBreak        = "\n"
inlineText (Emphasis is)    = inlinesText is
inlineText (Strong is)      = inlinesText is
inlineText (Strikeout is)   = inlinesText is
inlineText (Subscript is)   = inlinesText is
inlineText (Superscript is) = inlinesText is
inlineText (CodeSpan _)     = ""
inlineText (Link is _ _)    = inlinesText is
inlineText (Image is _ _)   = inlinesText is

-- | The note of a vault a wiki link points at, if the vault holds one.
--
-- A link names a note either by its path within the vault or by its name alone, with or without the
-- markdown extension, and may go on to name a place within it and the words to show it as. A link
-- naming no note at all is not an error: writing one is how a note yet to be written is planned,
-- and Obsidian shows it as unresolved rather than refusing it.
resolveWikiLink :: FilePath -> [FilePath] -> String -> Maybe FilePath
resolveWikiLink vault notes link = case wikiLinkTarget link of
  ""     -> Nothing -- A link naming only a place is within the note that wrote it
  target -> listToMaybe (byPath target) <|> listToMaybe (byName target)
  where
    byPath target = [note | note <- notes, dropExtension' (makeRelative vault note) == dropExtension' target]
    byName target = [note | note <- notes, takeBaseName note == dropExtension' target]
    dropExtension' p = if ".md" `isSuffixOf` p then take (length p - 3) p else p

-- | The note a wiki link names, without the place within it or the words it is shown as.
wikiLinkTarget :: String -> String
wikiLinkTarget = trim . takeWhile (/= '#') . takeWhile (/= '|')

-- | Text without the whitespace written around it.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
