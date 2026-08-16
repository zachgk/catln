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

import           Control.Applicative   ((<|>))
import           Control.Monad         (forM_)
import           CtConstants
import           Data.Char             (isDigit, isSpace)
import           Data.Foldable         (toList)
import qualified Data.HashMap.Strict   as H
import           Data.IORef            (IORef, modifyIORef', newIORef,
                                        readIORef)
import           Data.List             (dropWhileEnd, intercalate, isInfixOf,
                                        isPrefixOf, isSuffixOf, nub,
                                        stripPrefix)
import           Data.Maybe            (listToMaybe, mapMaybe, maybeToList)
import qualified Data.Text             as T
import           Semantics.Prgm
import           Semantics.Types       (Constant (CStr), makeAbsoluteName,
                                        mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Obsidian.Cache (ObsidianCache, cachedNoteLinks,
                                        flushObsidianCache, keepOnlyNotes,
                                        newObsidianCache, recordNoteLinks)
import           System.Directory
import           System.FilePath       (makeRelative, takeBaseName,
                                        takeDirectory, (</>))
import qualified Text.MMark            as MMark
import           Text.MMark.Extension  (Block (..), Bni, Inline (..), scanner)
import           Text.Printf

-- | The name this parser is registered under in 'Syntax.Parsers.importSyntaxes'.
--
-- Shared with the parser rather than written at the registration because a markdown file within a
-- vault is inferred to be a note of it, by 'Syntax.InferImport.inferFileParser'.
obsidian :: String
obsidian = "obsidian"

-- | The directory in which Obsidian keeps a vault's own configuration, and by which a vault is
-- recognized.
obsidianConfigDir :: FilePath
obsidianConfigDir = ".obsidian"

-- | Reads an Obsidian vault, or one note of one, and writes a note back.
--
-- A vault is a page of its own whose program is empty, holding every note as an import. A note is a
-- page holding a 'CtConstants.notePagePath' for itself, whose comment is the note as it was
-- written, and importing each of the notes it links to. So the graph the notes of a vault form by
-- linking to each other is the import graph of the pages they are read as. The one part of a note
-- that says something the program can act on is a checkbox, which is read as a
-- 'CtConstants.noteTaskPath' declared where it was written.
--
-- Only a note is written back. A vault is a directory, whose notes are pages of their own and are
-- each written back as themselves, so it has no source text of its own for a reversion to produce.
obsidianSyntax :: ImportSyntax
obsidianSyntax = ImportSyntax newObsidianReader unparseNote

-- | What one read has found of the Obsidian vaults it reads.
data ObsidianReader = ObsidianReader {
  -- | The read's own walks of the trees it has walked. A vault's notes are the markdown files one
  -- of those walks found, and that same walk is what times the vault's page — see 'vaultNotes'.
  orEnv     :: ReadEnv,
  -- | Where each vault this read has resolved a link against has its notes looked up.
  --
  -- Per read rather than per note, because every note of a vault resolves against the same notes;
  -- and per read rather than kept, because what a vault holds is whatever a walk of it found.
  orIndexes :: IORef (H.HashMap FilePath VaultIndex),
  -- | What an earlier run recorded of the vaults this read reaches, and what this read has to write
  -- back to them. Absent for a read which is to read every note it reaches.
  --
  -- Held for the whole read because a read reads many notes of one vault, and each of them would
  -- otherwise read that vault's record from disk for itself. This is the record 'orIndexes' above
  -- says a read does not keep between reads, and the difference is that this one decides for itself
  -- whether it still holds ('Syntax.Obsidian.Cache.cachedNoteLinks') rather than being trusted
  -- because a read is under way.
  orCache   :: Maybe ObsidianCache
                                     }

-- | The reader one read reads Obsidian vaults with.
newObsidianReader :: ReadEnv -> IO ImportReader
newObsidianReader env = do
  indexes <- newIORef H.empty
  cache <- traverse newObsidianCache (reCacheRoot env)
  let reader = ObsidianReader env indexes cache
  return $ ImportReader (obsidianPreload reader) (obsidianParse reader) (mapM_ flushObsidianCache cache)

-- | Which of the two an obsidian import names: a vault, or a note and the vault holding it.
data ObsidianImport
  = ObsidianVault FilePath
  | ObsidianNote FilePath FilePath

-- | Which of the two an import names, which comes from what its path names, a directory or a file,
-- so that a vault and its notes are read by the one parser. Neither is named a vault by the import:
-- a vault is the directory holding an 'obsidianConfigDir', as Obsidian itself decides it, and a note
-- finds its own by looking upwards for one. Being able to answer that from the path alone is what
-- lets
-- 'Syntax.InferImport.inferFileParser' route a note here without being told to, which is in turn
-- what makes a note the same page whether the vault, another note or a reader of the doc server
-- named it.
--
-- Importing one directory as both a vault and a plain @dir@ makes two pages at the one display
-- path. 'Eval.Runtime.WebDocs.sidebarLevel' shows a directory holding pages as a single node, so
-- the two are shown as one; an empty one has no node to collapse into and is an error there.
obsidianImport :: Expr () -> IO ObsidianImport
obsidianImport imp = case [arg | arg@ObjArr{oaObj=Nothing} <- exprAppliedArgs imp] of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr path)), _)}:_) -> do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    case (isDir, isFile) of
      (True, _) -> do
        isVault <- doesDirectoryExist $ path </> obsidianConfigDir
        if isVault
          then return $ ObsidianVault path
          else fail $ printf "The directory %s is not an Obsidian vault, as it holds no %s configuration" path obsidianConfigDir
      (_, True) -> do
        vault <- noteVault path
        case vault of
          Just vault' -> return $ ObsidianNote vault' path
          Nothing -> fail $ printf "The note %s is in no Obsidian vault, as no directory holding it holds an %s configuration" path obsidianConfigDir
      _ -> fail $ printf "Could not find the Obsidian vault or note %s" path
  _ -> fail $ printf "An obsidian import must name a vault directory or a note within one: %s" (show imp)

obsidianPreload :: ObsidianReader -> Expr () -> ImportPreloadResult
obsidianPreload reader imp = do
  imp' <- obsidianImport imp
  case imp' of
    ObsidianVault vault     -> vaultPreload reader vault
    ObsidianNote vault note -> notePreload reader vault note

obsidianParse :: ObsidianReader -> Expr () -> ImportParseResult
obsidianParse reader imp = do
  imp' <- obsidianImport imp
  case imp' of
    ObsidianVault vault     -> return $ vaultPrgm vault
    ObsidianNote vault note -> noteParse reader vault note

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

-- | The page of a vault, which is empty: what a vault holds is its notes, and each of them is a
-- page of its own ('vaultPreload').
--
-- The notes are named by their path alone, leaving 'Syntax.InferImport.inferFileParser' to
-- read each one as the note of a vault it is, so that a note the vault names and the same note a
-- reader asks for by path are the one page.
--
-- They are named as imports the program does not itself write ('Syntax.Ct.Prgm.piExtraImports'),
-- which is how 'Syntax.InferImport.dirSyntax' names the files of a directory. The read walks on to
-- every one of them, so a vault of any size is still every note as a page of its own; what the
-- vault does not have is an edge to each of them. A vault which imported its notes would be a page
-- reaching the whole vault, and the doc server opens at the page it was given
-- ('CtService.ctssRootPage') — so opening a vault would be compiling all of it, and no page of it
-- could be compiled by itself. The notes are not a program the vault writes: the vault holds them,
-- and each says what it imports by linking.
--
-- The whole vault is what this page was read from, not the notes: which notes a vault holds is read
-- out of the listings under it, and a note's own writing says nothing about whether the vault holds
-- it. A note added anywhere under the vault is an entry written into one of those listings.
vaultPrgm :: FilePath -> ParsedImport
vaultPrgm vault = ParsedImport (RawPrgm [] []) Nothing (Just [ReadTree vault])

-- | That page, and every note of the vault as a page the read walks on to.
vaultPreload :: ObsidianReader -> FilePath -> ImportPreloadResult
vaultPreload reader vault = do
  notes <- vaultNotes reader vault
  -- Which notes the vault holds is read here and nowhere else, so this is the one place a record
  -- of the vault can be told that a note it holds something about is gone.
  forM_ (orCache reader) $ \cache -> keepOnlyNotes cache vault notes
  return (eagerPreload $ vaultPrgm vault){plExtraImports = map (mkRawFileImport . rawStr) notes}

-- | The page of one note: what it is, and what it links to.
--
-- The vault is asked for here rather than left to the vault page, because a note is a page whether
-- it was reached through its vault or by a link from another note, and either way its links resolve
-- against every note of the vault. It is asked for and neither walked nor indexed: the read walks a
-- tree once ('Syntax.Ct.Prgm.scanTree') and indexes a vault once ('vaultIndex'), so a vault of a
-- thousand notes is one walk and one index rather than a thousand of each.
noteParse :: ObsidianReader -> FilePath -> FilePath -> ImportParseResult
noteParse reader vault note = do
  contents <- readFile note
  index <- vaultIndex reader vault
  let linked = nub $ filter (/= note) $ mapMaybe (resolveWikiLink index) $ wikiLinks note contents
  forM_ (orCache reader) $ \cache -> recordNoteLinks cache vault note linked
  return $ ParsedImport (RawPrgm (noteImports linked) [notePageStatement (takeBaseName note) contents]) (Just contents) (Just $ noteReads note linked)

-- | Where a note sits in the graph: the notes it links to, out of what an earlier run recorded of
-- it, and out of the note itself where there is no record of it to take them from.
--
-- Whether a record still holds belongs to the record itself
-- ('Syntax.Obsidian.Cache.cachedNoteLinks'). This is the one syntax which can say where a page sits
-- without reading it, so it is the one whose pages are left to be read the rest of the way.
notePreload :: ObsidianReader -> FilePath -> FilePath -> ImportPreloadResult
notePreload reader vault note = do
  recorded <- case orCache reader of
    Nothing    -> return Nothing
    Just cache -> cachedNoteLinks cache vault note
  case recorded of
    Just linked -> return $ PreloadImport (noteImports linked) [] Nothing (Just $ noteReads note linked)
    Nothing     -> eagerPreload <$> noteParse reader vault note

-- | A note's links, as the imports of the page it is read as.
noteImports :: [FilePath] -> [RawFileImport]
noteImports = map (mkRawFileImport . rawStr)

-- | What reading a note reads: the note itself, and the notes it links to.
--
-- Its own writing is the page, and a note it links to going away or moving is what makes the page's
-- imports wrong: an import is replayed even where the page is given back unread
-- ('Syntax.Parsers.readFiles'), so a note left importing a note that has been deleted is a read that
-- fails rather than a link that dangles. Reading a note from the notes it links to is what has it
-- read again in time to drop the import.
--
-- A linked note is read as a 'Syntax.Ct.Prgm.ReadExists' rather than a 'Syntax.Ct.Prgm.ReadFile',
-- because nothing in it was read. What is in a linked note is that note's own page, so writing to it
-- must read that page again and no other — otherwise a note linked to from a hundred notes is a
-- hundred notes read again every time it is typed into.
--
-- NOT read from the vault, though the whole vault is what the links resolved against. A note added
-- anywhere under the vault could turn a link that resolved to nothing into an import, and saying so
-- would be every note of a vault read again for every note added to it — the cost of a change being
-- the size of the vault rather than the size of the change. So a link that resolved to nothing is
-- left resolving to nothing until the note writing it is read again for a reason of its own. A link
-- that did resolve is watched through the note it resolved to, so deleting, moving or renaming that
-- note is seen.
noteReads :: FilePath -> [FilePath] -> [ReadPath]
noteReads note linked = ReadFile note : map ReadExists linked

-- | The note itself: the page it is, titled as it is linked to, holding what was written in it.
--
-- The writing is a comment inside the page rather than a field of it, so that a reader of the page
-- reads the note. A comment written under a declaration becomes both its doc comment and a
-- 'CtConstants.mdAnnot' annotation on it, which is the same annotation a whole markdown page is read
-- as by 'Syntax.Md.Parser.mdSyntax' and which the webdocs already display.
--
-- A task is a break in that writing: the comment ends where the checkbox was written, the task is
-- declared in its place, and what follows goes on as another comment. So the page holds the tasks of
-- the note in the order and the place they were written, and reading it is still reading the note.
-- What is written under a checkbox is written inside its task by the same reading that makes the
-- note the writing inside its page — including a checkbox indented under it, which is a task of that
-- task and is declared inside it.
notePageStatement :: String -> String -> RawStatementTree RawExpr ()
notePageStatement title contents = RawStatementTree (RawDeclStatement page) (concatMap partStatements $ noteParts contents)
  where
    page = RawObjArr (Just pageExpr) TypeObj Nothing [] Nothing Nothing
    pageExpr = rawVal notePagePath `applyRawArgs` [(Just $ partialKey (mkName notePageTitle), rawStr title)]

-- | The statements one part of a note is read as: what the part says, and what the page says
-- because of it.
--
-- Only the properties say anything about the page. A page on the web named in them is the page
-- this note is written over, which is a reading of the note rather than a part of it, so it is a
-- statement of its own standing beside the properties it was read out of — where the note writes
-- them, as a task is declared where the note writes its checkbox.
partStatements :: NotePart -> [RawStatementTree RawExpr ()]
partStatements part@(NoteFrontmatter properties) = partStatement part : map overlayStatement (maybeToList $ frontmatterUrl properties)
partStatements part = [partStatement part]

partStatement :: NotePart -> RawStatementTree RawExpr ()
partStatement (NoteWriting writing) = RawStatementTree (RawAnnot (rawComment writing)) []
partStatement (NoteFrontmatter properties) = RawStatementTree (RawAnnot (rawProperties properties)) []
partStatement (NoteTask says done under) = RawStatementTree (RawDeclStatement task) (map partStatement under)
  where
    task = RawObjArr (Just taskExpr) TypeObj Nothing [] Nothing Nothing
    taskExpr = rawVal noteTaskPath `applyRawArgs` [
      (Just $ partialKey (mkName noteTaskContents), rawStr says),
      (Just $ partialKey (mkName noteTaskState), rawVal (if done then truePrim else falsePrim))
                                                 ]

-- | The page a note is written over, as the statement saying so.
overlayStatement :: String -> RawStatementTree RawExpr ()
overlayStatement url = RawStatementTree (RawAnnot (rawOverlay url)) []

-- | The properties a note opens with, and the page it is written over, as the annotations the
-- webdocs show them as.
rawProperties, rawOverlay :: String -> RawExpr ()
rawProperties properties = rawVal notePropertiesAnnot `applyRawArgs` [(Just $ partialKey (mkName notePropertiesAnnotText), rawStr properties)]
rawOverlay url = rawVal overlayAnnot `applyRawArgs` [(Just $ partialKey (mkName overlayAnnotUrl), rawStr url)]

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
      parts' <- mapM notePart $ filter (not . derivedStatement) parts
      return $ unlines $ writeNoteParts parts'
unparseNote _ = Nothing

-- | Whether a statement of a note's page was derived from the note rather than read out of it.
--
-- A derived statement is written back as nothing, because what it was derived from is written back
-- as itself: the overlay is the address the note's properties name, and writing the note back
-- writes those properties. It is passed over here rather than met by 'notePart', which answers
-- Nothing for a statement it does not know — and a Nothing is a page which cannot be written back
-- at all ('Syntax.Parsers.unparseImport'), so a note would become unsaveable by naming a page.
derivedStatement :: RawStatementTree RawExpr () -> Bool
derivedStatement (RawStatementTree (RawAnnot annot) []) = named overlayAnnot annot
derivedStatement _                                      = False

-- | The part of a note a statement of its page was read from, if it was read from one.
notePart :: RawStatementTree RawExpr () -> Maybe NotePart
notePart (RawStatementTree (RawAnnot annot) [])
  | named mdAnnot annot = NoteWriting <$> rawArgStr mdAnnotText annot
  | named notePropertiesAnnot annot = NoteFrontmatter <$> rawArgStr notePropertiesAnnotText annot
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
    apartFrom NoteWriting{} _     = True
    apartFrom _ NoteWriting{}     = True
    -- The properties are a block of their own and the note begins after them, which is how
    -- Obsidian writes one: a blank line follows them whether the note goes on in writing or in a
    -- checkbox.
    apartFrom _ NoteFrontmatter{} = True
    apartFrom _ _                 = False

-- | The lines one part of a note is written as: its writing as it was found, or its checkbox
-- followed by whatever is written inside it, indented under it.
writeNotePart :: NotePart -> [String]
writeNotePart (NoteWriting writing) = lines writing
-- The delimiters are written again rather than carried, so a block written between more of them
-- than it needs is written back between two.
writeNotePart (NoteFrontmatter properties) = "---" : lines properties ++ ["---"]
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
  -- | The properties a note opens with, as the lines they are written between the @---@ as. The
  -- delimiters are the shape of the block rather than anything it says, so they are written again
  -- rather than carried; everything between them is carried exactly as it was found, because the
  -- one place that has to give the block back as it was is the one writing the note back.
  | NoteFrontmatter String
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
--
-- The properties a note opens with are broken off first and only here, not in 'noteLineParts': a
-- note opens with them at most once, and a @---@ written anywhere else — under a checkbox included
-- — is a rule of the writing rather than the edge of a block of properties.
noteParts :: String -> [NotePart]
noteParts contents = case splitFrontmatter (lines contents) of
  Just (properties, rest) -> NoteFrontmatter (intercalate "\n" properties) : noteLineParts rest
  Nothing                 -> noteLineParts (lines contents)

-- | The properties a note opens with and the note written after them, if it opens with a block of
-- them at all.
--
-- Only at the very start, and only closed: that is where Obsidian writes a block of properties and
-- how it recognises one, so a note opening with a rule it never closes is a note whose first line
-- is a rule rather than a note which is all properties.
splitFrontmatter :: [String] -> Maybe ([String], [String])
splitFrontmatter (l:ls)
  | isPropertiesFence l
  , (properties, _:rest) <- break isPropertiesFence ls = Just (properties, rest)
splitFrontmatter _ = Nothing

-- | Whether a line is the edge of a block of properties.
isPropertiesFence :: String -> Bool
isPropertiesFence = ("---" ==) . dropWhileEnd isSpace

-- | The properties a note names the page it is written over by, in the order they are looked for.
--
-- @url@ is what the property is called when a note is written about a page; @roam_refs@ is what an
-- import from Roam Research leaves behind.
overlayKeys :: [String]
overlayKeys = ["url", "roam_refs"]

-- | The page a note is written over, out of the properties it opens with, if they name one.
--
-- A property naming no page on the web is passed over rather than taken as the answer, so a note
-- whose @url@ is a title and whose @roam_refs@ is an address is written over the address.
frontmatterUrl :: String -> Maybe String
frontmatterUrl properties = listToMaybe $ mapMaybe addressAt overlayKeys
  where
    addressAt key = webAddress . unwrapValue . firstEntry =<< frontmatterValue key properties

-- | The value a block of properties gives a key, if it gives it one.
--
-- Read as the lines it is written as rather than as YAML, because the one thing read out of the
-- block is a value written on the line of its key — and reading only that much is what leaves the
-- block a piece of text to be written back exactly as it was found. The key is read where it
-- starts its line, as a key indented under another is a part of that one rather than a property of
-- the note.
frontmatterValue :: String -> String -> Maybe String
frontmatterValue key properties = listToMaybe [trim v | l <- lines properties, Just v <- [stripPrefix (key ++ ":") l]]

-- | What a value writes between the bracket it opens with and the bracket closing it, if it opens
-- with one which closes at its end.
--
-- A list is written between brackets and so is a wiki link, and either may hold the other. What is
-- not written in a pair at all is neither: @[a] and [b]@ opens and ends with a bracket without the
-- first one being closed by the last.
bracketedBody :: String -> Maybe String
bracketedBody ('[':rest) = aux (1 :: Int) "" rest
  where
    aux _ _ []       = Nothing
    aux 1 acc "]"    = Just (reverse acc)
    aux 1 _ (']':_)  = Nothing -- The bracket it opened with closes before its end
    aux d acc (c:cs) = aux (depth c d) (c:acc) cs
    depth '[' d = d + 1
    depth ']' d = d - 1
    depth _ d   = d
bracketedBody _ = Nothing

-- | The first of the entries a value writes, which for a value written on its own is that value.
--
-- The entries of a list are written apart by commas, and are read at the depth the brackets put
-- them at rather than at every comma: a wiki link written as an entry is written between brackets
-- of its own, and a comma inside one is part of what that entry names.
firstEntry :: String -> String
firstEntry v = case bracketedBody (trim v) of
  Just entries -> trim $ untilEntryEnds entries
  Nothing      -> trim v
  where
    untilEntryEnds = aux (0 :: Int)
      where
        aux _ []      = []
        aux 0 (',':_) = []
        aux d (c:cs)  = c : aux (depth c d) cs
        depth '[' d = d + 1
        depth ']' d = d - 1
        depth _ d   = d

-- | A value as it was meant rather than as it was written: without the quotes it was written in and
-- without the brackets it was written between.
--
-- A wiki link is written between two brackets, and a value read out of a list may still be written
-- between its own, so each pair is taken off in turn.
unwrapValue :: String -> String
unwrapValue = unwrapBrackets . unquote . trim
  where
    unquote s@(c:_) | c `elem` ['"', '\''], length s > 1, [c] `isSuffixOf` s = init (tail s)
    unquote s = s
    unwrapBrackets s = maybe s unwrapBrackets (bracketedBody s)

-- | Whether what a property names is a page on the web, which is the one thing a note can be
-- written over. A property naming another note, or naming a title, names nothing there is to show
-- beside the writing.
webAddress :: String -> Maybe String
webAddress v = if "://" `isInfixOf` v then Just v else Nothing

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
-- This is the reading a 'Syntax.Ct.Prgm.ReadTree' of the vault stands for, so it is the read's own
-- walk of the tree ('Syntax.Ct.Prgm.scanTree') rather than a walk of its own — which is what has a
-- vault walked once by a read rather than once by the vault page and once again by every note in
-- it, and what makes what this reads and what a tree read is timed by the same thing rather than
-- two things which have to agree.
--
-- Markdown alone, as a vault holds whatever else its owner keeps in it and only a note is a page.
vaultNotes :: ObsidianReader -> FilePath -> IO [FilePath]
vaultNotes reader dir = filter (".md" `isSuffixOf`) . tsFiles <$> scanTree (orEnv reader) dir

-- | Where the notes of a vault are looked up by what a link may name them by, indexing them if this
-- read has not.
--
-- Kept for the read the same way and for the same reason as the walk it is built out of
-- ('Syntax.Ct.Prgm.scanTree'): every note of a vault resolves its links against the same notes, so
-- a vault indexed for each of them would be indexed once for every note in it.
vaultIndex :: ObsidianReader -> FilePath -> IO VaultIndex
vaultIndex reader vault = do
  found <- readIORef (orIndexes reader)
  case H.lookup vault found of
    Just index -> return index
    Nothing -> do
      index <- mkVaultIndex vault <$> vaultNotes reader vault
      modifyIORef' (orIndexes reader) $ H.insert vault index
      return index

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

-- | Where a link written in a note finds the note it names.
--
-- Every note of the vault under the path it sits at within the vault, and every note under its name
-- alone, each written as a link may write it: without the markdown extension, since a link may
-- write it or leave it out. Two of them rather than one, because a link naming a path is a
-- different thing from a link naming a name, and the first is asked before the second.
--
-- Built once for a vault by the read that reads it, and that is the whole reason it exists. A link
-- resolved by walking the notes costs the size of the vault, so a vault of a thousand notes writing
-- ten links each walks ten million notes — and what the walking does at each of them is
-- 'makeRelative' over a path, which is not a comparison ten million of go unnoticed. Here it is one
-- 'makeRelative' for each note, once, and a lookup for each link.
data VaultIndex = VaultIndex {
  viByPath :: H.HashMap FilePath FilePath,
  viByName :: H.HashMap FilePath FilePath
                             }

-- | That index, out of the notes a walk of the vault found, in the order it walked them.
--
-- Where two notes are written at the one key the first the walk found wins, which is the note a
-- reading that took the first match while walking them in order already answered with. The walk is
-- sorted ('Syntax.Ct.Prgm.walkTree'), so which of them that is, is a property of the vault rather
-- than of the order a filesystem happened to answer a listing in.
mkVaultIndex :: FilePath -> [FilePath] -> VaultIndex
mkVaultIndex vault notes = VaultIndex
  (firstWins [(dropMdExtension (makeRelative vault note), note) | note <- notes])
  (firstWins [(takeBaseName note, note) | note <- notes])
  where
    -- 'H.fromListWith' reads the list from the front and gives its function the value being added
    -- and the value already held, so keeping the one already held keeps the note found first.
    firstWins :: [(FilePath, FilePath)] -> H.HashMap FilePath FilePath
    firstWins = H.fromListWith (\_new old -> old)

-- | The note of a vault a wiki link points at, if the vault holds one.
--
-- A link names a note either by its path within the vault or by its name alone, with or without the
-- markdown extension, and may go on to name a place within it and the words to show it as. A link
-- naming no note at all is not an error: writing one is how a note yet to be written is planned,
-- and Obsidian shows it as unresolved rather than refusing it.
resolveWikiLink :: VaultIndex -> String -> Maybe FilePath
resolveWikiLink VaultIndex{viByPath, viByName} link = case wikiLinkTarget link of
  ""     -> Nothing -- A link naming only a place is within the note that wrote it
  target -> H.lookup (dropMdExtension target) viByPath <|> H.lookup (dropMdExtension target) viByName

-- | A path without the markdown extension, where it is written with one.
--
-- That extension alone, and not whatever extension a name happens to end with: a note called
-- @Version 1.2@ is a note whose name ends in @.2@, and dropping that would file it under a name no
-- link writes.
dropMdExtension :: FilePath -> FilePath
dropMdExtension p = if ".md" `isSuffixOf` p then take (length p - 3) p else p

-- | The note a wiki link names, without the place within it or the words it is shown as.
wikiLinkTarget :: String -> String
wikiLinkTarget = trim . takeWhile (/= '#') . takeWhile (/= '|')

-- | Text without the whitespace written around it.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
