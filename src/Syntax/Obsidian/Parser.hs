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

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM_, guard)
import           CtConstants
import           Data.Aeson               (object, (.=))
import qualified Data.Aeson               as A
import           Data.Char                (isDigit, isSpace)
import           Data.Foldable            (toList)
import qualified Data.HashMap.Strict      as H
import qualified Data.IntSet              as IS
import           Data.IORef               (IORef, modifyIORef', newIORef,
                                           readIORef)
import           Data.List                (dropWhileEnd, intercalate, isInfixOf,
                                           isPrefixOf, isSuffixOf, nub,
                                           stripPrefix)
import           Data.Maybe               (fromMaybe, isNothing, listToMaybe,
                                           mapMaybe, maybeToList)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           Semantics.Prgm
import           Semantics.Types          (Constant (CStr), makeAbsoluteName,
                                           mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Obsidian.Cache    (ObsidianCache, cachedNoteLinks,
                                           flushObsidianCache, keepOnlyNotes,
                                           newObsidianCache, recordNoteLinks)
import           Syntax.Obsidian.Settings (StatusKind (..), TaskSettings,
                                           TaskStatus (..), readTaskSettings,
                                           statusFor, taskSettingsFiles, tsKind,
                                           tsName)
import           Syntax.Obsidian.Task     (ObsidianTask (..), TaskPriority (..),
                                           obsidianTask)
import           System.Directory
import           System.FilePath          (makeRelative, takeBaseName,
                                           takeDirectory, (</>))
import qualified Text.MMark               as MMark
import           Text.MMark.Extension     (Block (..), Bni, Inline (..),
                                           scanner)
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
obsidianSyntax = ImportSyntax newObsidianReader unparseNote noteLinkDir

-- | Where a relative link written in a note resolves: the vault holding it.
--
-- A note names another note of its vault by that note's path within the vault, which is how Obsidian
-- names one and is what 'wikiHref' writes a link as — so a link written in a note resolves against
-- the vault rather than against the folder the note sits in. That is the whole of what the rest of
-- the compiler has to know about a vault to show a note's links, and it is answered here so that
-- nothing outside this module has to know what a vault is.
--
-- A page under no vault is answered with the directory it was imported relative to, as any other
-- page is. That is a vault's own page, which is not a note of anything and has no writing to hold a
-- link.
noteLinkDir :: FileImport -> IO (Maybe FilePath)
noteLinkDir imp = case impDisp imp of
  Nothing   -> return $ impDir imp
  Just page -> (<|> impDir imp) <$> noteVault page

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
  -- | What each vault this read has reached says about the Obsidian Tasks plugin, and nothing where
  -- it does not run it.
  --
  -- Per read and per vault for the reason 'orIndexes' is: every note of a vault is read by the same
  -- settings, so a vault whose settings were read for each of them would read one small file once
  -- for every note in it.
  orTasks   :: IORef (H.HashMap FilePath (Maybe TaskSettings)),
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
  tasks <- newIORef H.empty
  cache <- traverse newObsidianCache (reCacheRoot env)
  let reader = ObsidianReader env indexes tasks cache
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
  settings <- vaultTaskSettings reader vault
  let linked = nub $ filter (/= note) $ mapMaybe (resolveWikiLink index) $ wikiLinks note contents
  forM_ (orCache reader) $ \cache -> recordNoteLinks cache vault note linked
  return $ ParsedImport (RawPrgm (noteImports linked) [notePageStatement settings (wikiHref vault index) (takeBaseName note) contents]) (Just contents) (Just $ noteReads vault note linked)

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
    Just linked -> return $ PreloadImport (noteImports linked) [] Nothing (Just $ noteReads vault note linked)
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
-- The vault's own Tasks settings are read as well, whether or not they are there. They decide what
-- every checkbox of the note is read as, so a note read before the plugin was turned on is a note
-- read wrongly afterwards; naming them here is what has the note read again when they change. They
-- are named as files rather than reached through the vault because a reading of a tree walks past a
-- dotted entry, and Obsidian keeps them in one.
--
-- NOT read from the vault, though the whole vault is what the links resolved against. A note added
-- anywhere under the vault could turn a link that resolved to nothing into an import, and saying so
-- would be every note of a vault read again for every note added to it — the cost of a change being
-- the size of the vault rather than the size of the change. So a link that resolved to nothing is
-- left resolving to nothing until the note writing it is read again for a reason of its own. A link
-- that did resolve is watched through the note it resolved to, so deleting, moving or renaming that
-- note is seen.
noteReads :: FilePath -> FilePath -> [FilePath] -> [ReadPath]
noteReads vault note linked = ReadFile note : map ReadFile (taskSettingsFiles vault) ++ map ReadExists linked

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
--
-- Which of its two readings a checkbox gets is decided here, out of what the vault holding the note
-- is set to, and nowhere else. That is why the settings are carried all the way down here rather
-- than looked at where a line is broken off: breaking a note into its parts is markdown's business
-- and is the same in every vault, where what a checkbox MEANS is the vault's.
--
-- Where a wiki link points is carried down the same way and for the same reason. Which note a link
-- names is the vault's to say, and it is said once for the vault ('wikiHref') rather than worked out
-- again wherever a link happens to be written.
notePageStatement :: Maybe TaskSettings -> (String -> Maybe String) -> String -> String -> RawStatementTree RawExpr ()
notePageStatement settings href title contents = RawStatementTree (RawDeclStatement page) (concatMap (partStatements settings href) $ noteParts contents)
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
partStatements :: Maybe TaskSettings -> (String -> Maybe String) -> NotePart -> [RawStatementTree RawExpr ()]
partStatements settings href part@(NoteFrontmatter properties) = partStatement settings href part : map overlayStatement (maybeToList $ frontmatterUrl properties)
partStatements settings href part = [partStatement settings href part]

-- | The statement one part of a note is declared by.
--
-- A checkbox is read as the reading its vault gives it. A vault running the Obsidian Tasks plugin
-- gives it 'CtConstants.noteObsidianTaskPath', with what the plugin wrote into the line read out of
-- it; every other vault gives it 'CtConstants.noteSimpleTaskPath', which is the box and what it
-- says. A vault which runs the plugin and asks that every task carry a word gives a line not
-- carrying that word the plain reading too — such a line is a checkbox of the note rather than a
-- task the plugin manages, and it is still a checkbox.
--
-- The wiki links of a note are written as the links they are here, where a run of its writing is
-- read as the comment holding it, and only here. A run of writing is a piece of markdown of its own
-- — 'noteParts' has already broken the note at its checkboxes and carried the state of a fence
-- across the breaks — so it is markdown that can be read for its links by itself. What is left out
-- by that falls out rather than being said: a link written on a checkbox line is not in any run of
-- writing, and a task carries the line it was written as, so it is left as it was written.
partStatement :: Maybe TaskSettings -> (String -> Maybe String) -> NotePart -> RawStatementTree RawExpr ()
partStatement _ href (NoteWriting writing) = RawStatementTree (RawAnnot (rawComment (embedWikiLinks href writing))) []
partStatement _ _ (NoteFrontmatter properties) = RawStatementTree (RawAnnot (rawProperties properties)) []
partStatement settings href (NoteTask symbol says under) = RawStatementTree (RawDeclStatement task) (map (partStatement settings href) under)
  where
    task = RawObjArr (Just taskExpr) TypeObj Nothing [] Nothing Nothing
    taskExpr = case settings of
      Just settings' | Just read' <- obsidianTask settings' says -> obsidianTaskExpr (statusFor settings' symbol) symbol read'
      _ -> simpleTaskExpr

    simpleTaskExpr = rawVal noteSimpleTaskPath `applyRawArgs` [
      (Just $ partialKey (mkName noteTaskContents), rawStr (trim says)),
      (Just $ partialKey (mkName noteTaskState), rawVal (if symbol `elem` ["x", "X"] then truePrim else falsePrim))
                                                              ]

-- | A checkbox of a vault running the Obsidian Tasks plugin, as the value it is declared as.
--
-- The line is carried whole in 'CtConstants.noteTaskContents' and the few fields worth naming are
-- named beside it; everything else the line said travels in 'CtConstants.noteTaskRest' as JSON. The
-- type's own comment in @stack\/core\/note.ct@ says why it is split that way and what would undo the
-- split.
obsidianTaskExpr :: TaskStatus -> String -> ObsidianTask -> RawExpr ()
obsidianTaskExpr status symbol task = rawVal noteObsidianTaskPath `applyRawArgs` [
  (Just $ partialKey (mkName noteTaskContents), rawStr (otContents task)),
  (Just $ partialKey (mkName noteTaskSymbol), rawStr symbol),
  (Just $ partialKey (mkName noteTaskStatusType), rawVal (statusTypeMember (tsKind status))),
  (Just $ partialKey (mkName noteTaskDescription), rawStr (otDescription task)),
  (Just $ partialKey (mkName noteTaskPriority), rawVal (priorityMember (otPriority task))),
  (Just $ partialKey (mkName noteTaskScheduled), rawMaybeStr (otScheduled task)),
  (Just $ partialKey (mkName noteTaskDue), rawMaybeStr (otDue task)),
  (Just $ partialKey (mkName noteTaskRecurrence), rawMaybeStr (otRecurrence task)),
  (Just $ partialKey (mkName noteTaskRest), rawStr (taskRestJson status task))
                                                                                 ]

-- | A value which may be absent, as the arm of a @\/Data\/Maybe@ it is.
rawMaybeStr :: Maybe String -> RawExpr ()
rawMaybeStr Nothing = rawVal ctMaybeNothing
rawMaybeStr (Just v) = rawVal ctMaybeSome `applyRawArgs` [(Just $ partialKey (mkName ctMaybeSomeValue), rawStr v)]

-- | The member of @\/Data\/Note\/StatusType@ a kind of status is.
statusTypeMember :: StatusKind -> String
statusTypeMember KTodo       = noteStatusTypePath ++ "/Todo"
statusTypeMember KDone       = noteStatusTypePath ++ "/Done"
statusTypeMember KInProgress = noteStatusTypePath ++ "/InProgress"
statusTypeMember KOnHold     = noteStatusTypePath ++ "/OnHold"
statusTypeMember KCancelled  = noteStatusTypePath ++ "/Cancelled"
statusTypeMember KNonTask    = noteStatusTypePath ++ "/NonTask"
statusTypeMember KEmpty      = noteStatusTypePath ++ "/Empty"

-- | The member of @\/Data\/Note\/Priority@ a priority is.
priorityMember :: TaskPriority -> String
priorityMember PHighest = notePriorityPath ++ "/Highest"
priorityMember PHigh    = notePriorityPath ++ "/High"
priorityMember PMedium  = notePriorityPath ++ "/Medium"
priorityMember PNone    = notePriorityPath ++ "/None"
priorityMember PLow     = notePriorityPath ++ "/Low"
priorityMember PLowest  = notePriorityPath ++ "/Lowest"

-- | Everything a task said that is not a field of its own, as JSON.
--
-- Only what the task carries is written, so a plain checkbox's is @{}@ rather than a row of nulls.
-- The done and the cancelled date are the one entry: a task is done or it is cancelled and never
-- both, and the status says which of the two a task with such a date is.
taskRestJson :: TaskStatus -> ObsidianTask -> String
taskRestJson status task = TL.unpack $ TLE.decodeUtf8 $ A.encode $ object $ ("statusName" .= tsName status) : concat [
  ["tags" .= otTags task | not (null (otTags task))],
  ["dependsOn" .= otDependsOn task | not (null (otDependsOn task))],
  ["created" .= v | Just v <- [otCreated task]],
  ["start" .= v | Just v <- [otStart task]],
  ["statusDate" .= v | Just v <- [otDone task <|> otCancelled task]],
  ["onCompletion" .= v | Just v <- [otOnCompletion task]],
  ["id" .= v | Just v <- [otTaskId task]],
  ["blockLink" .= v | Just v <- [otBlockLink task]]
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
--
-- The links of a run of writing are written back as the wiki links they were read from, which is
-- the reversion of what 'partStatement' does to them. It is done from the writing alone, with
-- neither the note nor its vault to hand, and 'wikiHref' writes an address that can be read that
-- way for exactly that reason.
notePart :: RawStatementTree RawExpr () -> Maybe NotePart
notePart (RawStatementTree (RawAnnot annot) [])
  | named mdAnnot annot = NoteWriting . restoreWikiLinks <$> rawArgStr mdAnnotText annot
  | named notePropertiesAnnot annot = NoteFrontmatter <$> rawArgStr notePropertiesAnnotText annot
notePart (RawStatementTree (RawDeclStatement task) under)
  | Just taskExpr <- roaObj task, named noteSimpleTaskPath taskExpr = do
      says <- rawArgStr noteTaskContents taskExpr
      done <- rawArgState taskExpr
      NoteTask (if done then "x" else " ") says <$> mapM notePart under
  -- A task the plugin manages is written back from the two things it was read with and from none of
  -- the readings beside them: the character in its box, and the line as it was written. That is what
  -- has a line carrying something this cannot yet read written back carrying it, and it is why
  -- reading a line apart never has to be undone.
  | Just taskExpr <- roaObj task, named noteObsidianTaskPath taskExpr = do
      says <- rawArgStr noteTaskContents taskExpr
      symbol <- rawArgStr noteTaskSymbol taskExpr
      NoteTask symbol says <$> mapM notePart under
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
writeNotePart (NoteTask symbol says under) = dropWhileEnd isSpace (box ++ says) : map indentLine (writeNoteParts under)
  where
    box = "- [" ++ symbol ++ "] "
    -- A blank line is written with nothing on it rather than with the indentation of the block it
    -- sits in, so that writing a note back does not leave trailing spaces through it.
    indentLine l = if isBlank l then l else "  " ++ l

-- | What a note is written as: the writing of it, and the tasks that break the writing.
data NotePart
  = NoteWriting String
  -- | The character written in the box, what the checkbox says after it, and what is written under
  -- it — which is the note again: the writing under the checkbox, and the checkboxes indented under
  -- it.
  --
  -- The character rather than whether it is checked, because which characters mean what is the
  -- vault's to say: a vault running the Obsidian Tasks plugin may write @[\/]@ for a task begun and
  -- @[-]@ for one dropped, and markdown knows neither. What the character means is worked out where
  -- the vault's settings are known ('partStatement'); what is written here is what was written.
  | NoteTask String String [NotePart]
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
      | not inCode, Just (symbol, says) <- taskLine l = let (under, inCode', ls') = taskWriting (indentation l) inCode ls in
          writingPart acc ++ (NoteTask symbol says (noteLineParts under) : go inCode' [] ls')
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

-- | The character in a checkbox's box and what the line says after it, if the line is a checkbox at
-- all.
--
-- A checkbox is a list item whose writing begins with a box: @- [ ] a task@, @1. [x] a task done@.
-- Obsidian writes a status of its own within the box as any one character, such as @[\/]@ for a task
-- begun, so any character is a box and the character is answered rather than read here.
--
-- Exactly one space is dropped after the box, and nothing is dropped from the end. What follows the
-- box is what the note wrote there, and a note running the Tasks plugin is written back from it
-- unchanged — so a line written with two spaces after its box keeps the second one. The space at the
-- end is dropped where the line is written back ('writeNotePart') rather than here, since trailing
-- space is not something a note means to say.
taskLine :: String -> Maybe (String, String)
taskLine line = do
  afterMarker <- listMarker $ dropWhile isSpace line
  case afterMarker of
    ('[':box:']':rest) | box /= ']', null rest || isSpace (head rest) -> Just ([box], drop 1 rest)
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

-- | What a vault says about the Obsidian Tasks plugin, reading it if this read has not.
--
-- Kept for the read the same way and for the same reason as 'vaultIndex': every note of a vault is
-- read by the same settings, so a vault asked for each of them would read one small file once for
-- every note in it.
vaultTaskSettings :: ObsidianReader -> FilePath -> IO (Maybe TaskSettings)
vaultTaskSettings reader vault = do
  found <- readIORef (orTasks reader)
  case H.lookup vault found of
    Just settings -> return settings
    Nothing -> do
      settings <- readTaskSettings vault
      modifyIORef' (orTasks reader) $ H.insert vault settings
      return settings

-- | The wiki links a note writes, in the order it writes them.
--
-- A note whose markdown cannot be parsed has no links. It is still a page, holding what was written
-- in it, so the vault it belongs to still reads. Reporting the parse failure rather than passing
-- over it needs an import parser that can answer with notes, which is the TODO on
-- 'Syntax.Parsers.readImport'.
wikiLinks :: FilePath -> String -> [String]
wikiLinks note contents = [wikiLinkText found | (i, found) <- zip [0 ..] (occurrences wikiLinkAt contents), i `IS.member` prose]
  where
    prose = proseOccurrences wikiLinkAt note contents

-- | The writing of a note with each wiki link naming a note of its vault written as the markdown
-- link to the page that note is read as.
--
-- Only the links a reader reads are written. A wiki link inside code is writing about a link rather
-- than making one, and one naming no note of the vault is a note yet to be written, which Obsidian
-- shows as unresolved rather than as a link. Both are left exactly as they were written, and so are
-- written back as themselves.
--
-- An embed @![[note]]@ is left alone as well. It shows the note it names within the note writing it,
-- which a link cannot do, so writing it as one would have the note say something else the next time
-- it was saved. It is still a link for the import it makes.
embedWikiLinks :: (String -> Maybe String) -> String -> String
embedWikiLinks href writing = rewriteOccurrences wikiLinkAt written writing
  where
    prose = proseOccurrences wikiLinkAt noteSource writing
    written i found = do
      guard $ i `IS.member` prose && not (wikiLinkEmbed found)
      address <- href $ wikiLinkText found
      return $ printf "[%s](%s)" (escapeShown $ wikiLinkShown $ wikiLinkText found) address

-- | The writing of a note with each markdown link naming another note of its vault written back as
-- the wiki link it was read from: the reversion of 'embedWikiLinks'.
--
-- A link naming a markdown file by a path of its own is such a link, because that is what
-- 'wikiHref' writes it as. So a markdown link written by hand to a note of the vault is written back
-- as a wiki link, which says the same thing in the way the vault says it.
--
-- The words the link is shown as are written as the wiki link's alias, and left out where they are
-- already what the link names — so a link a note wrote naming a note of the vault by that vault's
-- path for it is written back exactly as it was written.
restoreWikiLinks :: String -> String
restoreWikiLinks writing = rewriteOccurrences mdLinkAt written writing
  where
    prose = proseOccurrences mdLinkAt noteSource writing
    written i found = do
      guard $ i `IS.member` prose
      (shown, address, _) <- mdLink found
      target <- wikiTarget address
      let shown' = unescapeShown shown
      return $ printf "[[%s]]" (if shown' == target then target else target ++ "|" ++ shown')

-- | The name the writing of a note is reported by where its markdown cannot be parsed.
--
-- Nothing is reported: writing whose markdown cannot be parsed is writing that holds no links, as
-- 'wikiLinks' says of a whole note. What is named here is therefore never shown, and the writing is
-- a part of a note rather than a file of its own to name.
noteSource :: FilePath
noteSource = "<note>"

-- | Every occurrence of a token written in a note, in the order they are written.
occurrences :: (String -> Maybe (String, String)) -> String -> [String]
occurrences at = aux
  where
    aux [] = []
    aux s@(_:cs) = case at s of
      Just (found, rest) -> found : aux rest
      Nothing            -> aux cs

-- | The note with each occurrence of a token written as whatever it is to be written as, given which
-- occurrence it is and what was written there. An occurrence answered nothing for is left exactly as
-- it was written.
rewriteOccurrences :: (String -> Maybe (String, String)) -> (Int -> String -> Maybe String) -> String -> String
rewriteOccurrences at write = aux 0
  where
    aux _ [] = []
    aux i s@(c:cs) = case at s of
      Just (found, rest) -> fromMaybe found (write i found) ++ aux (i + 1) rest
      Nothing            -> c : aux i cs

-- | Which occurrences of a token written in a note are prose of it rather than code in it.
--
-- Neither token this is asked about is markdown the parser will hand back. A @[[wiki link]]@ is a
-- reference to a definition that a note never gives, which is an error rather than a link, and a
-- markdown link is handed back without the address it points at ('inlineText'). So each occurrence
-- is taken out of the note and replaced by a placeholder before the note is parsed, and the ones
-- that survive into the parsed note's prose are the ones a reader reads. What markdown is being
-- parsed for is which of them are prose at all: a token written inside code is writing about one
-- rather than being one, and 'blockText' leaves code out.
--
-- Whatever wiki links are left after that are taken out as well, without being counted. A wiki link
-- is what breaks the parse in the first place, and the note being asked about markdown links still
-- holds every wiki link that named no note of the vault — so a note planning one note yet to be
-- written would be a note none of whose links could be read at all.
--
-- The boxes are taken out after the tokens rather than before them, so that a box written inside a
-- token is not read as a checkbox of the note.
proseOccurrences :: (String -> Maybe (String, String)) -> FilePath -> String -> IS.IntSet
proseOccurrences at note contents = case MMark.parse note (T.pack $ unboxed masked') of
  Left _    -> IS.empty
  Right doc -> IS.fromList $ maskedIndices $ intercalate "\n" $ map blockText $ reverse $ MMark.runScanner doc (scanner [] (flip (:)))
  where
    masked = rewriteOccurrences at (\i _ -> Just (maskOpen : show i ++ [maskClose])) contents
    masked' = rewriteOccurrences wikiLinkAt (\_ _ -> Just [maskPlain]) masked

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
      Just (_, says) -> takeWhile isSpace l ++ "- " ++ says
      Nothing        -> l

-- | Reads the occurrences back out of the placeholders 'proseOccurrences' left, in the order they
-- are found.
maskedIndices :: String -> [Int]
maskedIndices s = case dropWhile (/= maskOpen) s of
  []       -> []
  (_:rest) -> case span isDigit rest of
    (i, c:rest') | c == maskClose, not (null i) -> read i : maskedIndices rest'
    _                                           -> maskedIndices rest

-- | The characters a masked occurrence is written between, and the one a masked occurrence that is
-- not being counted is written as.
--
-- From the private use area, which no markdown syntax gives a meaning to, so that a wiki link
-- holding a @|@, a @*@ or a @#@ is neither read as markdown nor has to be escaped as though it were.
maskOpen, maskClose, maskPlain :: Char
maskOpen = '\xE000'
maskClose = '\xE001'
maskPlain = '\xE002'

-- | A wiki link written at the start of a string, as the whole of what was written and what follows
-- it.
--
-- An embed @![[note]]@ shows another note within this one, which is a link to it.
wikiLinkAt :: String -> Maybe (String, String)
wikiLinkAt s = case s of
  ('!':'[':'[':rest) -> closed "![[" rest
  ('[':'[':rest)     -> closed "[[" rest
  _                  -> Nothing
  where
    closed opened rest = do
      (link, after) <- splitOnFirstInLine "]]" rest
      return (opened ++ link ++ "]]", after)

-- | What a wiki link names, out of the whole of what was written: the text between the brackets.
wikiLinkText :: String -> String
wikiLinkText found = take (length inner - 4) $ drop 2 inner
  where inner = fromMaybe found $ stripPrefix "!" found

-- | Whether a wiki link is an embed, which shows the note it names within the note writing it.
wikiLinkEmbed :: String -> Bool
wikiLinkEmbed = isPrefixOf "!"

-- | The words a markdown link written at the start of a string is shown as, the address it points
-- at, and what follows it.
--
-- The words are read to the first bracket which is not escaped, which is the bracket 'escapeShown'
-- escapes so that they can be.
mdLink :: String -> Maybe (String, String, String)
mdLink ('[':rest) = do
  (shown, afterShown) <- untilClosed "" rest
  afterOpened <- stripPrefix "(" afterShown
  (address, after) <- splitOnFirstInLine ")" afterOpened
  return (shown, address, after)
  where
    untilClosed _ []         = Nothing
    untilClosed _ ('\n':_)   = Nothing
    untilClosed acc ('\\':c:cs) | c `elem` escapedShown = untilClosed (c : '\\' : acc) cs
    untilClosed acc (']':cs) = Just (reverse acc, cs)
    untilClosed acc (c:cs)   = untilClosed (c:acc) cs
mdLink _ = Nothing

-- | A markdown link written at the start of a string, as the whole of what was written and what
-- follows it.
mdLinkAt :: String -> Maybe (String, String)
mdLinkAt s = do
  (shown, address, after) <- mdLink s
  return (printf "[%s](%s)" shown address, after)

-- | Splits at the first occurrence of a separator written on the same line, if there is one.
--
-- On the same line because everything read this way is written on one: a wiki link, and the words
-- and the address of a markdown link. Stopping there is also what keeps a bracket which opens
-- nothing from being read to the end of the note, so a note full of brackets costs its length rather
-- than the square of it.
splitOnFirstInLine :: String -> String -> Maybe (String, String)
splitOnFirstInLine sep = aux ""
  where
    aux _ []       = Nothing
    aux _ ('\n':_) = Nothing
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

-- | The place within a note a wiki link names, and nothing where it names the note alone.
wikiLinkAnchor :: String -> String
wikiLinkAnchor = trim . drop 1 . dropWhile (/= '#') . takeWhile (/= '|')

-- | The words a wiki link is shown as: the ones written after the @|@, and what it names where none
-- are written. This is what Obsidian shows a link as.
wikiLinkShown :: String -> String
wikiLinkShown link = trim $ maybe link snd $ splitOnFirstInLine "|" link

-- | The address a wiki link is written as, where it names a note of the vault: the path of that note
-- within the vault, and the place within it where it names one.
--
-- Within the vault rather than within the note writing it, because writing a note back has only that
-- note's own writing to work from — it knows neither which note it is nor which vault holds it
-- ('unparseNote') — and a path from the vault is the one thing that can be read back as a wiki link
-- without knowing either. It is also the very path the vault is indexed by ('mkVaultIndex'), so the
-- address a link is written as is the name it resolved by.
wikiHref :: FilePath -> VaultIndex -> String -> Maybe String
wikiHref vault index link = do
  target <- resolveWikiLink index link
  return $ escapeHref (makeRelative vault target) ++ anchor
  where
    anchor = case wikiLinkAnchor link of
      ""    -> ""
      place -> '#' : escapeHref place

-- | The note and the place within it a markdown address names, written as a wiki link names them,
-- where it names a note of the vault at all.
--
-- A markdown file named by a path of its own is such an address, because that is what 'wikiHref'
-- writes. An address on the web, and one naming a file from the root of the filesystem rather than
-- from the vault, name something which is not a note of the vault and are left alone.
wikiTarget :: String -> Maybe String
wikiTarget address = do
  guard $ isNothing $ webAddress address
  let (named', anchor) = break (== '#') address
  let path' = unescapeHref named'
  guard $ ".md" `isSuffixOf` path' && not ("/" `isPrefixOf` path')
  let target = dropMdExtension path'
  guard $ not $ null target
  return $ target ++ if null anchor then "" else '#' : unescapeHref (drop 1 anchor)

-- | A path written as the address of a markdown link, with the characters which would end that
-- address, or be read as the place within it, written as the escapes markdown reads them back from.
--
-- These are the escapes Obsidian itself writes when it writes a link as a markdown one, so a note
-- naming a note whose name holds a space is written the way that vault's own editor writes it.
escapeHref :: String -> String
escapeHref = concatMap aux
  where
    aux c | c `elem` (" ()<>#%" :: String) = printf "%%%02X" (fromEnum c)
          | otherwise                      = [c]

-- | The path an escaped address names.
unescapeHref :: String -> String
unescapeHref ('%':a:b:rest) | Just c <- hexPair a b = c : unescapeHref rest
unescapeHref (c:rest)                               = c : unescapeHref rest
unescapeHref []                                     = []

-- | The character a pair of hexadecimal digits names, where the two of them name one.
hexPair :: Char -> Char -> Maybe Char
hexPair a b = do
  a' <- digit a
  b' <- digit b
  return $ toEnum $ a' * 16 + b'
  where
    digit c | isDigit c                     = Just $ fromEnum c - fromEnum '0'
            | c `elem` ("abcdef" :: String) = Just $ fromEnum c - fromEnum 'a' + 10
            | c `elem` ("ABCDEF" :: String) = Just $ fromEnum c - fromEnum 'A' + 10
            | otherwise                     = Nothing

-- | Text written as the words a markdown link is shown as, with the brackets which would end them
-- escaped.
escapeShown :: String -> String
escapeShown = concatMap (\c -> if c `elem` escapedShown then ['\\', c] else [c])

-- | The words an escaped run of them says.
unescapeShown :: String -> String
unescapeShown ('\\':c:rest) | c `elem` escapedShown = c : unescapeShown rest
unescapeShown (c:rest)                              = c : unescapeShown rest
unescapeShown []                                    = []

-- | The characters written after a backslash within the words a markdown link is shown as: the
-- brackets which would end them, and the backslash which escapes those.
escapedShown :: String
escapedShown = "[]\\"

-- | Text without the whitespace written around it.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
