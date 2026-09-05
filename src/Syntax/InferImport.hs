--------------------------------------------------------------------
-- |
-- Module    :  Syntax.InferImport
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module supports the string inferred import style.
--------------------------------------------------------------------

module Syntax.InferImport where
import           Control.Monad
import           CtConstants            (dirEntryFileNames)
import qualified Data.HashMap.Strict    as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Obsidian.Parser (noteVault, obsidian, obsidianConfigDir)
import           System.Directory
import           System.FilePath        (isAbsolute, joinPath, (</>))
import           Text.Printf

-- | The parser each file extension is read with, before where the file is is taken into account by
-- 'inferFileParser'.
fileExtensionParsers :: H.HashMap String String
fileExtensionParsers = H.fromList [
  ("ct", "ct"),
  -- A @.ctc@ is Catln, read exactly as a @.ct@ is. The extension says what the file is /for/ rather
  -- than what it is written in: it configures the directory holding it (see 'dirParse') rather than
  -- being a program that happens to be kept there.
  ("ctc", "ct"),
  ("ctx", "ctx"),
  ("md", "md"),
  ("hs", "haskell")
                             ]

-- | Imports a path with a named parser, as an import naming its own parser is written.
parserImport :: String -> FilePath -> RawExpr ()
parserImport parser path = rawVal parser `applyRawArgs` [(Nothing, rawStr path)]

-- | Whether a file found in a directory's listing is one anything can read.
--
-- Both tables, and it must be both. 'fileExtensionParsers' is the syntaxes the compiler supplies,
-- and 'ieExtensions' is the ones an importer declared in a @.ct@ file reads — so a @.py@ or a
-- @.json@ is listed because @stack\/importers.ct@ says something reads it, and not because the
-- compiler knows what one is.
--
-- Answering from the first table alone is what this did, and it silently stopped listing @.py@ files
-- when the Python importer's declaration moved out of that table and onto a page.
--
-- Answers with the test rather than with the answer, so that the importers are asked once and the
-- test is then applied to every entry of a directory. Asking per entry is what a directory of a
-- hundred files would cost a hundred times over, and what it costs is not small: 'ieExtensions'
-- parses the registry and every page the registry imports.
isSupportedFileExtension :: ImporterEnv -> IO (String -> Bool)
isSupportedFileExtension importers = do
  declared <- ieExtensions importers
  let exts = H.keys fileExtensionParsers ++ declared
  return $ \fileName -> any ((`isSuffixOf` fileName) . ('.':)) exts

findExistingPath :: [FilePath] -> IO (Maybe FilePath)
findExistingPath [] = return Nothing
findExistingPath (p:ps) = do
  exists <- doesPathExist p
  if exists
    then Just <$> makeAbsolute p
    else findExistingPath ps

-- A directory has no source text of its own to write back — the files in it are pages of their own,
-- each written back by the syntax which read it.
--
-- Its reader is made from the read's 'ReadEnv' rather than being the same reader every time, which
-- is what separates it from every other syntax the compiler supplies. What a directory holds that is
-- worth reading depends on which importers the program declares, and that is a question only the
-- read can answer — see 'isSupportedFileExtension'. The comment on 'Syntax.Ct.Prgm.ReadEnv' names
-- this as the reason the importers are carried there.
dirSyntax :: ImportSyntax
dirSyntax = ImportSyntax dirReader (const Nothing) importedLinkDir Nothing
  where
    dirReader env = return $ readyReader (withDirPath $ dirPreload $ reImporters env) (withDirPath dirParse) (return ())

-- | The directory an import of one names.
withDirPath :: (FilePath -> IO a) -> Expr () -> IO a
withDirPath act imp = case exprAppliedArgs imp of
  [ObjArr{oaArr=Just (Just (CExpr _ (CStr name)), _)}] -> act name
  _                                                    -> undefined

-- | The page of a directory: the program it is entered through, where it holds one.
--
-- Which that is comes from 'dirEntryFileNames', in the order written there: a @dir.ctc@ before a
-- @main.ct@, because a @dir.ctc@ says what the directory is and a @main.ct@ is a program kept in it.
-- A directory holding both is therefore the overlay it declares itself to be, and the @main.ct@
-- beside it is still a page of its own like any other file there.
--
-- The directory alone, because everything read here is read out of its listing: which files it
-- holds, which of those are directories of their own, and which of the entry files it holds. A file
-- in it changing is a change to that file's own page, not to this one.
dirParse :: FilePath -> ImportParseResult
dirParse name = do
  entry <- findExistingFile $ map (name </>) dirEntryFileNames
  let dirPrgm = RawPrgm (map (mkRawFileImport . rawStr) $ maybeToList entry) []
  return $ ParsedImport dirPrgm Nothing (Just [ReadDir name])

-- | The first of these paths which is a file, or nothing where none of them is.
--
-- A file rather than a path, unlike 'findExistingPath', because these are read as programs: a
-- directory named @main.ct@ is not one, and taking it for one would import a directory as the
-- program a directory is entered through.
findExistingFile :: [FilePath] -> IO (Maybe FilePath)
findExistingFile [] = return Nothing
findExistingFile (p:ps) = do
  isFile <- doesFileExist p
  if isFile
    then return (Just p)
    else findExistingFile ps

-- | That page, and every file and directory in it as a page the read walks on to.
dirPreload :: ImporterEnv -> FilePath -> ImportPreloadResult
dirPreload importers name = do
  files <- listDirectory name
  -- Once for the directory, not once per entry: see 'isSupportedFileExtension'.
  supported <- isSupportedFileExtension importers
  files' <- forM files $ \file -> do
    let file' = name ++ "/" ++ file
    isF <- doesFileExist file'
    isD <- doesDirectoryExist file'
    case (isF, isD) of
      (True, False) -> return $ if not ("." `isPrefixOf` file) && supported file'
        then Just file'
        else Nothing
      (False, True) -> return (Just file')
      _ -> error $ printf "Found non-file or directory: %s" file'
  parsed <- dirParse name
  return (eagerPreload parsed){plExtraImports = map (mkRawFileImport . rawStr) $ catMaybes files'}

-- | Where a path written relative to the standard library root is, with a leading @~\/@ in that root
-- expanded against the home directory.
--
-- Answers where the path would be rather than whether anything is there, so that a page of the
-- standard library which is not a file can be named the same way one which is, is
-- ('Syntax.Ct.Prgm.isPagePath').
stackPath :: CTSSConfig -> FilePath -> IO FilePath
stackPath config name = case joinPath [ctssStackPath config, name] of
  '~':'/':p -> do
    homeDir <- getHomeDirectory
    return $ joinPath [homeDir, p]
  p -> return p

-- | The places an import of @name@ could be: beside the file which wrote it, then in the standard
-- library.
importPathCandidates :: CTSSConfig -> Maybe FileImport -> String -> IO [FilePath]
importPathCandidates config caller name = do
  inStack <- stackPath config name
  return $ if isAbsolute name
    then [name]
    else [maybe name (\p -> joinPath [p, name]) (caller >>= impDir), inStack]

-- | The absolute path an import names, or 'Nothing' when it names nothing that is there.
--
-- Not every import names a path: @haskell@ takes a module name, which it resolves itself against
-- the Stack project it belongs to (see 'Syntax.Haskell.Parser.hsSyntax'). So this answers whether
-- an import can be resolved rather than insisting that it must be.
findImportPath :: CTSSConfig -> Maybe FileImport -> String -> IO (Maybe FilePath)
findImportPath config caller name = findExistingPath =<< importPathCandidates config caller name

-- | The absolute path an import names, failing when there is nothing there.
--
-- Used for the inferred @import "name"@ form, which has nothing left to try: it picks its parser
-- from what it finds at the path, so a path with nothing at it names no parser either.
resolveImportPath :: CTSSConfig -> Maybe FileImport -> String -> IO FilePath
resolveImportPath config caller name = do
  paths <- importPathCandidates config caller name
  name' <- findExistingPath paths
  case name' of
    Nothing -> fail $ printf "Could not find file or directory %s. Tried paths %s" name (show paths)
    Just name'' -> return name''

-- | The parser a file is read with: the one its extension names, except that a markdown file within
-- an Obsidian vault is a note of that vault rather than plain markdown.
--
-- A note has to be the same page however it is named — by the vault holding it, by another note
-- linking to it, or by a reader asking the doc server for the path it is displayed at — and a page
-- is identified by the import which names it. So which parser a note is read with cannot be left to
-- whichever of those named it, and is decided here from where the file is instead.
inferFileParser :: FilePath -> IO (Maybe String)
inferFileParser path = case H.lookup ext fileExtensionParsers of
  Just parser | ext == "md" -> do
    vault <- noteVault path
    return $ Just $ if isJust vault then obsidian else parser
  parser -> return parser
  where
    ext = last $ splitOn "." path

-- | The parser a directory is read with: a plain directory of the files in it, except that a
-- directory holding an Obsidian configuration is the vault that makes it.
--
-- The same rule as 'inferFileParser' and for the same reason. A vault has to be the same page
-- however it is named — by whoever started the compiler, or by a reader asking the doc server for
-- the path it is displayed at — and a page is identified by the import which names it. Read as a
-- plain directory it is a second page at the one display path, which is a doc server whose root
-- page cannot be opened and whose navigation holds two entries for one thing.
inferDirParser :: FilePath -> IO String
inferDirParser path = do
  isVault <- doesDirectoryExist $ path </> obsidianConfigDir
  return $ if isVault then obsidian else "dir"

inferRawImportStr :: CTSSConfig -> Maybe FileImport -> String -> IO (RawExpr ())
inferRawImportStr config caller name = do
  inferred <- inferRawImportStrMaybe config caller name
  case inferred of
    Just e -> return e
    Nothing -> do
      name' <- resolveImportPath config caller name
      fail $ printf "Unexpected file extension in import %s" name'

-- | The import a path means, or nothing where no syntax the compiler supplies reads that extension.
--
-- Nothing rather than a failure, because an extension the compiler does not know is not necessarily
-- one nothing reads: an importer declared in a @.ct@ file may read it, and finding that out means
-- reading pages, which is 'Syntax.Parsers.registryImporters' and is above this module. So this
-- answers what it can and leaves the rest to a caller that can look further.
inferRawImportStrMaybe :: CTSSConfig -> Maybe FileImport -> String -> IO (Maybe (RawExpr ()))
inferRawImportStrMaybe config caller name = do
  name' <- resolveImportPath config caller name
  isFile <- doesFileExist name'
  if isFile
    then do
      parser <- inferFileParser name'
      return $ (`parserImport` name') <$> parser
    else do
      parser <- inferDirParser name'
      return $ Just $ parserImport parser name'
