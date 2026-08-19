--------------------------------------------------------------------
-- |
-- Module    :  Syntax.FileDir.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Reads a directory of files as a page declaring what is in it.
--
-- @import fileDirReference("../notes")@ walks that directory and writes a page holding one
-- declaration: the directory, as the @\/Catln\/CatlnResult@ of every file under it. So a program
-- can be handed a copy of a directory that is somewhere else, and is handed it as a value rather
-- than as something to go and read.
--
-- This is what an overlay directory is built on. A directory holding a @dir.ctc@ is entered through
-- that file (see 'Syntax.InferImport.dirParse'), and a @dir.ctc@ whose @main@ is a reference of this
-- kind is a directory standing for another one.
--
-- The directory is read through the import rather than by a function called at evaluation, so that
-- the read is recorded as a read: what a page depended on is what the watcher watches and what a
-- re-read compares against ('fileDirReads'). A function that opened the directory itself would be a
-- copy that never noticed the original changing.
--------------------------------------------------------------------

module Syntax.FileDir.Parser where

import           Control.Exception (IOException, try)
import           CtConstants
import           Data.List         (sort)
import           Data.Maybe        (listToMaybe, mapMaybe)
import           Semantics.Prgm
import           Semantics.Types   (Constant (CStr), mkName, partialKey)
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           System.Directory  (canonicalizePath)
import           System.FilePath   (makeRelative)
import           System.IO         (readFile')
import           Text.Printf

-- | The name an import reads a directory with.
fileDirReference :: String
fileDirReference = "fileDirReference"

-- | What one read reads directories with. It holds the read's 'ReadEnv' so that a walk goes through
-- 'scanTree', which both memoizes the walk and is what times it — a page timed by one walk and built
-- from another goes stale without having changed.
newtype FileDirReader = FileDirReader {
  fdEnv :: ReadEnv
                                      }

-- | Reading a directory as the page of what is in it.
--
-- No reversion: the files of the directory are the source, each of them written back as itself by
-- whatever reads it, so the page assembled from them has no text of its own to be written back.
fileDirSyntax :: ImportSyntax
fileDirSyntax = ImportSyntax newFileDirReader (const Nothing) importedLinkDir

newFileDirReader :: ReadEnv -> IO ImportReader
newFileDirReader env = do
  let reader = FileDirReader env
  return $ ImportReader (fileDirPreload reader) (fileDirParse reader) (return ())

-- | The directory an import names, as the one path that names it.
--
-- Canonical, rather than merely absolute. 'Syntax.Parsers.canonicalImport' makes an import's path
-- absolute but does not normalize it, so a directory imported as @"../notes"@ arrives as
-- @\/…\/here\/..\/notes@ — the same directory as @\/…\/notes@, spelled differently. The name a
-- directory is declared under is built from its path ('CtConstants.fileDirName'), and the whole
-- point of that is that one directory is one name however it was reached, so the path it is built
-- from has to be the one path there is. Symlinks are resolved for the same reason.
fileDirImport :: Expr () -> IO FilePath
fileDirImport imp = case listToMaybe $ mapMaybe argStr $ exprAppliedArgs imp of
  Just name -> canonicalizePath name
  Nothing -> fail $ printf "A %s import must name a directory, in %s" fileDirReference (show imp)
  where
    argStr ObjArr{oaArr=Just (Just (CExpr _ (CStr s)), _)} = Just s
    argStr _                                               = Nothing

-- | The import the page writes: the core, which is where the types it declares live.
--
-- Written rather than left to 'Syntax.Parsers.pageImports', which injects the core into any page
-- that does not say otherwise but does not do so for a page whose name holds @core@ — and the name
-- here is the directory being read, so a directory anywhere under one called @core@ would silently
-- be read without a standard library.
fileDirImports :: [RawFileImport]
fileDirImports = [mkRawFileImport $ rawStr "core"]

-- | Where a directory's page sits in the graph, which is known without reading any of the files.
--
-- The page brings in no pages of its own — the files are values in it, not pages beside it — so
-- there is nothing here that reading them could say. The walk still happens, because what was read
-- is part of where the page sits: see 'fileDirReads'.
fileDirPreload :: FileDirReader -> Expr () -> ImportPreloadResult
fileDirPreload reader imp = do
  dir <- fileDirImport imp
  reads' <- fileDirReads reader dir
  return $ PreloadImport fileDirImports [] Nothing reads'

-- | What a reading of a directory read: the tree, and every file in it.
--
-- Both, because they answer different questions and neither answers the other. A 'ReadTree' is what
-- 'CtService.readPathWatches' turns into the one recursive watch, and is what notices a file
-- appearing or being removed. But a tree's recorded time is the newest time any DIRECTORY under it
-- was written ('Syntax.Ct.Prgm.TreeScan'), and editing a file does not write to the directory
-- holding it — so a tree alone would hand back an unchanged page for a directory whose every file
-- had been rewritten, which is the one thing this must not do. The 'ReadFile's are what notice that.
fileDirReads :: FileDirReader -> FilePath -> IO ReadPaths
fileDirReads reader dir = do
  files <- fileDirFiles reader dir
  return $ Just $ ReadTree dir : map ReadFile files

-- | The files under a directory, in the order the walk found them, which is sorted.
--
-- Sorted matters rather than merely being tidy: these become the entries of a declaration, and a
-- re-read of an unchanged directory has to produce the same page. A listing in filesystem order
-- would make every read look like an edit.
fileDirFiles :: FileDirReader -> FilePath -> IO [FilePath]
fileDirFiles FileDirReader{fdEnv} dir = tsFiles <$> scanTree fdEnv dir

-- | A directory as the page declaring what is in it.
fileDirParse :: FileDirReader -> Expr () -> ImportParseResult
fileDirParse reader imp = do
  dir <- fileDirImport imp
  files <- fileDirFiles reader dir
  read' <- mapM (readEntry dir) files
  reads' <- fileDirReads reader dir
  let entries = [entry | Right entry <- read']
  let unread = sort [path | Left path <- read']
  return $ ParsedImport (RawPrgm fileDirImports (fileDirStatements dir entries unread)) Nothing reads'

-- | One file as the path it is at and what is written in it, or as the path alone where it is not
-- text.
--
-- A file which cannot be read as text is left out rather than failing the read. What a file holds is
-- a [String] — there is no type here for a file which is not text — and a directory with one PNG in
-- it is still a directory worth having the rest of. Which files those were is written onto the page
-- rather than passed over, so a file missing from the copy is a file the page says is missing.
readEntry :: FilePath -> FilePath -> IO (Either FilePath (FilePath, String))
readEntry dir file = do
  contents <- try (readFile' file) :: IO (Either IOException String)
  return $ case contents of
    Right text -> Right (makeRelative dir file, text)
    Left _     -> Left (makeRelative dir file)

-- | The statements a directory's page holds: the name to write to reach it, then the directory
-- itself as one declaration.
--
-- The name is written into the page as a heading because it is the one thing a reader has to
-- reproduce by hand — a @dir.ctc@ naming this directory writes exactly this name — and it is built
-- from the path rather than chosen, so it is not a name anyone would guess. See
-- 'CtConstants.fileDirName'.
fileDirStatements :: FilePath -> [(FilePath, String)] -> [FilePath] -> [RawStatementTree RawExpr ()]
fileDirStatements dir entries unread = heading : unreadNote ++ [decl]
  where
    heading = comment $ printf "The files of %s. Write `%s` to reach them." dir (fileDirName dir)
    unreadNote
      | null unread = []
      | otherwise = [comment $ printf "Not text, and so not carried: %s." (unwords unread)]
    comment text = RawStatementTree (RawAnnot (rawComment text)) []
    decl = RawStatementTree (RawDeclStatement $ RawObjArr (Just (rawVal $ fileDirName dir)) FunctionObj Nothing [] (Just (Just resultExpr, Nothing)) Nothing) []
    resultExpr = rawVal catlnResultPath `applyRawArgs` [(Just $ partialKey (mkName catlnResultFiles), RawList emptyMetaN $ map fileExpr entries)]
    fileExpr (path, contents) = rawVal buildFilePath `applyRawArgs` [
      (Just $ partialKey (mkName buildFileFilePath), rawStr path),
      (Just $ partialKey (mkName buildFileContents), rawStr contents)
                                                                   ]
