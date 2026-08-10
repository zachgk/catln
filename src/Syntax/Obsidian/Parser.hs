--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Obsidian.Parser
-- Copyright :  (c) Zach Kimberg 2026
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is for importing an Obsidian vault.
--------------------------------------------------------------------

module Syntax.Obsidian.Parser where

import           Data.List         (isPrefixOf, isSuffixOf, sort)
import           Semantics.Prgm
import           Semantics.Types   (Constant (CStr))
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           System.Directory
import           System.FilePath   ((</>))
import           Text.Printf

-- | Imports an Obsidian vault as the notes it holds.
--
-- The vault is a page of its own whose program is empty, holding every note as an import. Each note
-- is then read by the markdown parser as its own page, so a note is the same page whether the vault
-- or the note itself was imported.
--
-- Importing one directory as both a vault and a plain @dir@ makes two pages at the one display
-- path. 'Eval.Runtime.WebDocs.sidebarLevel' shows a directory holding pages as a single node, so
-- the two are shown as one; an empty one has no node to collapse into and is an error there.
obsidianParser :: ImportParser
obsidianParser imp = case exprAppliedArgs imp of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr vault)), _)}:_impArgs) -> do
    isDir <- doesDirectoryExist vault
    if isDir
      then do
        notes <- vaultNotes vault
        return (RawPrgm (map (mkRawFileImport . rawStr) notes) [], [], Nothing)
      else fail $ printf "Could not find the Obsidian vault directory %s" vault
  _ -> undefined

-- | The notes of a vault: every markdown file under it, at any depth.
--
-- Sorted, because 'listDirectory' answers in whatever order the filesystem holds it and the notes
-- become the vault page's imports — a part of the program that re-reading an unchanged vault must
-- reproduce, or 'CtService.ctssRead' takes every read for an edit and rebuilds the vault.
--
-- Entries named with a leading dot are skipped, which is what leaves out @.obsidian@: a vault's own
-- configuration is not notes of it. Directory symlinks are not followed either, as a vault holding
-- one that points at an ancestor would otherwise be walked forever.
vaultNotes :: FilePath -> IO [FilePath]
vaultNotes dir = do
  entries <- listDirectory dir
  notes <- mapM aux $ sort $ filter (not . ("." `isPrefixOf`)) entries
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
