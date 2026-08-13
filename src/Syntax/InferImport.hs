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
import qualified Data.HashMap.Strict    as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Obsidian.Parser (noteVault, obsidian)
import           System.Directory
import           System.FilePath        (isAbsolute, joinPath)
import           Text.Printf

-- | The parser each file extension is read with, before where the file is is taken into account by
-- 'inferFileParser'.
fileExtensionParsers :: H.HashMap String String
fileExtensionParsers = H.fromList [
  ("ct", "ct"),
  ("ctx", "ctx"),
  ("md", "md"),
  ("hs", "haskell"),
  ("py", "python")
                             ]

-- | Imports a path with a named parser, as an import naming its own parser is written.
parserImport :: String -> FilePath -> RawExpr ()
parserImport parser path = rawVal parser `applyRawArgs` [(Nothing, rawStr path)]

isSupportedFileExtension :: String -> Bool
isSupportedFileExtension fileName = any ((`isSuffixOf` fileName) . ('.':)) (H.keys fileExtensionParsers)
-- isSupportedFileExtension fileName = ".ct" `isSuffixOf` fileName || ".ctx" `isSuffixOf` fileName

findExistingPath :: [FilePath] -> IO (Maybe FilePath)
findExistingPath [] = return Nothing
findExistingPath (p:ps) = do
  exists <- doesPathExist p
  if exists
    then Just <$> makeAbsolute p
    else findExistingPath ps

-- A directory has no source text of its own to write back — the files in it are pages of their own,
-- each written back by the parser which read it.
dirParser :: ImportParser
dirParser = readOnlyParser $ \_ imp -> case exprAppliedArgs imp of
  [ObjArr{oaArr=Just (Just (CExpr _ (CStr name)), _)}] -> do
    files <- listDirectory name
    files' <- forM files $ \file -> do
      let file' = name ++ "/" ++ file
      isF <- doesFileExist file'
      isD <- doesDirectoryExist file'
      case (isF, isD) of
        (True, False) -> if not ("." `isPrefixOf` file) && isSupportedFileExtension file'
          then return (Just file')
          else return Nothing
        (False, True) -> return (Just file')
        _ -> error $ printf "Found non-file or directory: %s" file'

    -- Main program
    let mainPath = name ++ "/main.ct"
    mainExists <- doesFileExist mainPath
    let dirPrgm = if mainExists
          then RawPrgm [mkRawFileImport $ rawStr mainPath] []
          else RawPrgm [] []

    -- The directory alone, because everything read here is read out of its listing: which files it
    -- holds, which of those are directories of their own, and whether it holds a main.ct. A file in
    -- it changing is a change to that file's own page, not to this one.
    return $ ParsedImport dirPrgm (map (mkRawFileImport . rawStr) $ catMaybes files') Nothing (Just [ReadDir name])
  _ -> undefined

-- | The places an import of @name@ could be: beside the file which wrote it, then in the standard
-- library.
importPathCandidates :: CTSSConfig -> Maybe FileImport -> String -> IO [FilePath]
importPathCandidates config caller name = do
  homeDir <- getHomeDirectory
  stackPath <- case joinPath [ctssStackPath config, name] of
    '~':'/':p -> return $ joinPath [homeDir, p]
    p         -> return p
  return $ if isAbsolute name
    then [name]
    else [maybe name (\p -> joinPath [p, name]) (caller >>= impDir), stackPath]

-- | The absolute path an import names, or 'Nothing' when it names nothing that is there.
--
-- Not every import names a path: @haskell@ takes a module name, which it resolves itself against
-- the Stack project it belongs to (see 'Syntax.Haskell.Parser.hsParser'). So this answers whether
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

inferRawImportStr :: CTSSConfig -> Maybe FileImport -> String -> IO (RawExpr ())
inferRawImportStr config caller name = do
  name' <- resolveImportPath config caller name
  isFile <- doesFileExist name'
  if isFile
    then do
      parser <- inferFileParser name'
      case parser of
        Just parser' -> return $ parserImport parser' name'
        Nothing -> fail $ printf "Unexpected file extension in import %s" name'
    else do
      return $ parserImport "dir" name'
