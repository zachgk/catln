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
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Semantics
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           System.Directory
import           System.FilePath     (isAbsolute, joinPath)
import           Text.Printf

fileExtensionParsers :: H.HashMap String (String -> RawExpr ())
fileExtensionParsers = H.fromList [
  ("ct", \n -> rawVal "ct" `applyRawArgs` [(Nothing, rawStr n)]),
  ("ctx", \n -> rawVal "ctx" `applyRawArgs` [(Nothing, rawStr n)]),
  ("md", \n -> rawVal "md" `applyRawArgs` [(Nothing, rawStr n)]),
  ("hs", \n -> rawVal "haskell" `applyRawArgs` [(Nothing, rawStr n)]),
  ("py", \n -> rawVal "python" `applyRawArgs` [(Nothing, rawStr n)])
                             ]

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

dirParser :: ImportParser
dirParser imp = case exprAppliedArgs imp of
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

    return (dirPrgm, map (mkRawFileImport . rawStr) $ catMaybes files', Nothing)
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
-- Used for the inferred @import "name"@ form, which has nothing left to try: it picks its strategy
-- from what it finds at the path, so a path with nothing at it names no strategy either.
resolveImportPath :: CTSSConfig -> Maybe FileImport -> String -> IO FilePath
resolveImportPath config caller name = do
  paths <- importPathCandidates config caller name
  name' <- findExistingPath paths
  case name' of
    Nothing -> fail $ printf "Could not find file or directory %s. Tried paths %s" name (show paths)
    Just name'' -> return name''

inferRawImportStr :: CTSSConfig -> Maybe FileImport -> String -> IO (RawExpr ())
inferRawImportStr config caller name = do
  name' <- resolveImportPath config caller name
  isFile <- doesFileExist name'
  if isFile
    then do
      case H.lookup (last $ splitOn "." name') fileExtensionParsers of
        Just parser -> return $ parser name'
        Nothing -> fail $ printf "Unexpected file extension in import %s" name'
    else do
      return (rawVal "dir" `applyRawArgs` [(Nothing, rawStr name')])
