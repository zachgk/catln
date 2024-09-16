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
import           Semantics.Prgm
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
  ("hs", \n -> rawVal "haskell" `applyRawArgs` [(Nothing, rawStr n)])
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
          then ([mkRawFileImport $ rawStr mainPath], [])
          else ([], [])

    return (dirPrgm, map (mkRawFileImport . rawStr) $ catMaybes files')
  _ -> undefined

inferRawImportStr :: Maybe FileImport -> String -> IO (RawExpr ())
inferRawImportStr caller name = do
  let paths = if isAbsolute name
        then [name]
        else [maybe name (\p -> joinPath [p, name]) (caller >>= impDir), joinPath ["stack", name]]
  name' <- findExistingPath paths
  case name' of
    Nothing -> fail $ printf "Could not find file or directory %s" name
    Just name'' -> do
      isFile <- doesFileExist name''
      if isFile
        then do
          case H.lookup (last $ splitOn "." name'') fileExtensionParsers of
            Just parser -> return $ parser name''
            Nothing -> fail $ printf "Unexpected file extension in import %s" name''
        else do
          return (rawVal "dir" `applyRawArgs` [(Nothing, rawStr name'')])
