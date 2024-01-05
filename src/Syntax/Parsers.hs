--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Parsers
-- Copyright :  (c) Zach Kimberg 2019
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is the main module for arbitary syntax parsing.
-- It will read in files from their file paths and then parse into a 'RawPrgm'.
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Syntax.Parsers where

import qualified Data.HashSet          as S

import           Control.Monad
import           CRes
import           Data.Graph
import qualified Data.HashMap.Strict   as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Syntax.Ct.Parser      (ctParser, ctxParser)
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Parser (hsParser)
import           System.Directory
import           Text.Printf
import           Utils

fileExtensionParsers :: H.HashMap String (String -> IO (CRes (RawPrgm ())))
fileExtensionParsers = H.fromList [
  ("ct", ctParser),
  ("ctx", ctxParser),
  ("hs", hsParser)
                             ]

isSupportedFileExtension :: String -> Bool
-- isSupportedFileExtension fileName = any ((`isSuffixOf` fileName) . ('.':)) (H.keys fileExtensionParsers)
isSupportedFileExtension fileName = ".ct" `isSuffixOf` fileName || ".ctx" `isSuffixOf` fileName

parseFile :: String -> IO (CRes (RawPrgm ()))
parseFile fileName = case H.lookup (last $ splitOn "." fileName) fileExtensionParsers of
  Just parser -> parser fileName
  Nothing -> return $ CErr [MkCNote $ GenCErr Nothing $ printf "Unexpected file extension %s" fileName]

-- replaces imports of a directory with directory/main.ct
dirImportToMain :: String -> IO String
dirImportToMain f = do
  isFile <- doesFileExist f
  isDir <- doesDirectoryExist f
  return $ case (isFile, isDir) of
    (True, False) -> f
    (False, True) -> f ++ "/main.ct"
    _             -> error $ printf "Invalid directory %s" f

readFiles :: Bool -> Bool -> [String] -> IO (CRes (GraphData (RawPrgm ()) String))
readFiles includeCore includeDependencies = fmap (fmap (graphFromEdges . snd)) . aux [] S.empty
  where
    aux acc visited [] = return $ return (visited, acc)
    aux acc visited (nextToVisit:restToVisit) | S.member nextToVisit visited = aux acc visited restToVisit
    aux acc visited (nextToVisit:restToVisit) = do
      isFile <- doesFileExist nextToVisit
      isDir <- doesDirectoryExist nextToVisit
      case (isFile, isDir) of
        (True, False) -> do -- file
          parsed <- parseFile nextToVisit
          case parsed of
            CErr notes -> return $ CErr notes
            CRes _ (parsedImports, statements) -> do
              let parsedImports' = if includeCore && not ("stack/core" `isPrefixOf` nextToVisit)
                    then "stack/core":parsedImports
                    else parsedImports
              parsedImports'' <- mapM dirImportToMain parsedImports'
              let prgm' = (parsedImports'', statements)
              let restToVisit' = if includeDependencies
                    then parsedImports' ++ restToVisit
                    else restToVisit
              aux ((prgm', nextToVisit, parsedImports'') : acc) (S.insert nextToVisit visited) restToVisit'
        (False, True) -> do -- directory
          files <- listDirectory nextToVisit
          files' <- forM files $ \file -> do
            let file' = nextToVisit ++ "/" ++ file
            isF <- doesFileExist file'
            isD <- doesDirectoryExist file'
            case (isF, isD) of
              (True, False) -> if not ("." `isPrefixOf` file) && isSupportedFileExtension file'
                then return (Just file')
                else return Nothing
              (False, True) -> return (Just file')
              _ -> error $ printf "Found non-file or directory: %s" file'
          aux acc visited (catMaybes files' ++ restToVisit)
        _ -> return $ CErr [MkCNote $ GenCErr Nothing $ printf "Could not find file or directory %s" nextToVisit]
