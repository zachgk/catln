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
import qualified Data.HashMap.Strict     as H
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Semantics.Prgm
import           Syntax.Ct.Desugarf.Expr (SemiDesMode (SDOutput), semiDesExpr)
import           Syntax.Ct.Parser.Syntax
import           Syntax.Ct.Prgm
import           System.Directory
import           Text.Printf

fileExtensionParsers :: H.HashMap String (String -> RawFileImport)
fileExtensionParsers = H.fromList [
  ("ct", \n -> rawVal "ct" `applyRawArgs` [(Nothing, rawStr n)]),
  ("ctx", \n -> rawVal "ctx" `applyRawArgs` [(Nothing, rawStr n)]),
  ("hs", \n -> rawVal "haskell" `applyRawArgs` [(Nothing, rawStr n)])
                             ]

-- readFiles :: Bool -> Bool -> [RawExpr ()] -> IO (CRes (GraphData (RawPrgm ()) (RawExpr ())))
-- readFiles includeCore includeDependencies = fmap (fmap (graphFromEdges . snd)) . aux [] S.empty
--   where
--     aux acc visited [] = return $ return (visited, acc)
--     aux acc visited (nextToVisit:restToVisit) | S.member nextToVisit visited = aux acc visited restToVisit
--     aux acc visited (nextToVisit:restToVisit) = do
--       isFile <- doesFileExist nextToVisit
--       isDir <- doesDirectoryExist nextToVisit
--       case (isFile, isDir) of
--         (True, False) -> do -- file
--           parsed <- parseFile nextToVisit
--           case parsed of
--             CErr notes -> return $ CErr notes
--             CRes _ (parsedImports, statements) -> do
--               let parsedImports' = if includeCore && not ("stack/core" `isPrefixOf` nextToVisit)
--                     then "stack/core":parsedImports
--                     else parsedImports
--               parsedImports'' <- mapM dirImportToMain parsedImports'
--               let prgm' = (parsedImports'', statements)
--               let restToVisit' = if includeDependencies
--                     then parsedImports' ++ restToVisit
--                     else restToVisit
--               aux ((prgm', nextToVisit, parsedImports'') : acc) (S.insert nextToVisit visited) restToVisit'
--         (False, True) -> do -- directory
--           files <- listDirectory nextToVisit
--           files' <- forM files $ \file -> do
--             let file' = nextToVisit ++ "/" ++ file
--             isF <- doesFileExist file'
--             isD <- doesDirectoryExist file'
--             case (isF, isD) of
--               (True, False) -> if not ("." `isPrefixOf` file) && isSupportedFileExtension file'
--                 then return (Just file')
--                 else return Nothing
--               (False, True) -> return (Just file')
--               _ -> error $ printf "Found non-file or directory: %s" file'
--           aux acc visited (catMaybes files' ++ restToVisit)
--         _ -> return $ CErr [MkCNote $ GenCErr Nothing $ printf "Could not find file or directory %s" nextToVisit]


isSupportedFileExtension :: String -> Bool
-- isSupportedFileExtension fileName = any ((`isSuffixOf` fileName) . ('.':)) (H.keys fileExtensionParsers)
isSupportedFileExtension fileName = ".ct" `isSuffixOf` fileName || ".ctx" `isSuffixOf` fileName

dirParser :: ImportParser
dirParser imp = do
  let [ObjArr{oaArr=(Just (GuardExpr (RawCExpr _ (CStr name)) _), _)}] = exprAppliedArgs imp
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
  let dirPrgm = ([rawStr (name ++ "/main.ct")], [])
  return (dirPrgm, map rawStr $ catMaybes files')

inferRawImportStr :: String -> IO RawFileImport
inferRawImportStr name = do
  isFile <- doesFileExist name
  isDir <- doesDirectoryExist name
  case (isFile, isDir) of
    (True, False) -> do -- file
      absName <- makeAbsolute name
      case H.lookup (last $ splitOn "." name) fileExtensionParsers of
        Just parser -> return $ parser absName
        Nothing -> fail $ printf "Unexpected file extension in import %s" name
    (False, True) -> do -- directory
      absName <- makeAbsolute name
      return (rawVal "dir" `applyRawArgs` [(Nothing, rawStr absName)])
    _ -> fail $ printf "Could not find file or directory %s" name

inferImportStr :: String -> IO FileImport
inferImportStr = fmap (semiDesExpr SDOutput Nothing) . inferRawImportStr
