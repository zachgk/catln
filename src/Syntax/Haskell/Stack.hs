{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Haskell.Stack
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides integration with Haskell Stack projects
-- by shelling out to the stack CLI tool.
--------------------------------------------------------------------

module Syntax.Haskell.Stack
  ( StackProjectInfo(..)
  , loadStackDependencyGraph
  , resolveModuleInProject
  ) where

import           Control.Exception (SomeException, try)
import           Control.Monad     (unless)
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified System.Directory  as Dir
import           System.FilePath   (takeDirectory, (</>))
import           System.IO         (hGetContents)
import           System.Process    (CreateProcess (..), StdStream (..),
                                    createProcess, proc, waitForProcess)

-- | Run a process and capture stdout while suppressing stderr
readProcessSilent :: FilePath -> [String] -> IO String
readProcessSilent cmd args = do
  let process = (proc cmd args)
        { std_out = CreatePipe
        , std_err = CreatePipe  -- Capture stderr to suppress it
        , std_in = NoStream
        }
  (_, Just hout, Just herr, ph) <- createProcess process

  -- Read stdout (force evaluation before closing)
  output <- hGetContents hout
  let !strictOutput = length output `seq` output

  -- Discard stderr silently (force evaluation)
  stderrOutput <- hGetContents herr
  let !_ = length stderrOutput `seq` ()

  -- Wait for process to complete
  _ <- waitForProcess ph

  -- Handles will be closed automatically after hGetContents is fully consumed

  return strictOutput

-- | Information about a Stack project's dependency graph
data StackProjectInfo = StackProjectInfo
  { spiLocalPackages :: !(Set.Set FilePath)
    -- ^ Paths to local package directories
  , spiDependencies  :: !(Map.Map String [String])
    -- ^ Map from package names to their dependencies
  , spiProjectRoot   :: !FilePath
    -- ^ Root directory of the project
  }

-- | Load dependency information for a Stack project by calling stack CLI
loadStackDependencyGraph :: FilePath -> IO StackProjectInfo
loadStackDependencyGraph projectRoot = do
  -- Normalize the project root path
  absProjectRoot <- Dir.makeAbsolute projectRoot
  let stackYaml = absProjectRoot </> "stack.yaml"

  -- Check if stack.yaml exists
  exists <- Dir.doesFileExist stackYaml
  if not exists
    then return $ StackProjectInfo Set.empty Map.empty absProjectRoot
    else do
      -- Get local package directories using `stack path --local-pkg-db`
      localPkgs <- getLocalPackages absProjectRoot stackYaml

      -- Get dependencies using `stack ls dependencies`
      deps <- getDependencies absProjectRoot stackYaml

      return $ StackProjectInfo
        { spiLocalPackages = localPkgs
        , spiDependencies = deps
        , spiProjectRoot = absProjectRoot
        }

-- | Get local package directories from stack project
getLocalPackages :: FilePath -> FilePath -> IO (Set.Set FilePath)
getLocalPackages projectRoot _stackYaml = do
  -- Change to project directory since some stack versions don't support --stack-yaml
  currentDir <- Dir.getCurrentDirectory
  Dir.setCurrentDirectory projectRoot
  result <- try $ readProcessSilent "stack"
    ["ide", "packages", "--cabal-files"]
  Dir.setCurrentDirectory currentDir
  case result of
    Left (_ :: SomeException) -> do
      -- Fallback: look for common patterns
      return $ Set.fromList [projectRoot]
    Right output -> do
      let cabalFiles = filter (not . null) $ lines output
      if null cabalFiles
        then do
          -- Fallback if stack returns empty output
          return $ Set.fromList [projectRoot]
        else do
          let pkgDirs = Set.fromList $ map takeDirectory cabalFiles
          return pkgDirs

-- | Get dependencies from stack project
getDependencies :: FilePath -> FilePath -> IO (Map.Map String [String])
getDependencies projectRoot _stackYaml = do
  -- Change to project directory since some stack versions don't support --stack-yaml
  currentDir <- Dir.getCurrentDirectory
  Dir.setCurrentDirectory projectRoot
  result <- try $ readProcessSilent "stack"
    ["ls", "dependencies", "--separator", " "]
  Dir.setCurrentDirectory currentDir
  case result of
    Left (_ :: SomeException) -> return Map.empty
    Right output -> do
      let depLines = filter (not . null) $ lines output
          parseDep line = case words line of
            (pkg:_version:_) -> Just (pkg, [])  -- Simplified: just track package names
            _                -> Nothing
          deps = Map.fromList $ map (, []) $
                   [pkg | Just (pkg, _) <- map parseDep depLines]
      return deps

-- | Resolve a module name to a file path within a Stack project
-- Module name should be in slash-separated or dot-separated format
resolveModuleInProject :: StackProjectInfo -> String -> IO (Maybe FilePath)
resolveModuleInProject info moduleName = do
  -- Convert module name to file path format
  -- e.g., "Data/Map" or "Data.Map" -> "Data/Map.hs"
  let moduleFile = map (\c -> if c == '.' then '/' else c) moduleName
      possibleExts = [".hs", ".lhs"]

  -- Search through all local packages first
  localResult <- findModuleInPackages (Set.toList $ spiLocalPackages info) moduleFile possibleExts
  case localResult of
    Just found -> return (Just found)
    Nothing    -> resolveModuleExternal (spiProjectRoot info) moduleName
 where
  findModuleInPackages [] _ _ = return Nothing
  findModuleInPackages (pkgDir:rest) moduleFile exts = do
    mbFound <- searchInSourceDirs pkgDir moduleFile exts
    case mbFound of
      Just found -> return (Just found)
      Nothing    -> findModuleInPackages rest moduleFile exts

  searchInSourceDirs pkgDir moduleFile exts = do
    -- Common Haskell source directories
    let sourceDirs = ["src", "lib", "app", "test", "."]
        candidates = [ pkgDir </> srcDir </> moduleFile ++ ext
                     | srcDir <- sourceDirs
                     , ext <- exts
                     ]

    -- Find the first existing file
    findFirstExisting candidates

  findFirstExisting [] = return Nothing
  findFirstExisting (path:paths) = do
    exists <- Dir.doesFileExist path
    if exists
      then return (Just path)
      else findFirstExisting paths

-- | Resolve an external module by finding its package and downloading source if needed
resolveModuleExternal :: FilePath -> String -> IO (Maybe FilePath)
resolveModuleExternal projectRoot moduleName = do
  -- Convert module name to dot-separated format for ghc-pkg
  let moduleNameDot = map (\c -> if c == '/' then '.' else c) moduleName
      moduleFile = map (\c -> if c == '.' then '/' else c) moduleName
      unpackedDir = projectRoot </> ".stack-work" </> "unpacked"

  -- Find which package provides this module
  currentDir <- Dir.getCurrentDirectory
  Dir.setCurrentDirectory projectRoot
  result <- try $ readProcessSilent "stack"
    ["exec", "--", "ghc-pkg", "find-module", moduleNameDot, "--simple-output"]
  Dir.setCurrentDirectory currentDir

  case result of
    Left (_ :: SomeException) -> return Nothing
    Right output -> do
      let pkgNames = words output
      if null pkgNames
        then return Nothing  -- Module not found in any package
        else do
          let pkgName = head pkgNames
          -- Check if package is already unpacked
          let pkgDir = unpackedDir </> pkgName
          pkgExists <- Dir.doesDirectoryExist pkgDir

          -- Unpack if needed
          unless pkgExists $ do
            _ <- Dir.createDirectoryIfMissing True unpackedDir
            currentDir' <- Dir.getCurrentDirectory
            Dir.setCurrentDirectory unpackedDir
            _ <- try @SomeException $ readProcessSilent "stack" ["unpack", pkgName]
            Dir.setCurrentDirectory currentDir'
            return ()

          -- Search for module in unpacked package
          searchInUnpackedPackage pkgDir moduleFile
 where
  searchInUnpackedPackage pkgDir moduleFile = do
    let possibleExts = [".hs", ".lhs"]
        sourceDirs = ["src", "lib", ".", "libraries/base"]  -- base package has nested structure
        candidates = [ pkgDir </> srcDir </> moduleFile ++ ext
                     | srcDir <- sourceDirs
                     , ext <- possibleExts
                     ]

    findFirstExisting candidates

  findFirstExisting [] = return Nothing
  findFirstExisting (path:paths) = do
    exists <- Dir.doesFileExist path
    if exists
      then return (Just path)
      else findFirstExisting paths
