{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import           CRes
import           Options.Applicative
import           Semantics           (buildCtssConfig)
import           Semantics.Prgm      (FileImport, impDisp)
import           Syntax.Ct.Formatter (formatRootPrgm)
import           Syntax.Parsers      (mkDesCanonicalImportStr, mkRawImportStr,
                                      parseFile, readFiles)

import           Control.Monad
import           Control.Monad.Trans
import           CtService
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as S
import           Data.List           (isInfixOf)
import           Data.Maybe
import           Eval.Common         (Val (StrVal, TupleVal))
import           LSP                 (lspRun)
import           System.Directory
import           Text.Printf
import           Utils               (graphToNodes)
import           WebDocs             (docApi, docServe)
-- import Repl (repl)

xRun :: String -> String -> CResT IO ()
xRun prgmName function = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  returnValue <- getEvaluated ctss prgmName' function
  case returnValue of
    0 -> return ()
    i -> fail $ "error code " ++ show i

xBuild :: String -> String -> CResT IO ()
xBuild prgmName function = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  returnValue <- getEvalBuild ctss prgmName' function
  case returnValue of
    TupleVal _ args -> do
      let buildDir = "build"
      lift $ removePathForcibly buildDir
      lift $ createDirectoryIfMissing True buildDir
      case (fromJust $ H.lookup "name" args, fromJust $ H.lookup "contents" args) of
          (StrVal outFileName, StrVal outContents) -> do
            lift $ writeFile (buildDir ++ "/" ++ outFileName) outContents
            lift $ putStrLn $ printf "Successfully built %s" (show prgmName)
          _ -> fail "Invalid name or contents found in result as build"
    _ -> fail "Failed to build"

xDoc :: [String] -> Bool -> Bool -> CResT IO ()
xDoc prgmNames cached apiOnly = if apiOnly
  then lift $ docApi cached prgmNames
  else lift $ docServe cached prgmNames

xConvert :: String -> Maybe String -> CResT IO ()
xConvert prgmName _outFname = do
  (prgm, _fileName, _) <- parseFile buildCtssConfig $ mkRawImportStr prgmName
  -- TODO Print to file if outFname
  let prgm' = formatRootPrgm prgm
  lift $ print prgmName
  lift $ print prgm'

xLsp :: CResT IO ()
xLsp = do
  _ <- lift $ lspRun
  return ()

xFmt :: String -> CResT IO ()
xFmt prgmName = do
  (prgm, _, _) <- parseFile buildCtssConfig $ mkRawImportStr prgmName
  let prgm' = formatRootPrgm prgm
  lift $ writeFile prgmName prgm'

xDepTree :: String -> Bool -> CResT IO ()
xDepTree prgmName showCore = do
  -- Parse the root file and get canonical import
  rootImp <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName

  -- Build the full import graph
  graphData <- lift $ readFiles buildCtssConfig [mkRawImportStr prgmName]
  let nodes = graphToNodes graphData

  -- Build a lookup map from FileImport to its dependencies
  let depsMap = H.fromList [(imp, deps) | (_, imp, deps) <- nodes]

  -- Pretty-print the tree starting from root
  lift $ putStrLn $ displayName rootImp
  lift $ printTree depsMap S.empty "" rootImp
  where
    -- Display name for a FileImport (use impDisp if available, otherwise show)
    displayName :: FileImport -> String
    displayName imp = fromMaybe (show imp) (impDisp imp)
    
    -- Check if a dependency is a core dependency
    isCoreDep :: FileImport -> Bool
    isCoreDep imp = "core" `isInfixOf` displayName imp

    -- Print the dependency tree with proper tree characters
    printTree :: H.HashMap FileImport [FileImport] -> S.HashSet FileImport -> String -> FileImport -> IO ()
    printTree depsMap visited prefix imp = do
      let deps = H.lookupDefault [] imp depsMap
      -- Filter out core dependencies if showCore is False
      let filteredDeps = if showCore then deps else filter (not . isCoreDep) deps
      let visited' = S.insert imp visited
      let depCount = length filteredDeps
      mapM_ (printDep depsMap visited' prefix depCount) (zip [1..] filteredDeps)
      where
        printDep :: H.HashMap FileImport [FileImport] -> S.HashSet FileImport -> String -> Int -> (Int, FileImport) -> IO ()
        printDep dm vis pre totalDeps (idx, dep) = do
          let isLast = idx == totalDeps
          let connector = if isLast then "└── " else "├── "
          let extension = if isLast then "    " else "│   "
          let isCycle = S.member dep vis
          let depName = displayName dep ++ if isCycle then " (cycle)" else ""
          putStrLn $ pre ++ connector ++ depName
          unless isCycle $ printTree dm vis (pre ++ extension) dep

exec :: Command -> CResT IO ()
exec (RunFile file function)      = xRun file function
exec (BuildFile file function)    = xBuild file function
exec (Doc fname cached apiOnly)   = xDoc fname cached apiOnly
exec (Convert fname outFname)     = xConvert fname outFname
exec CLsp                         = xLsp
exec (Fmt fname)                  = xFmt fname
exec (DepTree fname showCore)     = xDepTree fname showCore

data Command
  = BuildFile String String
  | RunFile String String
  | Doc [String] Bool Bool
  | Convert String (Maybe String)
  | CLsp
  | Fmt String
  | DepTree String Bool

cRun :: Parser Command
cRun = RunFile
  <$> argument str (metavar "FILE" <> help "The file to run")
  <*> argument str (metavar "FUN" <> value "main" <> help "The function in the file to run")

cBuild :: Parser Command
cBuild = BuildFile
  <$> argument str (metavar "FILE" <> help "The file to build")
  <*> argument str (metavar "FUN" <> value "main" <> help "The function in the file to build")

cDoc :: Parser Command
cDoc = Doc
  <$> some (argument str (metavar "FILES" <> help "The files to run"))
  <*> switch (long "cached" <> help "Cache results instead of reloading live (useful for serving rather than development)")
  <*> switch (long "api" <> help "Serve only the API rather than the pages too")

cConvert :: Parser Command
cConvert = Convert
  <$> argument str (metavar "FILE" <> help "The file to run")
  <*> option auto (metavar "OUT" <> value Nothing <> help "The output file path")

cLsp :: Parser Command
cLsp = pure CLsp

cFmt :: Parser Command
cFmt = Fmt
  <$> argument str (metavar "FILE" <> help "The file to run")

cDepTree :: Parser Command
cDepTree = DepTree
  <$> argument str (metavar "FILE" <> help "The file to show dependency tree for")
  <*> switch (long "show-core" <> help "Show core library dependencies in the tree")

main :: IO ()
main = do
  cmd <- execParser opts
  res <- runCResT $ exec cmd
  case res of
    CRes notes _ -> unless (null notes) $ putStrLn $ prettyCNotes notes
    CErr notes   -> unless (null notes) $ putStrLn $ prettyCNotes notes
  where
    opts = info (mainCommands <**> helper)
      ( fullDesc
     <> progDesc "Executes Catln options"
     <> header "Catln Compiler" )
    mainCommands = subparser (
         command "run" (info cRun (progDesc "Runs a program"))
      <> command "build" (info cBuild (progDesc "Builds a program"))
      <> command "doc" (info cDoc (progDesc "Runs webdocs for a program"))
      <> command "convert" (info cConvert (progDesc "Converts code in one format to another"))
      <> command "lsp" (info cLsp (progDesc "Runs the language server"))
      <> command "fmt" (info cFmt (progDesc "Runs the Catln formatter for a program"))
      <> command "deptree" (info cDepTree (progDesc "Shows the dependency tree for a program"))
                             )
