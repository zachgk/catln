{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import           CRes
import           CtConstants          (printAnnot, printAnnotText,
                                       resultAnnotText)
import           Options.Applicative
import           Semantics            (buildCtssConfig)
import           Semantics.Prgm       (CodeRangeDat, FileImport, MetaDat,
                                       exprPath, getExprMeta, getMetaPos,
                                       impDisp)
import           Semantics.Types      (mkName, nameStr, partialKey)
import           Syntax.Ct.Builder    (applyRawArgs, rawStr, rawVal)
import           Syntax.Ct.Formatter  (formatRootPrgm)
import           Syntax.Ct.Prgm       (RawPrgm (..), RawStatement (..),
                                       RawStatementTree (..),
                                       rawExprAppliedArgsMap)
import           Syntax.Parsers       (canonicalImport, mkDesCanonicalImportStr,
                                       mkRawImportStr, parseFile, readFiles,
                                       readImport)

import           Control.Monad
import           Control.Monad.Trans
import           CtRef                (printRefIndex)
import           CtService
import           Data.Foldable        (foldlM)
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as S
import           Data.List            (isInfixOf, nub)
import           Data.Maybe
import           Eval.Common          (TestKind (TestPrintGolden),
                                       TestResult (..), Val (StrVal, TupleVal))
import           Eval.Testing         (TestDesc (..), runTestDesc)
import           LSP                  (lspRun)
import           System.Directory
import           System.Exit          (exitFailure)
import           Text.Megaparsec      (sourceName)
import           Text.Printf
import           TypeCheck.Common     (flipTraceConstrain)
import           TypeCheck.TraceWrite (renderTypeTrace)
import           Utils                (fst3, graphToNodes, snd3, treeBranch)
import           WebDocs              (docApi, docServe)
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

xTypeTrace :: String -> CResT IO ()
xTypeTrace prgmName = do
  let traceDir = "build/typetrace"
  -- Clear any previously generated trace first, so a failed run never leaves
  -- stale output from a prior file behind.
  lift $ removePathForcibly traceDir
  lift $ createDirectoryIfMissing True (traceDir ++ "/pnt")
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  (_, vprgm, tr) <- ctssLookupReq ssfTPrgmWithTrace prgmName' ctss
  let files = renderTypeTrace vprgm (flipTraceConstrain tr)
  forM_ files $ \(relPath, contents) ->
    lift $ writeFile (traceDir ++ "/" ++ relPath) contents
  lift $ putStrLn $ printf "Wrote %d typetrace files to %s" (length files) traceDir

xDoc :: [String] -> Bool -> Bool -> Bool -> CResT IO ()
xDoc prgmNames cached apiOnly readonly = if apiOnly
  then lift $ docApi cached readonly prgmNames
  else lift $ docServe cached readonly prgmNames

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
  -- Format the RAW parsed program (the source as written), NOT the result of the full parse
  -- pipeline: 'processParsed' injects an implicit @import "core"@ for compilation, and formatting
  -- that would write a duplicate @import "core"@ back into any file that already imports it.
  imp <- lift $ canonicalImport buildCtssConfig Nothing $ mkRawImportStr prgmName
  (rawPrgm, _, _) <- lift $ readImport imp
  let prgm' = formatRootPrgm rawPrgm
  lift $ writeFile prgmName prgm'

-- | Builds the arrow trees ('TreeBuild') for every definition in a file and its imports, without
-- running or evaluating anything. This is the stage between typechecking and evaluation, so it
-- reports build errors that @catln run@ would only reach for the definitions it actually calls.
xTBuild :: String -> CResT IO ()
xTBuild prgmName = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  g <- ctssGetFrom ssfTBPrgm prgmName' ctss
  lift $ putStrLn $ printf "Built arrow trees for %d programs" (length $ graphToNodes g)

xTest :: String -> Bool -> CResT IO ()
xTest prgmName update = do
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  testDescs <- getTestResults ctss
  -- In update mode the print-golden checks are skipped (we are about to write the goldens, so
  -- they would otherwise fail as "missing"); every other test family still runs and is checked.
  let testDescs' = if update
        then filter ((/= TestPrintGolden) . tdKind) testDescs
        else testDescs
  results <- lift $ mapM runTestDesc testDescs'
  let failures = filter (not . trPassed) results
  let total = length results
  let passed = total - length failures
  lift $ putStrLn $ printf "Tests: %d/%d passed" passed total
  forM_ failures $ \result -> do
    lift $ putStrLn $ printf "  FAIL: %s" (trName result)
    lift $ mapM_ putStrLn (trNotes result)
  when update $ xTestWriteGoldens ctss
  unless (null failures) $ fail $ printf "%d test(s) failed" (length failures)

-- | Evaluate every string-serializable @#print@ and write its value back into the @#result@ of the
-- source file that contains it, reformatting each touched file (like @catln fmt@).
xTestWriteGoldens :: CTSS -> CResT IO ()
xTestWriteGoldens ctss = do
  goldens <- getPrintGoldens ctss
  let files = nub $ map (sourceName . fst . fst) goldens
  forM_ files $ \file -> do
    let fileGoldens = [(cr, g) | (cr, g) <- goldens, sourceName (fst cr) == file]
    (prgm, _, _) <- parseFile buildCtssConfig $ mkRawImportStr file
    lift $ writeFile file $ formatRootPrgm $ setPrintResults fileGoldens prgm
  lift $ putStrLn $ printf "Updated %d golden(s) across %d file(s)" (length goldens) (length files)

-- | Inject each evaluated golden into the @#result@ arg of its matching @#print@ statement,
-- located by the source range of the print's @p@ argument.
setPrintResults :: (MetaDat m, Show m) => [(CodeRangeDat, String)] -> RawPrgm m -> RawPrgm m
setPrintResults goldens (RawPrgm imports stmts) = RawPrgm imports (map fixTree stmts)
  where
    fixTree (RawStatementTree st subs) = RawStatementTree (fixStmt st) (map fixTree subs)
    fixStmt (RawAnnot annot)
      | nameStr (exprPath annot) == printAnnot
      , Just pExpr <- pArg annot
      , Just crange <- getMetaPos (getExprMeta pExpr)
      , Just golden <- lookup crange goldens
      = RawAnnot $ rawVal printAnnot `applyRawArgs`
          [(Just $ partialKey (mkName printAnnotText), pExpr), (Just $ partialKey (mkName resultAnnotText), rawStr golden)]
    fixStmt st = st
    pArg annot = case H.lookup (partialKey (mkName printAnnotText)) (rawExprAppliedArgsMap annot) of
      Just (Just (_, Just e)) -> Just e
      _                       -> Nothing

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
  _ <- lift $ printTree depsMap (S.singleton rootImp) "" rootImp
  return ()
  where
    -- Display name for a FileImport (use impDisp if available, otherwise show)
    displayName :: FileImport -> String
    displayName imp = fromMaybe (show imp) (impDisp imp)

    -- Check if a dependency is a core dependency
    isCoreDep :: FileImport -> Bool
    isCoreDep imp = "core" `isInfixOf` displayName imp

    -- Print the dependency tree, returning the updated visited set.
    -- The visited set is threaded through siblings so each node appears at most once.
    printTree :: H.HashMap FileImport [FileImport] -> S.HashSet FileImport -> String -> FileImport -> IO (S.HashSet FileImport)
    printTree dm visited prefix imp = do
      let deps = H.lookupDefault [] imp dm
      -- Filter out core dependencies if showCore is False, and already-visited nodes
      let filteredDeps = filter (not . (`S.member` visited))
                       $ if showCore then deps else filter (not . isCoreDep) deps
      let depCount = length filteredDeps
      foldlM (printDep dm prefix depCount) visited (zip [1..] filteredDeps)
      where
        printDep :: H.HashMap FileImport [FileImport] -> String -> Int -> S.HashSet FileImport -> (Int, FileImport) -> IO (S.HashSet FileImport)
        printDep dm' pre totalDeps vis (idx, dep) = do
          let (connector, extension) = treeBranch (idx == totalDeps)
          putStrLn $ pre ++ connector ++ displayName dep
          printTree dm' (S.insert dep vis) (pre ++ extension) dep

xRefIndex :: String -> Bool -> CResT IO ()
xRefIndex prgmName showCore = do
  -- Use the desugared program: object names are resolved to absolute namespaced paths and
  -- source positions are preserved, without requiring the whole program to type-check.
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  graph <- getPrgm ctss
  let nodes = graphToNodes graph
  -- Filter core at the file (node) level, mirroring xDepTree's isCoreDep.
  let keptNodes = if showCore then nodes else filter (not . isCoreImport . snd3) nodes
  lift $ printRefIndex $ mconcat $ map fst3 keptNodes
  where
    isCoreImport :: FileImport -> Bool
    isCoreImport imp = "core" `isInfixOf` fromMaybe (show imp) (impDisp imp)

exec :: Command -> CResT IO ()
exec (RunFile file function)             = xRun file function
exec (BuildFile file function)           = xBuild file function
exec (Doc fname cached apiOnly readonly) = xDoc fname cached apiOnly readonly
exec (Convert fname outFname)            = xConvert fname outFname
exec CLsp                                = xLsp
exec (Fmt fname)                         = xFmt fname
exec (DepTree fname showCore)            = xDepTree fname showCore
exec (RefIndex fname showCore)           = xRefIndex fname showCore
exec (TestFile fname update)             = xTest fname update
exec (TypeTrace fname)                   = xTypeTrace fname
exec (TBuildFile fname)                  = xTBuild fname

data Command
  = BuildFile String String
  | RunFile String String
  | Doc [String] Bool Bool Bool
  | Convert String (Maybe String)
  | CLsp
  | Fmt String
  | DepTree String Bool
  | RefIndex String Bool
  | TestFile String Bool
  | TypeTrace String
  | TBuildFile String

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
  <*> switch (long "readonly" <> help "Disable saving edits from the webdocs editor")

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

cRefIndex :: Parser Command
cRefIndex = RefIndex
  <$> argument str (metavar "FILE" <> help "The file to index")
  <*> switch (long "show-core" <> help "Include core library objects in the index")

cRef :: Parser Command
cRef = subparser (
     command "index" (info cRefIndex (progDesc "Indexes all objects in a program as a namespace tree"))
  <> command "deptree" (info cDepTree (progDesc "Shows the dependency tree for a program"))
                 )

cTest :: Parser Command
cTest = TestFile
  <$> argument str (metavar "FILE" <> help "The file to test")
  <*> switch (long "update" <> short 'u' <> help "Write #result goldens for #print statements instead of checking them")

cTBuild :: Parser Command
cTBuild = TBuildFile
  <$> argument str (metavar "FILE" <> help "The file to build arrow trees for")

cTypeTrace :: Parser Command
cTypeTrace = TypeTrace
  <$> argument str (metavar "FILE" <> help "The file to trace type inference for")

main :: IO ()
main = do
  cmd <- execParser opts
  res <- runCResT $ exec cmd
  case res of
    CRes notes _ -> unless (null notes) $ putStrLn $ prettyCNotes notes
    CErr notes   -> do
      unless (null notes) $ putStrLn $ prettyCNotes notes
      exitFailure
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
      <> command "ref" (info cRef (progDesc "Reference queries about a program"))
      <> command "test" (info cTest (progDesc "Runs all tests in a program"))
      <> command "typetrace" (info cTypeTrace (progDesc "Writes the type inference trace to the build directory"))
      <> command "tbuild" (info cTBuild (progDesc "Builds the arrow trees for a program and its imports"))
                             )
