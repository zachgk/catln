{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import           CRes
import           Options.Applicative
import           Semantics               (buildCtssConfig)
import           Semantics.Prgm          (FileImport, impDisp, impRaw)
import           Syntax.Ct.Formatter     (formatRootPrgm)
import           Syntax.Ct.Prgm          (ParsedImport (..), importPath,
                                          isCorePage, noImporters,
                                          rootImportCtx)
import           Syntax.Ct.Results       (setPrintResults)
import           Syntax.Parsers          (canonicalImport,
                                          mkDesCanonicalImportStr,
                                          mkRawImportStr, parseFile,
                                          parseImportAlone, readFiles)

import           Control.Monad
import           Control.Monad.Trans
import           CtRef                   (printRefIndex)
import           CtService
import           Data.Foldable           (foldlM)
import qualified Data.HashMap.Strict     as H
import qualified Data.HashSet            as S
import           Data.List               (isInfixOf, nub, sortOn)
import           Data.Maybe
import           Eval.Common             (SSF (..), TestKind (TestPrintGolden),
                                          TestResult (..))
import           Eval.Testing            (TestDesc (..), runTestDesc)
import           LSP                     (lspRun)
import           Syntax.HsBuiltins.Types (BuiltFile (..), valResultFiles)
import           System.Directory
import           System.Exit             (exitFailure)
import           System.FilePath         (isAbsolute, splitDirectories,
                                          takeDirectory, (</>))
import           Text.Megaparsec         (sourceName)
import           Text.Printf
import           TypeCheck.Common        (flipTraceConstrain,
                                          lookupEpochCountResult,
                                          lookupTraceResult)
import           TypeCheck.Instruments   (defaultInstruments, epochInstruments,
                                          typeInfoInstruments)
import           TypeCheck.TraceWrite    (renderTypeTrace)
import           Utils                   (fst3, graphToNodes, snd3, treeBranch)
import           WebDocs                 (docApi, docServe)
-- import Repl (repl)

xRun :: String -> String -> CResT IO ()
xRun prgmName function = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  returnValue <- getEvaluated ctss prgmName' function
  case returnValue of
    0 -> return ()
    i -> fail $ "error code " ++ show i

-- | Processes the annotations of a file which can be processed ('getProcessed'): the file processors
-- rewrite the file until none changes it, and each annotation a line processor updates is written
-- back into the file as what it gives, reformatting the file (like @catln fmt@).
xProcess :: String -> CResT IO ()
xProcess prgmName = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  ProcessReport{prPage, prLines, prLinesUnchanged, prFileRounds} <- getProcessed ctss prgmName'
  path <- case importPath prgmName' of
    Just path -> return path
    Nothing   -> fail $ printf "Could not find a file to write the processed %s back to" prgmName
  lift $ writeFile path $ formatRootPrgm prPage
  when (prFileRounds > 0) $ lift $ putStrLn $ printf "File processors changed %s %d time(s)" path prFileRounds
  lift $ putStrLn $ printf "Processed %d annotation(s) in %s" prLines path
  when (prLinesUnchanged > 0) $ lift $ putStrLn $ printf "%d of them came back unchanged" prLinesUnchanged

xBuild :: String -> String -> CResT IO ()
xBuild prgmName function = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  returnValue <- getEvalBuild ctss prgmName' function
  files <- either fail return $ valResultFiles returnValue
  let buildDir = "build"
  lift $ removePathForcibly buildDir
  lift $ createDirectoryIfMissing True buildDir
  mapM_ (writeBuiltFile buildDir) files
  lift $ putStrLn $ printf "Successfully built %s" (show prgmName)

-- | Writes one file of a build into the directory it is built into.
--
-- The directories along the way are made rather than required to exist, since a result says where
-- each of its files goes and not which directories it wanted along the way.
writeBuiltFile :: FilePath -> BuiltFile -> CResT IO ()
writeBuiltFile buildDir BuiltFile{bfPath, bfContents}
  -- A path names a place inside the build and nowhere else. One that does not is refused rather
  -- than written: this runs immediately after @build/@ has been removed and remade, so a program
  -- which could name its way out of that directory could have anything at all deleted and rewritten.
  | isAbsolute bfPath = fail $ printf "A build wrote %s, which is an absolute path rather than a path within the build" (show bfPath)
  | ".." `elem` splitDirectories bfPath = fail $ printf "A build wrote %s, which names a place outside the build" (show bfPath)
  | otherwise = do
      let path = buildDir </> bfPath
      lift $ createDirectoryIfMissing True $ takeDirectory path
      lift $ writeFile path bfContents

xTypeTrace :: String -> CResT IO ()
xTypeTrace prgmName = do
  let traceDir = "build/typetrace"
  -- Clear any previously generated trace first, so a failed run never leaves
  -- stale output from a prior file behind.
  lift $ removePathForcibly traceDir
  lift $ createDirectoryIfMissing True (traceDir ++ "/pnt")
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  -- Said before anything is built, and about this page alone. The page named on the command line is
  -- the one whose inference is going to be read, so its own typecheck is the one worth watching:
  -- watched here, it is compiled once, and every other page it is compiled against — which for the
  -- smallest program is the whole standard library — is compiled unwatched as it always is.
  lift $ ctssSetInstrument ctss $ \names ->
    if prgmName' `elem` names then typeInfoInstruments else defaultInstruments
  (vprgm, results) <- ctssTypeInfo typeInfoInstruments prgmName' ctss
  trace <- case lookupTraceResult results of
    Just trace -> return trace
    Nothing    -> fail $ printf "The typecheck of %s produced no trace to write" prgmName
  let epochs = printf "epochs: %s\n" (maybe "?" show $ lookupEpochCountResult results)
  let files = ("epochs.txt", epochs) : renderTypeTrace vprgm (flipTraceConstrain trace)
  forM_ files $ \(relPath, contents) ->
    lift $ writeFile (traceDir ++ "/" ++ relPath) contents
  lift $ putStrLn $ printf "Wrote %d typetrace files to %s" (length files) traceDir

xDoc :: [String] -> Bool -> Bool -> CResT IO ()
xDoc prgmNames apiOnly readonly = if apiOnly
  then lift $ docApi readonly prgmNames
  else lift $ docServe readonly prgmNames

-- | Show a file of another syntax as the Catln page it is read as.
--
-- Through the compilation service rather than through a lone parse, because reading a file of
-- another syntax may now mean running an importer declared in a @.ct@ file — which has to be built
-- before it can be run, and building is what a service does. A syntax the compiler supplies itself
-- would still be readable without one; going through the service is what makes the two the same
-- command.
xConvert :: String -> Maybe String -> CResT IO ()
xConvert prgmName _outFname = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  prgm <- getRawPrgmOf ctss prgmName'
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
  -- Format the parsed program alone (the source as written) rather than reading it through a
  -- service: formatting needs no other page, and so builds none.
  imp <- lift $ canonicalImport buildCtssConfig (rootImportCtx noImporters) $ mkRawImportStr prgmName
  ParsedImport{piPrgm=rawPrgm} <- lift $ parseImportAlone imp
  let prgm' = formatRootPrgm rawPrgm
  -- Written back to the file the import resolved to rather than to the name it was given by, which
  -- is not always a path: an import may name its parser instead ('mkRawImportStr'), and writing to
  -- that would make a file out of the import expression. A page with no file resolved is reported
  -- rather than written somewhere.
  case impDisp imp of
    Just path -> lift $ writeFile path prgm'
    Nothing   -> fail $ printf "Could not find a file to write the formatted %s back to" prgmName

-- | Builds the arrow trees ('TreeBuild') for every definition in a file and its imports, without
-- running or evaluating anything. This is the stage between typechecking and evaluation, so it
-- reports build errors that @catln run@ would only reach for the definitions it actually calls.
xTBuild :: String -> CResT IO ()
xTBuild prgmName = do
  prgmName' <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName
  ctss <- lift $ ctssBaseFiles buildCtssConfig [prgmName]
  g <- ctssGetFrom ssfTBPrgm prgmName' ctss
  lift $ putStrLn $ printf "Built arrow trees for %d programs" (length $ graphToNodes g)

-- | What the type inference of a whole compilation cost, in solver epochs.
--
-- An epoch is one pass of 'TypeCheck.Constrain.runConstraintsIdx' over a solve fragment's
-- constraints, and how many a fragment needs is decided by how far a value travels through the
-- constraint list per pass — that is, by the order 'TypeCheck.Encode' emits the constraints in. So
-- the total over every fragment of every page is what an ordering of them is answerable to, and
-- this is the thing that reports it.
--
-- Every component is watched, unlike @typetrace@, which watches the one page it was named: the
-- epoch counter keeps a single 'Integer', so watching everything costs nothing and a total over
-- part of a program is not a total. The build is run for its own sake ('ctssBuildAll') rather than
-- read back, so a page that fails to typecheck still reports what its solve spent before it failed.
xEpochs :: [String] -> CResT IO ()
xEpochs prgmNames = do
  ctss <- lift $ ctssBaseFiles buildCtssConfig prgmNames
  lift $ ctssSetInstrument ctss (const epochInstruments)
  lift $ ctssBuildAll ctss
  counts <- lift $ ctssEpochCounts ctss
  failed <- lift $ ctssFailedPageCount ctss
  forM_ (sortOn (negate . fst . snd) counts) $ \(name, (n, f)) ->
    lift $ putStrLn $ printf "%6d epochs %5d fragments  %s" n f (show $ impRaw name)
  -- The floor beside the total, because a fragment costs two epochs however well its constraints
  -- are ordered — the pass that changes something and the pass that finds nothing left to change.
  -- What an ordering can win back is the distance between the two numbers, not the first of them.
  let (totalEpochs, totalFrags) = (sum $ map (fst . snd) counts, sum $ map (snd . snd) counts)
  lift $ putStrLn $ printf "total %d epochs over %d fragments in %d components (floor %d, headroom %d)"
    totalEpochs totalFrags (length counts) (2 * totalFrags) (totalEpochs - 2 * totalFrags)
  -- Said beside the total and not instead of it, because a page that failed to typecheck stops its
  -- own fragment loop: a build that broke spends FEWER epochs than one that worked, and a total
  -- read without this could be a regression read as an improvement.
  when (failed > 0) $ lift $ putStrLn $ printf "WARNING: %d pages failed to typecheck" failed

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

xDepTree :: String -> Bool -> CResT IO ()
xDepTree prgmName showCore = do
  -- Parse the root file and get canonical import
  rootImp <- lift $ mkDesCanonicalImportStr buildCtssConfig prgmName

  -- Build the full import graph
  -- A one-shot read has nothing an earlier read left to give back. It reads no page in full
  -- either: a tree of what imports what is the graph, which a page answered out of a record still
  -- says.
  graphData <- lift $ readFiles buildCtssConfig S.empty H.empty [mkRawImportStr prgmName]
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

    -- Print the dependency tree, returning the updated visited set.
    -- The visited set is threaded through siblings so each node appears at most once.
    printTree :: H.HashMap FileImport [FileImport] -> S.HashSet FileImport -> String -> FileImport -> IO (S.HashSet FileImport)
    printTree dm visited prefix imp = do
      let deps = H.lookupDefault [] imp dm
      -- Filter out core dependencies if showCore is False, and already-visited nodes
      let filteredDeps = filter (not . (`S.member` visited))
                       $ if showCore then deps else filter (not . isCorePage) deps
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
exec (RunFile file function)      = xRun file function
exec (BuildFile file function)    = xBuild file function
exec (Doc fname apiOnly readonly) = xDoc fname apiOnly readonly
exec (Convert fname outFname)     = xConvert fname outFname
exec CLsp                         = xLsp
exec (Fmt fname)                  = xFmt fname
exec (DepTree fname showCore)     = xDepTree fname showCore
exec (RefIndex fname showCore)    = xRefIndex fname showCore
exec (TestFile fname update)      = xTest fname update
exec (TypeTrace fname)            = xTypeTrace fname
exec (TBuildFile fname)           = xTBuild fname
exec (Epochs fnames)              = xEpochs fnames
exec (Process fname)              = xProcess fname

data Command
  = BuildFile String String
  | RunFile String String
  | Doc [String] Bool Bool
  | Convert String (Maybe String)
  | CLsp
  | Fmt String
  | DepTree String Bool
  | RefIndex String Bool
  | TestFile String Bool
  | TypeTrace String
  | TBuildFile String
  | Epochs [String]
  | Process String

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
  <$> some (argument str (metavar "FILES" <> help "The files to run, each a path or an import naming the parser to read it with. The second needs quoting from the shell: 'obsidian(\"vault\")'"))
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
     subCommand "index" "Indexes all objects in a program as a namespace tree" cRefIndex
  <> subCommand "deptree" "Shows the dependency tree for a program" cDepTree
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

cEpochs :: Parser Command
cEpochs = Epochs
  <$> some (argument str (metavar "FILES" <> help "The files to compile and count type inference epochs for"))

cProcess :: Parser Command
cProcess = Process
  <$> argument str (metavar "FILE" <> help "The file whose annotations to process")

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
         subCommand "run" "Runs a program" cRun
      <> subCommand "build" "Builds a program" cBuild
      <> subCommand "doc" "Runs webdocs for a program" cDoc
      <> subCommand "convert" "Converts code in one format to another" cConvert
      <> subCommand "lsp" "Runs the language server" cLsp
      <> subCommand "fmt" "Runs the Catln formatter for a program" cFmt
      <> subCommand "ref" "Reference queries about a program" cRef
      <> subCommand "test" "Runs all tests in a program" cTest
      <> subCommand "typetrace" "Writes the type inference trace to the build directory" cTypeTrace
      <> subCommand "tbuild" "Builds the arrow trees for a program and its imports" cTBuild
      <> subCommand "epochs" "Reports how many type inference epochs compiling a program costs" cEpochs
      <> subCommand "process" "Processes the annotations of a file, such as asking each q> query, and writes them back" cProcess
                             )

-- | One command of the compiler, with the @--help@ describing it.
--
-- Help is a parser rather than a property of a program, so a command assembled without it does not
-- merely lack its own help — it rejects @--help@ as an option it does not have, and every @help@
-- written on its arguments is unreachable. Commands are built through here so that is not something
-- each of them has to remember, and so a command added later has its help by being a command.
subCommand :: String -> String -> Parser Command -> Mod CommandFields Command
subCommand name desc parser = command name (info (parser <**> helper) (progDesc desc))
