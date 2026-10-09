{-# OPTIONS_GHC -Wall -Wno-missing-fields #-}
--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Haskell.Parser
-- Copyright :  (c) Zach Kimberg 2023
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines a parser based on ghc-lib-parser
--------------------------------------------------------------------

module Syntax.Haskell.Parser where
import qualified Data.HashMap.Strict      as H
import           Data.IORef
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Driver.Config.Parser (initParserOpts)
import           GHC.Driver.Session
import           GHC.Fingerprint
import           GHC.Parser
import           GHC.Parser.Header
import           GHC.Parser.Lexer
import           GHC.Platform
import           GHC.Settings
import           GHC.Settings.Config
import           GHC.Types.SourceError
import           GHC.Types.SrcLoc
import           GHC.Utils.Panic
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Convert   (convertModule)
import           Syntax.Haskell.Stack     (StackProjectInfo,
                                           loadStackDependencyGraph,
                                           resolveModuleInProject)
import           System.Directory         (doesFileExist)
import           System.FilePath          (takeDirectory, (</>))
import           Text.Printf

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

fakeSettings :: Settings
fakeSettings = Settings {
  sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=genericPlatform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
  where
    fileSettings = FileSettings {
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
       }

    ghcNameVersion = GhcNameVersion{
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

-- From https://blog.shaynefletcher.org/2019/06/have-ghc-parsing-respect-dynamic-pragmas.html
-- Based on https://github.com/digital-asset/ghc-lib/blob/master/examples/ghc-lib-test-mini-hlint/src/Main.hs
parsePragmasIntoDynFlags :: FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags filepath str =
  catchErrors $ do
    let (_, opts) = getOptions (initParserOpts fakeFlags) (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma fakeFlags opts
    return $ Just flags
  where
    fakeFlags = defaultDynFlags fakeSettings fakeLlvmConfig
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = do putStrLn $ "error : " ++ show e; return Nothing

-- | Find the stack root by looking for stack.yaml in parent directories
findStackRoot :: FilePath -> IO (Maybe FilePath)
findStackRoot startPath = do
  let dir = takeDirectory startPath
  if dir == startPath  -- Reached filesystem root
    then return Nothing
    else do
      let stackYaml = dir </> "stack.yaml"
      exists <- doesFileExist stackYaml
      if exists
        then return (Just dir)
        else findStackRoot dir

-- From https://www.stackage.org/haddock/lts-21.25/ghc-lib-parser-8.10.7.20220219/Parser.html
runParser :: DynFlags -> String -> String -> P a -> ParseResult a
runParser flags filename str parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState (initParserOpts flags) buffer location

-- TODO Write a Haskell module back. Nothing reads one back yet, and a module is read for what it
-- declares rather than in full, so there is not enough of it in the program to write one.
hsSyntax :: ImportSyntax
hsSyntax = ImportSyntax newHaskellReader (const Nothing) importedLinkDir Nothing Nothing Nothing

-- | The reader one read reads Haskell modules with.
newHaskellReader :: ReadEnv -> IO ImportReader
newHaskellReader _ = do
  reader <- HaskellReader <$> newIORef H.empty
  let parse = hsParse reader
  return $ readyReader (fmap eagerPreload . parse) parse (return ())

-- | What one read has found of the Stack projects the modules it reads belong to.
--
-- A module named by module name rather than by path is resolved by asking Stack itself, which
-- 'Syntax.Haskell.Stack.loadStackDependencyGraph' does by running it. That is a process started and
-- waited on, and every module of one project asks the same question of it, so a read of a project's
-- modules would start one Stack per module to be told the same thing each time.
--
-- Keyed by the project's root, since that is what the answer is about. Per read rather than kept
-- between reads, for the reason 'Syntax.Ct.Prgm.reScans' gives: what is on disk changes between
-- reads, and a read is the span over which it is taken not to.
newtype HaskellReader = HaskellReader {
  hrProjects :: IORef (H.HashMap FilePath StackProjectInfo)
                                      }

-- | The Stack project rooted here, as this read found it, asking Stack if this read has not.
readStackProject :: HaskellReader -> FilePath -> IO StackProjectInfo
readStackProject HaskellReader{hrProjects} stackRoot = do
  found <- readIORef hrProjects
  case H.lookup stackRoot found of
    Just info -> return info
    Nothing -> do
      info <- loadStackDependencyGraph stackRoot
      modifyIORef' hrProjects $ H.insert stackRoot info
      return info

hsParse :: HaskellReader -> Expr () -> ImportParseResult
hsParse reader imp = case [arg | arg@ObjArr{oaObj=Nothing} <- exprAppliedArgs imp] of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr filename)), _)}:_) -> do
    -- Extract stackRoot and exportAll parameters from all arguments (including named ones)
    let maybeStackRoot = extractStackRoot (exprAppliedArgs imp)
        exportAll = extractExportAll (exprAppliedArgs imp)

    isFile <- doesFileExist filename

    -- The file read, alongside what this reading read. A module named by a path is read from that
    -- path, whether or not anything is there. A module named by module name is resolved by asking
    -- Stack itself — 'Syntax.Haskell.Stack.loadStackDependencyGraph' shells out to it — so what
    -- that resolution read cannot be stated, and the page says so rather than claiming a read set
    -- it does not have. Such a page is read again every time.
    (actualFile, reads') <- if isFile
      then return (Just filename, Just [ReadFile filename])
      else case maybeStackRoot of
        Just stackRoot -> do
          -- Try to resolve module name using Stack, out of what this read has already asked it
          stackInfo <- readStackProject reader stackRoot
          resolved <- resolveModuleInProject stackInfo filename
          return (resolved, Nothing)
        Nothing -> return (Nothing, Just [ReadFile filename])

    case actualFile of
      Just file -> do
        -- Auto-detect stack root if not provided
        stackRoot <- case maybeStackRoot of
          Just sr -> return (Just sr)
          Nothing -> findStackRoot file

        str <- readFile file
        maybeFlags <- parsePragmasIntoDynFlags file str
        case maybeFlags of
          Nothing -> fail $ printf "Failed to read flags from %s" file
          Just flags -> do
            let parsed = runParser flags file str parseModule
            case parsed of
              POk _ v -> return $ ParsedImport (convertModule flags exportAll stackRoot $ unLoc v) Nothing reads'
              PFailed _pstate ->
                -- TODO uncomment and fix this to show parse errors instead of silently returning an empty program
                -- let realSpan = psRealSpan $ last_loc pstate
                --     errMsg = printErrorBag flags $ fmap (unDecorated . diagnosticMessage . errMsgDiagnostic) $ getMessages $ snd $ getPsMessages pstate
                --     lnStart = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
                --     colStart = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
                --     lnEnd = srcLocLine $ SrcLoc.realSrcSpanEnd realSpan
                --     colEnd = srcLocCol $ SrcLoc.realSrcSpanEnd realSpan
                --     _srcPos = Just (SourcePos file (mkPos lnStart) (mkPos colStart), SourcePos file (mkPos lnEnd) (mkPos colEnd), file)
                -- in fail errMsg

                -- Return empty program on parse failure to skip unparseable files (e.g., files with CPP)
                return $ ParsedImport (RawPrgm [] []) Nothing reads'
      Nothing -> return $ ParsedImport (RawPrgm [] []) Nothing reads'
  _ -> undefined

  where
    -- Extract stackRoot parameter from arguments
    extractStackRoot :: [ObjArr Expr ()] -> Maybe FilePath
    extractStackRoot [] = Nothing
    extractStackRoot (ObjArr{oaObj=Just obj, oaArr=Just (Just (CExpr _ (CStr sr)), _)}:rest) =
      case maybeExprPath obj of
        Just "/stackRoot" -> Just sr  -- Desugared form has leading slash
        Just "stackRoot"  -> Just sr  -- Raw form (backward compatibility)
        _                 -> extractStackRoot rest
    extractStackRoot (_:rest) = extractStackRoot rest

    -- Extract exportAll parameter from arguments (defaults to False)
    extractExportAll :: [ObjArr Expr ()] -> Bool
    extractExportAll [] = False
    extractExportAll (ObjArr{oaObj=Just obj, oaArr=Just (Just (Value _ "True"), _)}:rest) =
      case maybeExprPath obj of
        Just "/exportAll" -> True  -- Desugared form has leading slash
        Just "exportAll"  -> True  -- Raw form (backward compatibility)
        _                 -> extractExportAll rest
    extractExportAll (_:rest) = extractExportAll rest
