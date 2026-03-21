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
import           Syntax.Haskell.Stack     (loadStackDependencyGraph,
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

hsParser :: ImportParser
hsParser imp = case [arg | arg@ObjArr{oaObj=Nothing} <- exprAppliedArgs imp] of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr filename)), _)}:_) -> do
    -- Extract stackRoot and exportAll parameters from all arguments (including named ones)
    let maybeStackRoot = extractStackRoot (exprAppliedArgs imp)
        exportAll = extractExportAll (exprAppliedArgs imp)

    isFile <- doesFileExist filename

    actualFile <- if isFile
      then return (Just filename)
      else case maybeStackRoot of
        Just stackRoot -> do
          -- Try to resolve module name using Stack
          stackInfo <- loadStackDependencyGraph stackRoot
          resolveModuleInProject stackInfo filename
        Nothing -> return Nothing

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
              POk _ v -> do
                let (prgm, imps) = convertModule flags exportAll stackRoot $ unLoc v
                return (prgm, imps, Nothing)
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
                return (RawPrgm [] [], [], Nothing)
      Nothing -> return (RawPrgm [] [], [], Nothing)
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
