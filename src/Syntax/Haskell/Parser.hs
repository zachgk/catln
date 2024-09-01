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
import           Data.List
import           GHC.Data.Bag
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Driver.Config.Parser (initParserOpts)
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Fingerprint
import           GHC.Parser
import           GHC.Parser.Header
import           GHC.Parser.Lexer
import           GHC.Platform
import           GHC.Settings
import           GHC.Settings.Config
import           GHC.Types.Error
import           GHC.Types.SourceError
import           GHC.Types.SrcLoc
import qualified GHC.Types.SrcLoc         as SrcLoc
import           GHC.Utils.Outputable
import           GHC.Utils.Panic
import           Semantics.Prgm
import           Syntax.Ct.Prgm
import           Syntax.Haskell.Convert   (convertModule)
import           System.Directory         (doesFileExist)
import           Text.Megaparsec          (mkPos)
import           Text.Megaparsec.Pos      (SourcePos (SourcePos))
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

-- From https://www.stackage.org/haddock/lts-21.25/ghc-lib-parser-8.10.7.20220219/Parser.html
runParser :: DynFlags -> String -> String -> P a -> ParseResult a
runParser flags filename str parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState (initParserOpts flags) buffer location

hsParser :: ImportParser
hsParser imp = case rawExprAppliedArgs imp of
  (RawObjArr{roaArr=Just (Just (RawCExpr _ (CStr filename)), _)}:_impArgs) -> do
    isFile <- doesFileExist filename
    if isFile
      then do
        str <- readFile filename
        maybeFlags <-
                parsePragmasIntoDynFlags
                  filename str
        case maybeFlags of
          Nothing -> fail $ printf "Failed to read flags from %s" filename
          Just flags -> do
            let parsed = runParser flags filename str parseModule
            case parsed of
              POk _ v -> return (convertModule flags $ unLoc v, [])
              PFailed pstate ->
                let realSpan = psRealSpan $ last_loc pstate
                    errMsg = printErrorBag flags $ fmap (unDecorated . diagnosticMessage . errMsgDiagnostic) $ getMessages $ snd $ getPsMessages pstate
                    lnStart = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
                    colStart = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
                    lnEnd = srcLocLine $ SrcLoc.realSrcSpanEnd realSpan
                    colEnd = srcLocCol $ SrcLoc.realSrcSpanEnd realSpan
                    _srcPos = Just (SourcePos filename (mkPos lnStart) (mkPos colStart), SourcePos filename (mkPos lnEnd) (mkPos colEnd), filename)
                in fail errMsg
      else return (([], []), [])
  _ -> undefined

  where
    printErrorBag flags bag = joinLines . map (showSDoc flags . ppr) $ bagToList bag
    joinLines = intercalate "\n"
