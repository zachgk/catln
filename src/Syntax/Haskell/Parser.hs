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
import           Bag
import           Config
import           CRes
import           Data.List
import           DynFlags
import           FastString
import           GHC.Fingerprint
import           GHC.Hs              (GhcPs, HsModule)
import           GHC.Platform
import           HeaderInfo
import           HscTypes
import           Lexer
import           Outputable
import           Panic
import           Parser              (parseModule)
import           SrcLoc
import           StringBuffer
import           Syntax.Ct.Prgm      (RawPrgm)
import           Text.Megaparsec     (mkPos)
import           Text.Megaparsec.Pos (SourcePos (SourcePos))
import           Text.Printf
import           ToolSettings

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

fakeSettings :: Settings
fakeSettings = Settings {
  sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  , sPlatformConstants=platformConstants
  }
  where
    fileSettings = FileSettings {
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
        platformMisc_integerLibraryType=IntegerSimple
       }

    ghcNameVersion = GhcNameVersion{
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform = Platform{
         platformWordSize=PW8
       , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
       , platformUnregisterised=True
      }

    platformConstants = PlatformConstants {
         pc_DYNAMIC_BY_DEFAULT=False
       , pc_WORD_SIZE=8
       , pc_STD_HDR_SIZE=1
       , pc_TAG_BITS=3
       , pc_BLOCKS_PER_MBLOCK=252
       , pc_BLOCK_SIZE=4096
       , pc_MIN_PAYLOAD_SIZE=1
       , pc_MAX_Real_Vanilla_REG=6
       , pc_MAX_Vanilla_REG=10
       , pc_MAX_Real_Long_REG=0
       }

-- From https://blog.shaynefletcher.org/2019/06/have-ghc-parsing-respect-dynamic-pragmas.html
-- Based on https://github.com/digital-asset/ghc-lib/blob/master/examples/ghc-lib-test-mini-hlint/src/Main.hs
parsePragmasIntoDynFlags :: FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags filepath str =
  catchErrors $ do
    let opts = getOptions fakeFlags (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma fakeFlags opts
    return $ Just flags
  where
    fakeFlags = defaultDynFlags fakeSettings fakeLlvmConfig
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = do putStrLn $ "error : " ++ show e; return Nothing

-- From https://www.stackage.org/haddock/lts-18.28/ghc-lib-parser-8.10.7.20220219/Parser.html
runParser :: DynFlags -> String -> String -> P a -> ParseResult a
runParser flags filename str parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

convert :: DynFlags -> HsModule GhcPs -> RawPrgm ()
convert flags p = error $ printf "Convert:\n%s" (showSDoc flags $ ppr p)

hsParser :: String -> IO (CRes (RawPrgm ()))
hsParser filename = do
  str <- readFile filename
  maybeFlags <-
          parsePragmasIntoDynFlags
             filename str
  case maybeFlags of
    Nothing -> return $ CErr [MkCNote $ GenCErr Nothing $ printf "Failed to read flags from %s" filename]
    Just flags -> do
      let parsed = runParser flags filename str parseModule
      return $ case parsed of
        POk _ v -> pure $ convert flags $ unLoc v
        PFailed pstate ->
          let realSpan = last_loc pstate
              errMsg = printErrorBag $ snd $ messages pstate flags
              lnStart = srcLocLine $ SrcLoc.realSrcSpanStart realSpan
              colStart = srcLocCol $ SrcLoc.realSrcSpanStart realSpan
              lnEnd = srcLocLine $ SrcLoc.realSrcSpanEnd realSpan
              colEnd = srcLocCol $ SrcLoc.realSrcSpanEnd realSpan
          in CErr [MkCNote $ GenCErr (Just (SourcePos filename (mkPos lnStart) (mkPos colStart), SourcePos filename (mkPos lnEnd) (mkPos colEnd), filename)) errMsg]

  where
    printErrorBag bag = joinLines . map show $ bagToList bag
    joinLines = intercalate "\n"
