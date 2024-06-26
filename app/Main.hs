{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import           CRes
import           Eval
import           Options.Applicative
import           Syntax.Ct.Desugarf   (desFiles)
import           Syntax.Ct.Formatter  (formatRootPrgm)
import           Syntax.Parsers       (mkDesCanonicalImportStr,
                                       mkRawCanonicalImportStr, parseFile,
                                       readFiles)
import           TypeCheck            (typecheckPrgm)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as H
import           Data.Maybe
import           Eval.Common          (Val (StrVal, TupleVal))
import           Syntax.Pandoc.Syntax (documentFormats, toDocument)
import           System.Directory
import           Text.Printf
import           WebDocs              (docServe)
-- import Repl (repl)

xRun :: String -> String -> IO ()
xRun prgmName function = do
  prgmName' <- mkRawCanonicalImportStr prgmName
  prgmName'' <- mkDesCanonicalImportStr prgmName
  maybeRawPrgm <- readFiles True [prgmName']
  case aux maybeRawPrgm prgmName'' of
    CErr err   -> print $ prettyCNotes err
    CRes _ resIO -> do
      returnValue <- resIO
      case returnValue of
        (0, _) -> return ()
        (i, _) -> print $ "error code " ++ show i
  where
    aux maybeRawPrgm prgmName'' = do
      rawPrgm <- maybeRawPrgm
      desPrgm <- desFiles rawPrgm
      tprgm <- typecheckPrgm desPrgm
      evalRun function prgmName'' tprgm

xBuild :: String -> String -> IO ()
xBuild prgmName function = do
  prgmName' <- mkRawCanonicalImportStr prgmName
  prgmName'' <- mkDesCanonicalImportStr prgmName
  maybeRawPrgm <- readFiles True [prgmName']
  case aux maybeRawPrgm prgmName'' of
    CErr err   -> print $ prettyCNotes err
    CRes _ resIO -> do
      returnValue <- resIO
      case returnValue of
        (TupleVal _ args, _) -> do
          let buildDir = "build"
          removePathForcibly buildDir
          createDirectoryIfMissing True buildDir
          case (fromJust $ H.lookup "name" args, fromJust $ H.lookup "contents" args) of
              (StrVal outFileName, StrVal outContents) -> do
                writeFile (buildDir ++ "/" ++ outFileName) outContents
                printf "Successfully built %s" (show prgmName)
              _ -> fail "Invalid name or contents found in result as build"
        _ -> error "Failed to build"
  where
    aux maybeRawPrgm prgmName'' = do
      rawPrgm <- maybeRawPrgm
      desPrgm <- desFiles rawPrgm
      tprgm <- typecheckPrgm desPrgm
      evalBuild function prgmName'' tprgm

xDoc :: String -> Bool -> IO ()
xDoc prgmName cached = docServe cached True prgmName

xDocument :: String -> String -> String -> IO ()
xDocument prgmName outFname format = do
  prgmName' <- mkRawCanonicalImportStr prgmName
  maybeRawPrgm <- parseFile False prgmName'
  case maybeRawPrgm of
    CErr err   -> print $ prettyCNotes err
    CRes _ (prgm, _, _) -> do
      prgm' <- toDocument format prgm
      BSL.writeFile outFname prgm'

xConvert :: String -> Maybe String -> IO ()
xConvert prgmName _outFname = do
  prgmName' <- mkRawCanonicalImportStr prgmName
  maybeRawPrgm <- parseFile False prgmName'
  case maybeRawPrgm of
    CErr err   -> print $ prettyCNotes err
    CRes _ (prgm, _fileName, _) -> do
      -- TODO Print to file if outFname
      let prgm' = formatRootPrgm prgm
      print prgmName
      print prgm'

xFmt :: String -> IO ()
xFmt prgmName = do
  prgmName' <- mkRawCanonicalImportStr prgmName
  maybeRawPrgm <- parseFile False prgmName'
  case maybeRawPrgm of
    CErr err   -> print $ prettyCNotes err
    CRes _ (prgm, _, _) -> do
      let prgm' = formatRootPrgm prgm
      writeFile prgmName prgm'

exec :: Command -> IO ()
exec (RunFile file function)          = xRun file function
exec (BuildFile file function)        = xBuild file function
exec (Doc fname cached)               = xDoc fname cached
exec (Document fname outFname format) = xDocument fname outFname format
exec (Convert fname outFname)         = xConvert fname outFname
exec (Fmt fname)                      = xFmt fname

data Command
  = BuildFile String String
  | RunFile String String
  | Doc String Bool
  | Document String String String
  | Convert String (Maybe String)
  | Fmt String

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
  <$> argument str (metavar "FILE" <> help "The file to run")
  <*> switch (long "cached" <> help "Cache results instead of reloading live (useful for serving rather than development)")

cDocument :: Parser Command
cDocument = Document
  <$> argument str (metavar "FILE" <> help "The file to run")
  <*> argument str (metavar "OUT" <> help "The output file path")
  <*> argument str (metavar "FORMAT" <> help ("The format of document. Possible formats are " ++ show documentFormats))

cConvert :: Parser Command
cConvert = Convert
  <$> argument str (metavar "FILE" <> help "The file to run")
  <*> option auto (metavar "OUT" <> value Nothing <> help "The output file path")

cFmt :: Parser Command
cFmt = Fmt
  <$> argument str (metavar "FILE" <> help "The file to run")

main :: IO ()
main = exec =<< execParser opts
  where
    opts = info (mainCommands <**> helper)
      ( fullDesc
     <> progDesc "Executes Catln options"
     <> header "Catln Compiler" )
    mainCommands = subparser (
         command "run" (info cRun (progDesc "Runs a program"))
      <> command "build" (info cBuild (progDesc "Builds a program"))
      <> command "doc" (info cDoc (progDesc "Runs webdocs for a program"))
      <> command "document" (info cDocument (progDesc "Builds a document for a program file"))
      <> command "convert" (info cConvert (progDesc "Converts code in one format to another"))
      <> command "fmt" (info cFmt (progDesc "Runs the Catln formatter for a program"))
                             )
