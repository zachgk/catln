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

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict  as H
import           Data.Maybe
import           Eval.Common          (Val (StrVal, TupleVal))
import           Syntax.Pandoc.Syntax (documentFormats, toDocument)
import           System.Directory
import           Text.Printf
import           WebDocs              (docApi, docServe)
-- import Repl (repl)

xRun :: String -> String -> CResT IO ()
xRun prgmName function = do
  prgmName' <- lift $ mkRawCanonicalImportStr prgmName
  prgmName'' <- lift $ mkDesCanonicalImportStr prgmName
  rawPrgm <- readFiles True [prgmName']
  desPrgm <- desFiles rawPrgm
  tprgm <- asCResT $ typecheckPrgm desPrgm
  returnValue <- evalRun function prgmName'' tprgm
  case returnValue of
    (0, _) -> return ()
    (i, _) -> fail $ "error code " ++ show i

xBuild :: String -> String -> CResT IO ()
xBuild prgmName function = do
  prgmName' <- lift $ mkRawCanonicalImportStr prgmName
  prgmName'' <- lift $ mkDesCanonicalImportStr prgmName
  rawPrgm <- readFiles True [prgmName']
  desPrgm <- desFiles rawPrgm
  tprgm <- asCResT $ typecheckPrgm desPrgm
  returnValue <- evalBuild function prgmName'' tprgm
  case returnValue of
    (TupleVal _ args, _) -> do
      let buildDir = "build"
      lift $ removePathForcibly buildDir
      lift $ createDirectoryIfMissing True buildDir
      case (fromJust $ H.lookup "name" args, fromJust $ H.lookup "contents" args) of
          (StrVal outFileName, StrVal outContents) -> do
            lift $ writeFile (buildDir ++ "/" ++ outFileName) outContents
            lift $ putStrLn $ printf "Successfully built %s" (show prgmName)
          _ -> fail "Invalid name or contents found in result as build"
    _ -> fail "Failed to build"

xDoc :: String -> Bool -> Bool -> CResT IO ()
xDoc prgmName cached apiOnly = if apiOnly
  then lift $ docApi cached True prgmName
  else lift $ docServe cached True prgmName

xDocument :: String -> String -> String -> CResT IO ()
xDocument prgmName outFname format = do
  prgmName' <- lift $ mkRawCanonicalImportStr prgmName
  (prgm, _, _) <- parseFile False prgmName'
  prgm' <- lift $ toDocument format prgm
  lift $ BSL.writeFile outFname prgm'

xConvert :: String -> Maybe String -> CResT IO ()
xConvert prgmName _outFname = do
  prgmName' <- lift $ mkRawCanonicalImportStr prgmName
  (prgm, _fileName, _) <- parseFile False prgmName'
  -- TODO Print to file if outFname
  let prgm' = formatRootPrgm prgm
  lift $ print prgmName
  lift $ print prgm'

xFmt :: String -> CResT IO ()
xFmt prgmName = do
  prgmName' <- lift $ mkRawCanonicalImportStr prgmName
  (prgm, _, _) <- parseFile False prgmName'
  let prgm' = formatRootPrgm prgm
  lift $ writeFile prgmName prgm'

exec :: Command -> CResT IO ()
exec (RunFile file function)          = xRun file function
exec (BuildFile file function)        = xBuild file function
exec (Doc fname cached apiOnly)       = xDoc fname cached apiOnly
exec (Document fname outFname format) = xDocument fname outFname format
exec (Convert fname outFname)         = xConvert fname outFname
exec (Fmt fname)                      = xFmt fname

data Command
  = BuildFile String String
  | RunFile String String
  | Doc String Bool Bool
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
  <*> switch (long "api" <> help "Serve only the API rather than the pages too")

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
      <> command "document" (info cDocument (progDesc "Builds a document for a program file"))
      <> command "convert" (info cConvert (progDesc "Converts code in one format to another"))
      <> command "fmt" (info cFmt (progDesc "Runs the Catln formatter for a program"))
                             )
