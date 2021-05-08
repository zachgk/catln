
module Main where

import           Desugarf                 (desFiles)
import           Eval
import           CRes
import           Parser
import           TypeCheck                (typecheckPrgm)
import Options.Applicative

import           Data.Semigroup ((<>))
import Eval.Common (Val(TupleVal, StrVal))
import qualified Data.HashMap.Strict as H
import Data.Maybe
import WebDocs (docServe)
import System.Directory
import Text.Printf
-- import Repl (repl)

xRun :: String -> IO ()
xRun prgmName = do
  maybeRawPrgm <- readFiles True [prgmName]
  case aux maybeRawPrgm of
    CErr err   -> print err
    CRes _ resIO -> do
      returnValue <- resIO
      case returnValue of
        (0, _) -> return ()
        (i, _) -> print $ "error code " ++ show i
  where
    aux maybeRawPrgm = do
      rawPrgm <- maybeRawPrgm
      desPrgm <- desFiles rawPrgm
      tprgm <- typecheckPrgm desPrgm
      evalMainx prgmName tprgm

xBuild :: String -> IO ()
xBuild prgmName = do
  maybeRawPrgm <- readFiles True [prgmName]
  case aux maybeRawPrgm of
    CErr err   -> print err
    CRes _ resIO -> do
      returnValue <- resIO
      case returnValue of
        (TupleVal _ args, _) -> do
          let buildDir = "build"
          removePathForcibly buildDir
          createDirectoryIfMissing True buildDir
          let (StrVal outFileName) = fromJust $ H.lookup "name" args
          let (StrVal outContents) = fromJust $ H.lookup "contents" args
          writeFile (buildDir ++ "/" ++ outFileName) outContents
          printf "Successfully built %s" (show prgmName)
        _ -> error "Failed to build"
  where
    aux maybeRawPrgm = do
      rawPrgm <- maybeRawPrgm
      desPrgm <- desFiles rawPrgm
      tprgm <- typecheckPrgm desPrgm
      evalMain prgmName tprgm

xDoc :: String -> Bool -> IO ()
xDoc prgmName cached = docServe cached True prgmName

exec :: Command -> IO ()
exec (RunFile fname) = xRun fname
exec (BuildFile fname) = xBuild fname
exec (Doc fname cached) = xDoc fname cached

data Command
  = BuildFile String
  | RunFile String
  | Doc String Bool

cRun :: Parser Command
cRun = RunFile <$> argument str (metavar "FILE" <> help "The file to run")

cBuild :: Parser Command
cBuild = BuildFile <$> argument str (metavar "FILE" <> help "The file to build")

cDoc :: Parser Command
cDoc = Doc
  <$> argument str (metavar "FILE" <> help "The file to run")
  <*> switch (long "cached" <> help "Cache results instead of reloading live (useful for serving rather than development)")

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
                             )
