
module Main where

import           Desugarf                 (desFiles)
import           Eval
import           CRes
import           Parser
import           Parser.Syntax
import           TypeCheck                (typecheckPrgm)
import Options.Applicative

import           Control.Monad
import           Data.Semigroup ((<>))
-- import Repl (repl)

processDes :: String -> CRes PPrgmGraphData -> IO ()
processDes prgmName maybeRawPrgm = case aux of
  CErr err   -> print err
  CRes _ resIO -> do
    returnValue <- resIO
    case returnValue of
      (0, _) -> return ()
      (i, _) -> print $ "error code " ++ show i
  where
    aux = do
      rawPrgm <- maybeRawPrgm
      desPrgm <- desFiles rawPrgm
      tprgm <- typecheckPrgm desPrgm
      evalMainx prgmName tprgm

process :: String -> IO ()
process source = do
  raw <- readFiles True [source]
  processDes source raw

runFile :: Parser String
runFile = argument str (metavar "FILE" <> help "The file to run")

exec :: String -> IO ()
exec fname = do
  void (process fname)

main :: IO ()
main = exec =<< execParser opts
  where
    opts = info (runFile <**> helper)
      ( fullDesc
     <> progDesc "Executes Catln options"
     <> header "Catln Compiler" )
