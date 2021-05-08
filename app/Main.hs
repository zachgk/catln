{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Desugarf                 (desFiles)
import           Eval
import           CRes
import           Parser
import           Parser.Syntax
import           TypeCheck                (typecheckPrgm)

import           Control.Monad
import           System.Environment
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- []      -> repl
    [fname] -> void (process fname)
    _       -> putStr "Unknown arguments"
