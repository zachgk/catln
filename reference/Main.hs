--------------------------------------------------------------------
-- |
-- Module    :  Main
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Syntax
import Parser
import Emit.Codegen
import Emit

import Control.Monad.Trans

import qualified Data.ByteString as BBS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Short as SBS

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule (SBS.toShort $ BSU.fromString "my cool jit")

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right (Prgm _ _ ex) -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

parseProcess :: String -> IO ()
parseProcess line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right (Prgm _ _ ex) -> mapM_ print ex

parseRepl :: IO ()
parseRepl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ parseProcess input) >> loop

parseFile :: String -> IO ()
parseFile fname = readFile fname >>= parseProcess

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop :: AST.Module -> InputT IO ()
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- lift $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
