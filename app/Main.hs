{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as H

import           Desugarf                 (desFiles)
-- import           Emit                     (codegen, initModule)
import           Eval
import           Syntax.Types
import           Syntax.Prgm
import           Syntax
import           CRes
import           Parser
import           Parser.Syntax
import           TypeCheck                (typecheckPrgm)

import           Control.Monad
import           Control.Monad.Trans
import           Data.List                (isPrefixOf)

import           System.Console.Haskeline
import           System.Environment

type ReplEnv = ([RawStatement PreTyped], RawPrgms PreTyped)

coreImport :: String
coreImport = "stack/core/main.ct"

buildReplBaseEnv :: IO ReplEnv
buildReplBaseEnv = do
  rawCore <- readFiles True [coreImport]
  case rawCore of
    CRes _ r -> return ([], r)
    CErr _ -> fail "Could not read core"

parsingRepl :: ReplEnv -> String -> IO ReplEnv
parsingRepl env source = case parseRepl source of
    ReplErr err   -> print err >> return env
    ReplExpr expr -> print expr >> return env
    ReplStatement statement -> print statement >> return env

-- genRepl :: ReplEnv -> String -> IO ReplEnv
-- genRepl env source = do
--   let res = parseRepl source
--   case res of
--     ReplErr err -> print err >> return env
--     ReplExpr _ -> print ("Can not generate expression" :: String) >> return env
--     ReplStatement statement -> case typecheckPrgm $ desStatements [statement] of
--         TypeCheckResE err    -> print ("type check err: " ++ show err) >> return env
--         TypeCheckResult _ tprgm -> codegen initModule tprgm >> return env

processDes :: CRes DesPrgm -> IO ()
processDes des = case aux of
  CErr err   -> print err
  CRes _ resIO -> do
    returnValue <- resIO
    case returnValue of
      (0, _) -> return ()
      (i, _) -> print $ "error code " ++ show i
  where
    aux = do
      prgm <- des
      tprgm <- typecheckPrgm prgm
      evalMain tprgm

mainStatement :: RawExpr PreTyped -> RawStatement PreTyped
mainStatement expr = RawDeclStatement $ RawDecl lhs [] (Just wrappedExpr)
  where
    lhs = DeclLHS emptyMetaN (Pattern (Object emptyMetaN FunctionObj "main" H.empty (H.singleton "io" (PreTyped (singletonType (PartialType (PTypeName "IO") H.empty H.empty H.empty PtArgAny)) Nothing, Nothing))) NoGuard)
    wrappedExpr = RawMethods (RawValue emptyMetaN "io") [RawTupleApply emptyMetaN (emptyMetaN, RawValue emptyMetaN "println") [RawTupleArgNamed "msg" (RawMethods expr [RawValue emptyMetaN "toString"])]]

processRepl :: ReplEnv -> String -> IO ReplEnv
processRepl env@(envRepl, envCore) source = do
  let res = parseRepl source
  case res of
    ReplErr err -> print err >> return env
    ReplExpr expr -> do
      -- main(IO io) = io.println(msg=expr.toString)
      let replRawPrgm = ([coreImport], mainStatement expr:envRepl)
      let des = desFiles (("replMain", replRawPrgm):envCore)
      processDes des
      return env
    ReplStatement s -> return (s:envRepl, envCore)

process :: [String] -> IO ()
process source = do
  raw <- readFiles True source
  case raw of
    CErr err -> print err
    CRes _ raw' -> processDes (desFiles raw')

processFile :: String -> IO ()
processFile fname = process [fname, coreImport]

repl :: IO ()
repl = do
  replBaseEnv <- buildReplBaseEnv
  runInputT defaultSettings (loop replBaseEnv)
  where loop env = do
          minput <- getInputLine "eval> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input | ":q" `isPrefixOf` input -> outputStrLn "Goodbye."
            Just input -> do
              env' <- case input of
                _ | ":p " `isPrefixOf` input -> lift $ parsingRepl env (drop 3 input)
                -- _ | ":g " `isPrefixOf` input -> lift $ genRepl env (drop 3 input)
                _ -> lift $ processRepl env input
              loop env'

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> void (processFile fname)
    _       -> putStr "Unknown arguments"
