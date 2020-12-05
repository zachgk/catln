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

buildReplBaseEnv :: IO ReplEnv
buildReplBaseEnv = do
  rawStd <- readFiles ["std/std.ct"]
  case rawStd of
    CRes _ r -> return ([], r)
    CErr _ -> fail "Could not read std"

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
      _ -> print $ "error code " ++ show returnValue
  where
    aux = do
      prgm <- des
      tprgm <- typecheckPrgm prgm
      evalMain tprgm

processRepl :: ReplEnv -> String -> IO ReplEnv
processRepl env@(envRepl, envStd) source = do
  let res = parseRepl source
  case res of
    ReplErr err -> print err >> return env
    ReplExpr expr -> do
      -- main(IO io) = io.println(msg=expr.toString)
      let exprAsMain = RawDeclStatement $ RawDecl (DeclLHS emptyMeta (Pattern (Object emptyMeta FunctionObj "main" H.empty (H.singleton "io" (PreTyped $ singletonType (PTypeName "IO", H.empty, H.empty, H.empty), Nothing))) NoGuard)) [] (Just $ RawMethods (RawValue emptyMeta "io") [RawTupleApply emptyMeta (emptyMeta, RawValue emptyMeta "println") [RawTupleArgNamed "msg" (RawMethods expr [RawValue emptyMeta "toString"])]])
      let replRawPrgm = (["std/std.ct"], exprAsMain:envRepl)
      let des = desFiles (("replMain", replRawPrgm):envStd)
      processDes des
      return env
    ReplStatement s -> return (s:envRepl, envStd)

process :: [String] -> IO ()
process source = do
  raw <- readFiles source
  case raw of
    CErr err -> print err
    CRes _ raw' -> processDes (desFiles raw')

processFile :: String -> IO ()
processFile fname = process [fname, "std/std.ct"]

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
