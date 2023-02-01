--------------------------------------------------------------------
-- |
-- Module    :  Repl
-- Copyright :  (c) Zach Kimberg 2021
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains the main code to execute a catln REPL. It
-- is currently not supported or building.
--------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Repl where


import           CRes
import           Syntax.Ct.Parser
import           Syntax.Ct.Prgm

import           Control.Monad.Trans
import           Data.List                (isPrefixOf)

import           System.Console.Haskeline
import           Utils

type ReplEnv = ([RawStatement RawExpr ()], GraphData (RawPrgm ()) String)

coreImport :: String
coreImport = "stack/core/main.ct"

buildReplBaseEnv :: IO ReplEnv
buildReplBaseEnv = do
  rawCore <- readFiles True True [coreImport]
  case rawCore of
    CRes _ r -> return ([], r)
    CErr _   -> fail "Could not read core"

parsingRepl :: ReplEnv -> String -> IO ReplEnv
parsingRepl env source = case parseRepl source of
    ReplErr err             -> print err >> return env
    ReplExpr expr           -> print expr >> return env
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

mainStatement :: RawExpr () -> RawStatementTree RawExpr ()
mainStatement = undefined
-- mainStatement expr = RawStatementTree (RawDeclStatement (RawDecl _)) []
--   where
    -- lhs = DeclLHS emptyMetaN (Pattern (ExprObject FunctionObj Nothing (RawValue (Meta (typeVal $ PTypeName "IO") Nothing emptyMetaDat) "io")) NoGuard)
    -- wrappedExpr = RawMethod (RawValue emptyMetaN "io") (RawValue emptyMetaN "println" `applyRawArgs` [(Just "msg", RawMethod expr (RawValue emptyMetaN "toString"))])


processRepl :: ReplEnv -> String -> IO ReplEnv
processRepl = undefined
-- processRepl env@(envRepl, envCore) source = do
--   let res = parseRepl source
--   case res of
--     ReplErr err -> print err >> return env
--     ReplExpr expr -> do
--       -- main(IO io) = io.println(msg=expr.toString)
--       let replRawPrgm = ([coreImport], mainStatement expr:envRepl)
--       let des = desFiles (("replMain", replRawPrgm):envCore)
--       processDes des
--       return env
--     ReplStatement s -> return (s:envRepl, envCore)


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
