{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as H

import           Desugarf                 (desFiles, desPrgm)
-- import           Emit                     (codegen, initModule)
import           Eval
import           Syntax.Types
import           Syntax.Prgm
import           CRes
import           Parser
import           Parser.Syntax
import           TypeCheck.Common  (TypeCheckResult(TypeCheckResult, TypeCheckResE))
import           TypeCheck                (typecheckPrgm)

import           Control.Monad
import           Control.Monad.Trans
import           Data.List                (isPrefixOf)

import           System.Console.Haskeline
import           System.Environment

type ReplEnv = [RawStatement PreTyped]

replBaseEnv :: ReplEnv
replBaseEnv = []

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
processDes des = case des of
  CErr err   -> print err
  CRes _ prgm ->
    case typecheckPrgm prgm of
      TypeCheckResE err -> print err
      TypeCheckResult _ tprgm ->
        case evalMain tprgm of
          CErr err -> print err
          CRes _ resIO -> do
            returnValue <- resIO
            case returnValue of
              0 -> return ()
              _ -> print $ "error code " ++ show returnValue

processRepl :: ReplEnv -> String -> IO ReplEnv
processRepl env source = do
  let res = parseRepl source
  case res of
    ReplErr err -> print err >> return env
    ReplExpr expr -> do
      -- main(IO io) = io.println(msg=expr.toString)
      let exprAsMain = RawDeclStatement $ RawDecl (DeclLHS emptyMeta (Pattern (Object emptyMeta FunctionObj "main" H.empty (H.singleton "io" (PreTyped $ singletonType (PTypeName "IO", H.empty, H.empty), Nothing))) NoGuard)) [] (Just $ RawMethods (RawValue emptyMeta "io") [RawTupleApply emptyMeta (emptyMeta, RawValue emptyMeta "println") [RawTupleArgNamed "msg" (RawMethods expr [RawValue emptyMeta "toString"])]])
      let rawPrgm = (["std/std.ct"], exprAsMain:env)
      des <- desPrgm rawPrgm
      processDes des
      return env
    ReplStatement s -> return (s:env)

process :: [String] -> IO ()
process source = do
  des <- desFiles source
  processDes des

processFile :: String -> IO ()
processFile fname = process [fname, "std/std.ct"]

repl :: IO ()
repl = runInputT defaultSettings (loop replBaseEnv)
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
