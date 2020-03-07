{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Desugarf                 (desDecl, desPrgm)
import           Emit                     (codegen, initModule)
import           Eval
import           Parser                   (parseFile, parseRepl)
import           Syntax
import           TypeCheck                (typecheckPrgm)

import           Control.Monad
import           Control.Monad.Trans
import           Data.List                (isPrefixOf)

import           System.Console.Haskeline
import           System.Environment

parsingRepl :: Env -> String -> IO Env
parsingRepl env source = case parseRepl source of
    ReplErr err   -> print err >> return env
    ReplExpr expr -> print expr >> return env
    ReplDecl decl -> print decl >> return env

genRepl :: Env -> String -> IO Env
genRepl env source = do
  let res = parseRepl source
  case res of
    ReplErr err -> print err >> return env
    ReplExpr _ -> print ("Can not generate expression" :: String) >> return env
    ReplDecl decl -> case typecheckPrgm $ desPrgm [decl] of
        Left err    -> print ("type check err: " ++ show err) >> return env
        Right tprgm -> codegen initModule tprgm >> return env

-- processRepl :: Env -> String -> IO Env
-- processRepl env source = do
--   let res = parseRepl source
--   case res of
--     ReplErr err -> print err >> return env
--     ReplExpr expr -> print (evalExpr env expr) >> return env
--     ReplDecl decl -> case addDecl env (head $ desDecl decl) of
--       Left err'  -> print err' >> return env
--       Right env' -> return env'

process :: String -> IO ()
process = undefined
-- process source = do
--   let res = parseFile source
--   case res of
--     Left err   -> print err
--     Right prgm -> print (evalPrgm (desPrgm prgm))

processFile :: String -> IO ()
processFile fname = readFile fname >>= process

repl :: IO ()
repl = undefined
-- repl = runInputT defaultSettings (loop baseEnv)
--   where loop env = do
--           minput <- getInputLine "eval> "
--           case minput of
--             Nothing -> outputStrLn "Goodbye."
--             Just input -> do
--               env' <- case input of
--                 _ | ":p " `isPrefixOf` input -> lift $ parsingRepl env (drop 3 input)
--                 _ | ":g " `isPrefixOf` input -> lift $ genRepl env (drop 3 input)
--                 _ -> lift $ processRepl env input
--               loop env'

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> void (processFile fname)
    _       -> putStr "Unknown arguments"
