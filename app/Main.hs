
module Main where

import           Eval
import           Parser                   (parseFile, parseRepl)
import           Desugarf (desPrgm, desDecl)
import           Syntax

import           Control.Monad
import           Control.Monad.Trans

import           System.Console.Haskeline
import           System.Environment

parsingRepl :: IO ()
parsingRepl = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "parse> "
          case minput of
            Nothing    -> outputStrLn "Goodbye."
            Just input -> liftIO (mapM_ print $ parseFile input) >> loop

processRepl :: Env -> String -> IO Env
processRepl env source = do
  let res = parseRepl source
  case res of
    ReplErr err -> print err >> return env
    ReplExpr expr -> print (evalExpr env expr) >> return env
    ReplDecl decl -> case addDecl env (head $ desDecl decl) of
      Left err'  -> print err' >> return env
      Right env' -> return env'

process :: String -> IO ()
process source = do
  let res = parseFile source
  case res of
    Left err -> print err
    Right prgm -> print (evalPrgm (desPrgm prgm))

processFile :: String -> IO ()
processFile fname = readFile fname >>= process

repl :: IO ()
repl = runInputT defaultSettings (loop baseEnv)
  where loop env = do
          minput <- getInputLine "eval> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
              env' <- lift $ processRepl env input
              loop env'

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    ["-p"]  -> parsingRepl
    [fname] -> void (processFile fname)
    _ -> putStr "Unknown arguments"
