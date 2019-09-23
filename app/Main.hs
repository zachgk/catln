
module Main where

import           Eval
import           Parser                   (parseFile, parseRepl)
import           Syntax

import           Control.Monad.Trans

import           System.Console.Haskeline
import           System.Environment
import           System.IO

parsingRepl :: IO ()
parsingRepl = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "parse> "
          case minput of
            Nothing    -> outputStrLn "Goodbye."
            Just input -> (liftIO $ mapM_ print $ parseFile input) >> loop

process :: Env -> String -> IO (Env)
process env source = do
  let res = parseRepl source
  case res of
    ReplErr err -> print err >> return env
    ReplExpr expr -> print (evalExpr env expr) >> return env
    ReplDecl decl -> case addDecl env decl of
      Left err'  -> print err' >> return env
      Right env' -> return env'

processFile = undefined

repl :: IO ()
repl = runInputT defaultSettings (loop baseEnv)
  where loop env = do
          minput <- getInputLine "eval> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
              env' <- lift $ process env input
              loop env'

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    ["-p"]  -> parsingRepl
    [fname] -> processFile fname >> return ()
