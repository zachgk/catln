
module Main where

import Parser (parseFile)

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

parseRepl :: IO ()
parseRepl = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "parse> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> (liftIO $ mapM_ print $ parseFile input) >> loop

processFile = undefined

repl = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    ["-p"]      -> parseRepl
    [fname] -> processFile fname >> return ()
