module Interpreter where

import Language.Brainfuck.Parser
import Language.Brainfuck.Interpreter

import System.Environment
import System.IO


-- Read and execute first arg, if no args then execute first line using rest of
-- stdin as input
main :: IO ()
main = do
  args <- getArgs
  prog <- if null args then
            getLine
          else
            readFile (head args)
  case parse prog of
    Nothing -> putStrLn "Invalid Program"
    Just p  -> do
      hSetBuffering stdout NoBuffering
      run p
