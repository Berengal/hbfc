{-# LANGUAGE OverloadedStrings #-}
{-# language NondecreasingIndentation #-}
{-# language BlockArguments #-}

module Compiler where

import Language.Brainfuck.Parser
import Language.Brainfuck.Compiler
import BFUtils

import qualified Data.ByteString.Char8 as BS

import System.Environment

main = do
  args <- getArgs
  program <- if null args then getContents
             else readFile (head args)
  case parse program of
    Nothing -> putStrLn "Invalid program"
    Just is -> compileToLLVM is >>= BS.putStrLn

compileToLLVM :: [BFInst] -> IO BS.ByteString
compileToLLVM is =
  let intermediate = inst2Seq is
      astModule = buildModule "main" (mainModule intermediate)
  in do
  initializeNativeTarget
  withContext \ctx -> do
    withModuleFromAST ctx astModule \cppModule -> do
      moduleLLVMAssembly cppModule
