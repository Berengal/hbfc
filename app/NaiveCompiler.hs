{-# LANGUAGE OverloadedStrings #-}
{-# language NondecreasingIndentation #-}
{-# language BlockArguments #-}

module NaiveCompiler where

import Language.Brainfuck.NaiveCompiler (mainModule)
import Language.Brainfuck.Parser
import BFUtils

import qualified Data.ByteString.Char8 as BS

import LLVM
import LLVM.Target
import LLVM.Context
import LLVM.Module

import LLVM.IRBuilder

import System.Environment

main = do
  args <- getArgs
  program <- if null args then getContents
             else readFile (head args)
  case parse program of
    Nothing -> putStrLn "Invalid program"
    Just is -> compileToLLVM is >>= BS.putStrLn

compileToLLVM is =
  let astModule = buildModule "main" (mainModule is)
  in do
  initializeNativeTarget
  withContext \ctx -> do
    withModuleFromAST ctx astModule \cppModule -> do
      moduleLLVMAssembly cppModule
