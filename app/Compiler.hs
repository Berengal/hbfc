{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language NondecreasingIndentation #-}
{-# language BlockArguments #-}

module Compiler where

import BFUtils

import Language.Brainfuck.Parser
import Language.Brainfuck.Compiler
import Language.Brainfuck.Compiler.Options

import LLVM
import LLVM.Target
import LLVM.Context
import LLVM.Module
import LLVM.IRBuilder


import Options.Applicative
import qualified Data.ByteString.Char8 as BS
import System.Environment

main = execParser opts >>= doCompile

doCompile CO{..} = do
  program <- readFile inputSource
  case parse program of
    Nothing -> putStrLn "InvalidProgram"
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
