{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language NondecreasingIndentation #-}
{-# language BlockArguments #-}

module Compiler where

import BFUtils

import Language.Brainfuck.Parser
import qualified Language.Brainfuck.Compiler as C
import Language.Brainfuck.Compiler.Options

import LLVM
import LLVM.Target
import LLVM.Context
import LLVM.Module
import LLVM.IRBuilder
import LLVM.PassManager
import qualified LLVM.AST as AST

import Control.Monad
import Options.Applicative
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO

main = execParser opts >>= doCompile

doCompile options@CO{..} = do
  source <- readFile inputSource
  case parse source of
    Nothing -> putStrLn "Invalid program"
    Just program -> do
      let astModule = compileToModule options program
      
      withContext \ctx -> do
      withTargetMachineOptions optimizationLevel \target -> do
      triple <- getProcessTargetTriple
      withModuleFromAST ctx astModule{AST.moduleTargetTriple=Just triple} \mod -> do
        
      llvmTransformPass target options mod
      outputBytes <- createOutput outputFormat mod target
      writeOutput options outputBytes

compileToModule :: CompilerOptions -> [BFInst] -> AST.Module
compileToModule options = C.compileToModule inputName codeGenOpts
  where inputName = BSS.toShort . BS.pack . inputSource $ options
        codeGenOpts = codeGenOptions options

llvmTransformPass :: TargetMachine -> CompilerOptions -> Module -> IO ()
llvmTransformPass target options mod =
  withPassManager spec \manager -> do
  runPassManager manager mod
  return ()
  where
    spec = defaultCuratedPassSetSpec
      { optLevel = Just case optimizationLevel options of
          {None -> 0; Simple -> 1; Medium -> 2; Aggressive -> 3;}
      , targetMachine = Just target
      }

createOutput
  :: OutputFormat -> Module -> TargetMachine -> IO BS.ByteString
createOutput IRAssembly mod _= moduleLLVMAssembly mod
createOutput IRBitCode mod  _ = moduleBitcode mod
createOutput NativeAssembly mod target = moduleTargetAssembly target mod
createOutput Object mod target = moduleObject target mod

writeOutput :: CompilerOptions -> BS.ByteString -> IO ()
writeOutput CO{outputDestination} bytes = withOutputHandle (flip BS.hPutStr bytes)
  where withOutputHandle action = case outputDestination of
          Nothing   -> action stdout
          Just path -> withFile path WriteMode action

        
          
