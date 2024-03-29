{-# LANGUAGE NamedFieldPuns #-}

module Compiler where

import           BFUtils

import qualified Language.Brainfuck.Compiler         as C
import           Language.Brainfuck.Compiler.Options
import           Language.Brainfuck.Parser

import           LLVM
import qualified LLVM.AST                            as AST
import           LLVM.Context
import           LLVM.PassManager
import           LLVM.Target

import           Control.Monad
import qualified Data.ByteString.Char8               as BS
import qualified Data.ByteString.Short               as BSS
import           Data.Maybe
import           Data.String
import           Options.Applicative
import           System.Exit
import           System.IO
import           System.Process

main :: IO ()
main = do
  options@CO{..} <- execParser opts
  let isWriteExecutableToStdout = outputDestination == Nothing && outputFormat == Executable
      newOutputDestination | isWriteExecutableToStdout = Just "a.out"
                           | otherwise = outputDestination
  when isWriteExecutableToStdout $ do
    hPutStrLn stderr "Warning: Writing executable to stdout not supported, will write to 'a.out' instead."
  doCompile options{outputDestination=newOutputDestination}

doCompile :: CompilerOptions -> IO ()
doCompile options@CO{..} = do
  source <- readFile inputSource
  case parse source of
    Nothing -> hPutStrLn stderr "Invalid program"
    Just program -> do
      let astModule = compileToModule options program

      withContext $ \ctx -> do
      withTargetMachineOptions optimizationLevel $ \target -> do
      triple <- getProcessTargetTriple
      withModuleFromAST ctx astModule{AST.moduleTargetTriple=Just triple} $ \mod -> do

      llvmTransformPass target options mod
      let outputType | outputFormat == Executable = Object
                     | otherwise = outputFormat
          objectPath = getTargetName inputSource Object
          outputPath | outputFormat == Executable = Just objectPath
                     | otherwise = outputDestination
      outputBytes <- createOutput outputType mod target
      writeOutput outputPath outputBytes

      if outputFormat == Executable
        then do exit <- rawSystem "cc" [ "-o"
                                       , fromMaybe (error "Bug in compiler: calling cc without destination.") outputDestination
                                       , objectPath
                                       ]
                exitWith exit
        else return ()

compileToModule :: CompilerOptions -> BFProgram -> AST.Module
compileToModule options = C.compileToModule inputName optLevel codeGenOpts
  where inputName   = fromString . inputSource $ options
        codeGenOpts = codeGenOptions options
        optLevel    = optimizationLevel options

llvmTransformPass :: TargetMachine -> CompilerOptions -> Module -> IO ()
llvmTransformPass target options mod =
  withPassManager spec $ \manager -> do

  runPassManager manager mod
  when (optLevel >= Medium) $ do -- Note: [Optimizing twice]
    () <$ runPassManager manager mod

  where
    optLevel = llvmOptimization options
    spec     = defaultCuratedPassSetSpec
      { optLevel = Just $ case optLevel of
          None -> 0; Simple -> 1; Medium -> 2; Aggressive -> 3;
      , targetMachine = Just target
      }

{- [Optimizing twice]
   The curated optimization pass set is tuned to C/C++ programs and in
   this case benefits from running twice.
-}

createOutput
  :: OutputFormat -> Module -> TargetMachine -> IO BS.ByteString
createOutput IRAssembly mod _          = moduleLLVMAssembly mod
createOutput IRBitCode mod  _          = moduleBitcode mod
createOutput NativeAssembly mod target = moduleTargetAssembly target mod
createOutput Object mod target         = moduleObject target mod

writeOutput :: Maybe FilePath -> BS.ByteString -> IO ()
writeOutput outputDestination bytes = withOutputHandle (flip BS.hPutStr bytes)
  where
    withOutputHandle action = case outputDestination of
      Nothing   -> action stdout
      Just path -> withFile path WriteMode action
