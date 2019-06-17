{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Brainfuck.Compiler where

import           Language.Brainfuck.Compiler.AdvancedIR
import           Language.Brainfuck.Compiler.CodeGen
import           Language.Brainfuck.Compiler.Optimization
import           Language.Brainfuck.Compiler.Options
import           Language.Brainfuck.Parser

import           LLVM.AST
import           LLVM.AST.Constant
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Name
import           LLVM.AST.Operand
import           LLVM.AST.Type
import           LLVM.IRBuilder
import           LLVM.IRBuilder.Module

import           Data.ByteString.Short                    as BS
import           Data.Foldable
import           Data.Sequence
import           Prelude                                  hiding (Ordering (..),
                                                           length)
import           Text.Printf

optimizationPasses None       = []
optimizationPasses Simple     = simpleOptimize
optimizationPasses Medium     = mediumOptimize
optimizationPasses Aggressive = aggressiveOptimize

compileToModule
  :: BS.ShortByteString
  -> OptimizationLevel
  -> CodeGenOptions
  -> BFProgram
  -> Module
compileToModule sourceName optimizationLevel opts program =
  builtModule{moduleSourceFileName = sourceName}
  where
    builtModule = buildModule "main" builder
    builder = mainModule opts optimizedIr
    programIr = fromBFProgram program
    optimizedIr = runOptimizationPasses (optimizationPasses optimizationLevel) programIr
