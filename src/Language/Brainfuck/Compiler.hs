{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Brainfuck.Compiler where

import           Language.Brainfuck.Compiler.AdvancedIR
import           Language.Brainfuck.Compiler.CodeGen
import           Language.Brainfuck.Compiler.Optimization
import           Language.Brainfuck.Compiler.Optimization.Passes
import           Language.Brainfuck.Compiler.Options
import           Language.Brainfuck.Parser

import           LLVM.AST
import           LLVM.IRBuilder

import           Data.ByteString.Short                    as BS
import           Prelude                                  hiding (Ordering (..),
                                                           length)

optimizationPasses :: OptimizationLevel -> [OptimizationPass]
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
