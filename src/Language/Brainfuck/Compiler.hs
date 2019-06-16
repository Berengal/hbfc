{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Brainfuck.Compiler where

import           Language.Brainfuck.Compiler.BFIR
import           Language.Brainfuck.Compiler.CodeGen
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

import           Data.ByteString.Short               as BS
import           Data.Foldable
import           Data.Sequence
import           Prelude                             hiding (Ordering (..),
                                                      length)
import           Text.Printf

type WholeProgramPass = BFSeq -> BFSeq
type PartialPass = Seq SimpleIR -> Seq SimpleIR
type PairStep = SimpleIR -> SimpleIR -> Maybe (Seq SimpleIR)
type SingleStep = SimpleIR -> Maybe (Seq SimpleIR)

pairStepTraversal :: PairStep -> WholeProgramPass
pairStepTraversal p (BFS s) = BFS $ go s
  where
    go (x :<| y :<| r) =
      case p x y of
        Nothing -> x <| y <| go r
        Just s  -> go (s >< r)
    go s = s

singleStepTraversal :: SingleStep -> WholeProgramPass
singleStepTraversal pass (BFS s) = BFS $ go s
  where
    go (x :<| r) =
      case pass x of
        Nothing -> x <| go r
        Just s  -> go (s <> r)
    go s = s

deadStartLoop :: WholeProgramPass
deadStartLoop (BFS (Loop _ :<| r)) = deadStartLoop (BFS r)
deadStartLoop x                    = x

uselessConsequentLoopPass :: WholeProgramPass
uselessConsequentLoopPass = pairStepTraversal p
  where p (Loop x) (Loop _) = Just (singleton $ Loop x)
        p _ _               = Nothing

outerLoopRedundancy :: WholeProgramPass
outerLoopRedundancy = singleStepTraversal p
  where p (Loop (BFS (a :|> loop@(Loop _)))) = Just (a |> loop)
        p _                                  = Nothing

optimize :: BFSeq -> BFSeq
optimize = outerLoopRedundancy
         . uselessConsequentLoopPass
         . deadStartLoop

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
    doOptimize | optimizationLevel == None = id
               | otherwise = optimize
    programIr = inst2Seq program
    optimizedIr = doOptimize programIr
