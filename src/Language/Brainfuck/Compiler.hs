{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# language BlockArguments #-}
module Language.Brainfuck.Compiler where

import Language.Brainfuck.Parser
import Language.Brainfuck.Compiler.Options
import Language.Brainfuck.Compiler.CodeGen
import Language.Brainfuck.Compiler.BFIR

import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate
import LLVM.AST.Name
import LLVM.AST.Operand
import LLVM.AST.Type

import Prelude hiding (length, Ordering(..))
import Data.Sequence
import Data.Foldable
import Data.ByteString.Short as BS
import Text.Printf

type WholeProgramPass = BFSeq -> BFSeq
type PartialPass = Seq BFIR -> Seq BFIR
type PairStep = BFIR -> BFIR -> Maybe (Seq BFIR)

stepTraversal :: PairStep -> WholeProgramPass
stepTraversal p (BFS s) = BFS $ go s
  where
    go (x :<| y :<| r) = case p x y of
                           Nothing -> x <| y <| go r
                           Just s -> go (s >< r)
    go s = s

deadStartLoop :: WholeProgramPass
deadStartLoop (BFS (Loop _ :<| r)) = deadStartLoop (BFS r)
deadStartLoop x = x 

uselessConsequentLoopPass = stepTraversal p
  where p (Loop x) (Loop _) = Just (singleton $ Loop x)
        p _ _ = Nothing

optimize :: BFSeq -> BFSeq
optimize = uselessConsequentLoopPass . deadStartLoop

compileToModule :: BS.ShortByteString -> CodeGenOptions -> [BFInst] -> Module
compileToModule sourceName opts program = builtModule{moduleSourceFileName = sourceName}
  where
    builtModule = buildModule "main" builder
    builder = mainModule opts (inst2Seq program)
