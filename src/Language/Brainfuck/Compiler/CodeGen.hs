{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Brainfuck.Compiler.CodeGen where

import LLVM.AST hiding (function)
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.AST.Global
import qualified LLVM.AST.Global as Glob
import LLVM.AST.Instruction hiding (function)
import LLVM.AST.IntegerPredicate
import LLVM.IRBuilder.Module

data PrimDefs = PrimDefs
  { typeFile :: Type
  , getch :: Operand
  , putch :: Operand
  , setvbuf :: Operand
  , noBuffering :: Operand
  , stdout :: Operand
  , typeSize_t :: Type
  , size_t :: Integer -> Constant
  }

defaultDefs :: (MonadModuleBuilder m) => m PrimDefs
defaultDefs = do
  let typeSize_t = i64
      size_t n = Int 64 n
      noBuffering = ConstantOperand (Int 32 2)
  typeFile <- typedef "FILE" Nothing
  getch <- extern "getchar" [] i32
  putch <- extern "putchar" [i32] i32
  setvbuf <- extern "setvbuf" [ptr typeFile, ptr i8, i32, typeSize_t] i32
  let stdoutDef = GlobalDefinition $ globalVariableDefaults
        { name = "stdout"
        , Glob.type'= ptr typeFile
        , isConstant = True
        }
      stdout = ConstantOperand (GlobalReference (ptr (ptr typeFile)) "stdout")
  emitDefn stdoutDef
  return PrimDefs{..}
    
