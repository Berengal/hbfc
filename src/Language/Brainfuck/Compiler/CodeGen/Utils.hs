module Language.Brainfuck.Compiler.CodeGen.Utils where

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Constant
import LLVM.AST.Operand
import LLVM.AST.Visibility
import LLVM.AST.Type

import LLVM.IRBuilder.Module

intOp bits n = ConstantOperand (Int bits n)

int64 = intOp 64
int32 = intOp 32
int8 = intOp 8

global :: MonadModuleBuilder m
       => Visibility
       -> Name
       -> Type
       -> Constant
       -> m Operand
global visibility name ty initializer =
  let def = GlobalDefinition globalVariableDefaults
            { LLVM.AST.Global.name = name
            , visibility = visibility
            , LLVM.AST.Global.type' = ty
            , initializer = Just initializer
            }
  in do
    emitDefn def
    return $ ConstantOperand (GlobalReference (ptr ty) name)
