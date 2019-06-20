{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Language.Brainfuck.Compiler.CodeGen.Utils where

import           Control.Monad.State
import           Data.IntMap
import           Data.Word
import           LLVM.AST
import           LLVM.AST.Constant
import           LLVM.AST.Global
import           LLVM.AST.Type
import           LLVM.AST.Visibility
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

intOp :: Word32 -> Integer -> Operand
intOp bits n = ConstantOperand (Int bits n)

int64 :: Integer -> Operand
int64 = intOp 64
int32 :: Integer -> Operand
int32 = intOp 32
int8 :: Integer -> Operand
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

data CodeGenState = CodeGenState
  { loadedCache :: IntMap Operand
  , storedCache :: IntMap Operand
  }

emptyCodeGenState = CodeGenState empty empty

newtype CodeGenMonad a = CodeGenMonad
  { runCodeGenMonad :: StateT CodeGenState (IRBuilderT ModuleBuilder) a
  } deriving ( MonadModuleBuilder, MonadIRBuilder, MonadState CodeGenState
             , MonadFix, Functor, Monad, Applicative)

modifyLoadedCache :: (IntMap Operand -> IntMap Operand) -> CodeGenMonad ()
modifyLoadedCache f = modify (\s@CodeGenState{loadedCache}->s{loadedCache= f loadedCache})
modifyBothCache :: (IntMap Operand -> IntMap Operand) -> CodeGenMonad ()
modifyBothCache f = modify (\s@CodeGenState{..}->
                               CodeGenState {loadedCache = f loadedCache
                                            ,storedCache = f storedCache})

execCodeGen :: CodeGenMonad a -> IRBuilderT ModuleBuilder (IntMap Operand)
execCodeGen m = storedCache <$> execStateT (runCodeGenMonad m) emptyCodeGenState
