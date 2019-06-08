{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Language.Brainfuck.Compiler.CodeGen where

import Language.Brainfuck.Compiler.BFIR
import Language.Brainfuck.Compiler.Options

import LLVM.AST hiding (function)
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.AST.Global
import qualified LLVM.AST.Global as Glob
import LLVM.AST.Instruction hiding (function)
import LLVM.AST.IntegerPredicate
import LLVM.IRBuilder.Module
import LLVM.IRBuilder

import Data.Sequence


data PrimDefs = PrimDefs
  { libc_FILE :: Type
  , libc_getch :: Operand
  , libc_putch :: Operand
  , libc_setvbuf :: Operand
  , libc__IONBF :: Operand
  , libc_stdout :: Operand
  , typeSize_t :: Type
  , size_t :: Integer -> Operand
  , nullPtr :: Type -> Operand
  }

defaultDefs :: (MonadModuleBuilder m) => m PrimDefs
defaultDefs = do
  let typeSize_t = i64
      size_t n = ConstantOperand (Int 64 n)
      nullPtr t = ConstantOperand (Null (ptr t))
      libc__IONBF = ConstantOperand (Int 32 2)
      
  libc_FILE <- typedef "FILE" Nothing
  libc_getch <- extern "getchar" [] i32
  libc_putch <- extern "putchar" [i32] i32
  libc_setvbuf <- extern "setvbuf" [ptr libc_FILE, ptr i8, i32, typeSize_t] i32
  
  let stdoutDef = GlobalDefinition $ globalVariableDefaults
        { name = "stdout"
        , Glob.type'= ptr libc_FILE
        , isConstant = True
        }
      libc_stdout = ConstantOperand (GlobalReference (ptr (ptr libc_FILE)) "stdout")
  emitDefn stdoutDef
  return PrimDefs{..}
    
mainModule :: CodeGenOptions -> BFSeq -> ModuleBuilder ()
mainModule CGO{..} (BFS program) = do
  PrimDefs {..} <- defaultDefs
  let cellSize = i8

  let arrayType = ArrayType 30000 i8
  da <- global "data" arrayType (AggregateZero arrayType)

  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 \_ -> do
    h <- load libc_stdout 1
    call libc_setvbuf [ (h, [])
                 , (nullPtr i8, [])
                 , (libc__IONBF, [])
                 , (size_t 0, [])
                 ]
    dp <- named (int32 0) "dp"
    compile (CC{getch = libc_getch, putch = libc_putch, ..}) program
    ret =<< int32 0
  return ()

data CompilerConstants =
  CC { dp :: Operand
     , da :: Operand
     , getch :: Operand
     , putch :: Operand
     , cellSize :: Type
     }

compile :: CompilerConstants
        -> Seq BFIR
        -> IRBuilderT (ModuleBuilder) Operand
compile cc Empty = return (dp cc)
compile cc@CC{..} (i :<| rest) = case i of
  Modify n -> do
    z <- int32 0
    index <- gep da [z, dp]
    v <- load index 1
    v' <- add v =<< int8 (fromIntegral n)
    store index 1 v'
    compile cc rest
    
  Move n -> do
    dp' <- add dp =<< int32 (fromIntegral n)
    compile cc{dp=dp'} rest
    
  Loop (BFS s) -> mdo
    z <- int32 0
    z8 <- int8 0
    index <- gep da [z, dp]
    v <- load index 1
    neq <- icmp NE v z8
    condBr neq loopStart loopEnd
    preLoop <- currentBlock
    
    loopStart <- block;
    dp' <- phi [(dp, preLoop), (dp'', postLoop)]
    dp'' <- compile cc{dp=dp'} s

    index' <- gep da [z, dp'']
    v' <- load index' 1
    neq' <- icmp NE v' z8
    postLoop <- currentBlock
    condBr neq' loopStart loopEnd
    
    loopEnd <- block
    dp''' <- phi [(dp, preLoop), (dp'', postLoop)]
    
    compile cc{dp=dp'''} rest

  Input -> do
    c <- call getch []
    c' <- trunc c (i8)

    z <- int32 0
    index <- gep da [z, dp]
    store index 1 c'
    compile cc rest

  Output -> do
    z <- int32 0
    index <- gep da [z, dp]
    v <- load index 1
    c <- sext v i32
    call putch [(c, [])]
    compile cc rest
