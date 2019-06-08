{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Language.Brainfuck.Compiler.CodeGen where

import Prelude hiding (EQ)

import Language.Brainfuck.Compiler.BFIR
import Language.Brainfuck.Compiler.Options
import Language.Brainfuck.Compiler.CodeGen.Utils

import LLVM.AST hiding (function)
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.AST.Global
import qualified LLVM.AST.Global as Glob
import LLVM.AST.Visibility
import LLVM.AST.Instruction hiding (function)
import LLVM.AST.IntegerPredicate
import LLVM.IRBuilder.Module hiding (global)
import LLVM.IRBuilder hiding (global, int8, int32, int64)

import Data.Sequence


data PrimDefs = PrimDefs
  { libc_FILE    :: Type
  , libc_getch   :: Operand
  , libc_putch   :: Operand
  , libc_setvbuf :: Operand
  , libc__IONBF  :: Operand
  , libc_stdout  :: Operand
  , libc_EOF     :: Operand
  , typeSize_t   :: Type
  , size_t       :: Integer -> Operand
  , nullPtr      :: Type    -> Operand
  }

defaultDefs :: (MonadModuleBuilder m) => m PrimDefs
defaultDefs = do
  let typeSize_t  = i64
      size_t n    = int64 n
      nullPtr t   = ConstantOperand (Null (ptr t))
      libc__IONBF = int32 2
      libc_EOF    = int32 (-1)
      
  libc_FILE    <- typedef "FILE" Nothing
  libc_getch   <- extern "getchar" [] i32
  libc_putch   <- extern "putchar" [i32] i32
  libc_setvbuf <- extern "setvbuf" [ptr libc_FILE, ptr i8, i32, typeSize_t] i32
  
  let stdoutDef = GlobalDefinition $ globalVariableDefaults
        { name       = "stdout"
        , Glob.type' = ptr libc_FILE
        , isConstant = True
        }
      libc_stdout = ConstantOperand (GlobalReference (ptr (ptr libc_FILE)) "stdout")
  emitDefn stdoutDef
  
  return PrimDefs{..}
    
mainModule :: CodeGenOptions -> BFSeq -> ModuleBuilder ()
mainModule CGO{..} (BFS program) = do
  primDefs@PrimDefs {..} <- defaultDefs
  let (cellType, cellVal) = case cellSize of
        I8  -> (i8,  int8)
        I32 -> (i32, int32)
        I64 -> (i64, int64)
        Unbounded -> error "Unbounded cell size not yet supported"

      arrayType = ArrayType arraySize cellType
      arraySize = case dataSize of
        FiniteSizeArray n -> fromIntegral n
        InfiniteSizeArray -> error "Infinite size arrays not yet supported"
      
  dataArray <- global Hidden "data" arrayType (AggregateZero arrayType)

  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 \_ -> do
    handle      <- load libc_stdout 1
    dataPointer <- alloca typeSize_t Nothing 1 `named` "dataPointer"
    store dataPointer 1 (size_t 0)
    call libc_setvbuf
      [ (handle     , [])
      , (nullPtr i8 , [])
      , (libc__IONBF, [])
      , (size_t 0   , [])
      ]
    mapM_ (compile (CC{..})) program
    ret (int32 0)
  return ()
data CompilerConstants =
  CC { dataPointer :: Operand
     , dataArray   :: Operand
     , cellType    :: Type
     , cellVal     :: Integer -> Operand
     , primDefs    :: PrimDefs
     }

compile :: CompilerConstants
        -> BFIR
        -> IRBuilderT (ModuleBuilder) ()
compile cc@CC{primDefs=PrimDefs{..},..} = \case
  Modify n -> do
    index <- dataIndex
    val   <- load index 1
    val'  <- add val (cellVal (fromIntegral n))
    store index 1 val'
    
  Move n -> do
    i  <- load dataPointer 1
    i' <- add i (size_t (fromIntegral n))
    store dataPointer 1 i'
    
  Loop (BFS s) -> mdo
    index <- dataIndex
    val   <- load index 1
    neq   <- icmp NE val (cellVal 0)
    condBr neq loopStart loopEnd
    
    loopStart <- block; do
      mapM_ (compile cc) s
      index' <- dataIndex
      val'   <- load index' 1
      neq'   <- icmp NE val' (cellVal 0)
      condBr neq' loopStart loopEnd
    
    loopEnd <- block
    return ()

  Input -> do
    char  <- call libc_getch []
    eof   <- icmp EQ char libc_EOF
    
    char' <- i32ToCell cellType char
    index <- dataIndex
    val   <- load index 1
    val'  <- select eof val char' -- Old value on EOF
    
    store index 1 val'

  Output -> do
    index <- dataIndex
    val   <- load index 1
    char  <- cellToI32 cellType val
    call libc_putch [(char, [])]
    return ()
  where
    dataIndex = do
      i <- load dataPointer 1
      gep dataArray [size_t 0, i]

-- TODO Error handling in compiler is a good idea
i32ToCell ty c | ty == i8  = trunc c i8
               | ty == i64 = zext c i64
               | ty == i32 = return c
               | otherwise = error "Unsupported cell size"
cellToI32 ty c | ty == i8  = zext c i32
               | ty == i64 = trunc c i32
               | ty == i32 = return c
               | otherwise = error "Unsupported cell size"
