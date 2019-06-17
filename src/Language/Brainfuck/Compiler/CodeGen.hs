{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

module Language.Brainfuck.Compiler.CodeGen where

import           Prelude                                   hiding (EQ)

import           Language.Brainfuck.Compiler.AdvancedIR
import           Language.Brainfuck.Compiler.CodeGen.Utils
import           Language.Brainfuck.Compiler.Options

import           LLVM.AST                                  hiding (function)
import           LLVM.AST.Constant
import           LLVM.AST.Global
import qualified LLVM.AST.Global                           as Glob
import           LLVM.AST.Instruction                      hiding (function)
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type
import           LLVM.AST.Visibility
import           LLVM.IRBuilder                            hiding (global,
                                                            int32, int64, int8)
import           LLVM.IRBuilder.Module                     hiding (global)

import           Data.Sequence


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

mainModule :: CodeGenOptions -> Seq AdvancedIR -> ModuleBuilder ()
mainModule CGO{..} program = do

  primDefs@PrimDefs {..} <- defaultDefs
  let (cellType, cellVal) = case cellSize of
        I8        -> (i8,  int8)
        I32       -> (i32, int32)
        I64       -> (i64, int64)
        Unbounded -> error "Unbounded cell size not yet supported"

      arrayType = ArrayType arraySize cellType
      arraySize = case dataSize of
        FiniteSizeArray n -> fromIntegral n
        InfiniteSizeArray -> error "Infinite size arrays not yet supported"

  dataArray <- global Hidden "data" arrayType (AggregateZero arrayType)

  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \_ -> do
    dataPointer <- alloca typeSize_t Nothing 1 `named` "dataPointer"
    store dataPointer 1 (size_t 0)

    handle      <- load libc_stdout 1
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
     , eofBehavior :: EofBehavior
     , primDefs    :: PrimDefs
     }

compile :: CompilerConstants
        -> AdvancedIR
        -> IRBuilderT (ModuleBuilder) ()
compile cc@CC{primDefs=PrimDefs{..},..} = \case
  Modify{modifyAmount, offset} -> do
    index <- dataIndex offset
    val   <- load index 1
    val'  <- add val (cellVal (fromIntegral modifyAmount))
    store index 1 val'

  Set{setAmount, offset} -> do
    index <- dataIndex offset
    store index 1 (cellVal (fromIntegral setAmount))

  Multiply{offset, offsetFrom, scale, step} -> do
    fromIndex <- dataIndex offsetFrom
    fromVal   <- load fromIndex 1
    times     <- sdiv fromVal (cellVal (fromIntegral step))
    mult      <- mul times (cellVal (fromIntegral scale))

    toIndex <- dataIndex offset
    toVal   <- load toIndex 1
    result  <- add toVal mult
    store toIndex 1 result

  BaseIndex{offset} -> do
    baseIndex <- load dataPointer 1
    newIndex  <- add baseIndex (size_t (fromIntegral offset))
    store dataPointer 1 newIndex

  Loop{offset, body} -> mdo
    index <- dataIndex offset
    val   <- load index 1
    neq   <- icmp NE val (cellVal 0)
    condBr neq loopStart loopEnd

    compile cc (BaseIndex offset 0)

    loopStart <- block; do
      mapM_ (compile cc) body
      index' <- dataIndex 0
      val'   <- load index' 1
      neq'   <- icmp NE val' (cellVal 0)
      condBr neq' loopStart loopEnd

    loopEnd <- block
    return ()

  Input{offset} -> do
    char  <- call libc_getch []
    char' <- i32ToCell cellType char
    index <- dataIndex offset
    eof   <- icmp EQ char libc_EOF

    val   <- case eofBehavior of
      NoChange -> do
        oldVal <- load index 1
        select eof oldVal char'
      SetZero  -> select eof (cellVal 0) char'
      SetEOF   -> select eof (cellVal (-1)) char'

    store index 1 val

  Output{offset} -> do
    index <- dataIndex offset
    val   <- load index 1
    char  <- cellToI32 cellType val
    call libc_putch [(char, [])]
    return ()

  where
    dataIndex offset = do
      base <- load dataPointer 1
      i <- add base (size_t (fromIntegral offset))
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
