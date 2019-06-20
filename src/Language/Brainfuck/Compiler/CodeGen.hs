{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

module Language.Brainfuck.Compiler.CodeGen where

import           Prelude                                   hiding (EQ)

import           Language.Brainfuck.Compiler.CodeGen.Utils
import           Language.Brainfuck.Compiler.IR
import           Language.Brainfuck.Compiler.Options

import           Control.Monad.State
import           Data.IntMap
import qualified Data.IntMap                               as IntMap
import           Data.Sequence
import           LLVM.AST                                  hiding (function)
import           LLVM.AST.Constant
import           LLVM.AST.Global
import qualified LLVM.AST.Global                           as Glob
import           LLVM.AST.IntegerPredicate
import           LLVM.AST.Type
import           LLVM.AST.Visibility
import           LLVM.IRBuilder                            hiding (global,
                                                            int32, int64, int8)


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

mainModule :: CodeGenOptions -> Seq IntermediateCode -> ModuleBuilder ()
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
    store dataPointer 1 (size_t (fromIntegral dataStart))

    handle      <- load libc_stdout 1
    call libc_setvbuf
      [ (handle     , [])
      , (nullPtr i8 , [])
      , (libc__IONBF, [])
      , (size_t 0   , [])
      ]

    let constants = CC{..}

    lastUpdates <- execCodeGen (mapM_ (compile constants) program)

    writeUpdates (IntMap.toList lastUpdates) constants

    ret (int32 0)
  return ()
data CompilerConstants =
  CC { dataPointer :: Operand
     , dataArray   :: Operand
     , cellType    :: Type -- ^ Configurable type
     , cellVal     :: Integer -> Operand -- ^ To make constants of the cell type
     , eofBehavior :: EofBehavior
     , primDefs    :: PrimDefs
     }

compile :: CompilerConstants
        -> IntermediateCode
        -> CodeGenMonad ()
compile cc@CC{primDefs=PrimDefs{..},..} = \case
  Modify{modifyAmount, offset} -> do
    val  <- loadIndex offset
    val' <- add val (cellVal (fromIntegral modifyAmount))
    storeIndex offset val'

  Set{setAmount, offset} -> do
    storeIndex offset (cellVal (fromIntegral setAmount))

  Multiply{offset, offsetFrom, scale, step} -> do
    fromVal <- loadIndex offsetFrom
    times   <- sdiv fromVal (cellVal (fromIntegral step)) -- [Overflow is UB]
    mult    <- mul times (cellVal (fromIntegral scale))

    toVal  <- loadIndex offset
    result <- add toVal mult
    storeIndex offset result

  BaseIndex{offset} -> do
    writeCachedStores
    baseIndex <- load dataPointer 1
    newIndex  <- add baseIndex (size_t (fromIntegral offset))
    store dataPointer 1 newIndex

  Loop{offset, body} -> mdo
    val <- loadIndex offset
    neq <- icmp NE val (cellVal 0)

    compile cc (BaseIndex offset) -- Implicit BaseIndex before loop

    condBr neq loopStart loopEnd

    loopStart <- block; do
      mapM_ (compile cc) body
      val'    <- loadIndex 0 -- Loop condition is always at base 0 inside loop
      neq'    <- icmp NE val' (cellVal 0)

      -- Loop operands are not valid outside the loop
      -- BaseIndex usually stores them, but BaseIndex 0 gets optimized away
      writeCachedStores

      condBr neq' loopStart loopEnd

    loopEnd <- block

    return ()

  Input{offset} -> do
    char  <- call libc_getch []
    char' <- i32ToCell cellType char
    eof   <- icmp EQ char libc_EOF

    val <- case eofBehavior of
      NoChange -> do
        oldVal <- loadIndex offset
        select eof oldVal char'
      SetZero  -> select eof (cellVal 0) char'
      SetEOF   -> select eof (cellVal (-1)) char'

    storeIndex offset val

  Output{offset} -> do
    val  <- loadIndex offset
    char <- cellToI32 cellType val
    call libc_putch [(char, [])]
    return ()

  where
    loadIndex :: Int -> CodeGenMonad Operand
    loadIndex offset = do
      saved     <- gets (IntMap.lookup offset . loadedCache)
      case saved of
        Just x       -> return x
        Nothing      -> do
          index <- dataIndex offset
          val   <- load index 1
          modifyLoadedCache (insert offset val)
          return val

    storeIndex :: Int -> Operand -> CodeGenMonad ()
    storeIndex offset value =
      modifyBothCache (insert offset value)

    writeCachedStores = do
      updates <- gets (toList . storedCache)
      writeUpdates updates cc
      put emptyCodeGenState

    dataIndex offset = do
      base <- load dataPointer 1
      i    <- add base (size_t (fromIntegral offset))
      gep dataArray [size_t 0, i]

{- [Overflow is UB]

Because overflow is undefined behavior it's okay to assume that diving by the
multiplication step never yields a remainder. If it did have a remainder the
original loop would've missed the zero, triggering the UB.

TODO This can maybe be made configurable as overflow makes sense, at least for
smaller cell sizes
-}

writeUpdates updates CC{dataPointer, dataArray, primDefs=PrimDefs{size_t}} =
  forM_ updates $ \(cell, val) -> do
  base  <- load dataPointer 1
  i     <- add base (size_t (fromIntegral cell))
  index <- gep dataArray [size_t 0, i]
  store index 1 val

-- TODO Error handling in compiler is a good idea
i32ToCell ty c | ty == i8  = trunc c i8
               | ty == i64 = zext c i64
               | ty == i32 = return c
               | otherwise = error "Unsupported cell size"
cellToI32 ty c | ty == i8  = zext c i32
               | ty == i64 = trunc c i32
               | ty == i32 = return c
               | otherwise = error "Unsupported cell size"
