{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}

module Language.Brainfuck.NaiveCompiler where

import Prelude hiding (getChar, putChar, Ordering(..), lookup)
import Language.Brainfuck.Parser

import LLVM.AST hiding (function)
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.AST.Instruction hiding (function)
import LLVM.AST.IntegerPredicate

import LLVM.IRBuilder hiding (named)
import qualified LLVM.IRBuilder.Monad (named)
import LLVM.IRBuilder.Module

import Control.Monad.State
import Data.Map


data CompilerState = CS
  { dataPointer :: !Operand
  , dataArray :: !Operand
  , getCh :: !Operand
  , putCh :: !Operand
  , backJmpStack :: [JumpInfo]
  , fwdJmpStack :: [JumpInfo]
  }

data JumpInfo = JI { jmpDp :: !Operand
                   , jmpFrom :: !Name
                   , jmpTo :: !Name
                   }

newtype BrainfuckCompiler a =
  BFC (StateT CompilerState (IRBuilderT ModuleBuilder) a)
  deriving ( MonadState CompilerState, MonadModuleBuilder
           , MonadIRBuilder, Monad, Applicative, Functor, MonadFix)


runBrainfuckCompiler (BFC m) = runStateT m

getDp, getDa, getChar, putChar :: BrainfuckCompiler Operand
getDp = dataPointer <$> get
getDa = dataArray <$> get
getChar = getCh <$> get
putChar = putCh <$> get
setDp :: Operand -> BrainfuckCompiler ()
setDp dp = modify \s -> s{dataPointer=dp}

pushBackJmpStack :: JumpInfo -> BrainfuckCompiler ()
pushBackJmpStack jmpInfo =
  modify \s@CS{..} -> s{backJmpStack=jmpInfo:backJmpStack}
popBackJmpStack :: BrainfuckCompiler JumpInfo
popBackJmpStack = do
  s <- gets backJmpStack
  let (h:t) = s
  modify \s ->s{backJmpStack=t}
  return h

pushFwdJmpStack :: JumpInfo -> BrainfuckCompiler ()
pushFwdJmpStack jmpInfo =
  modify \s@CS{..} -> s{fwdJmpStack=jmpInfo:fwdJmpStack}
popFwdJmpStack :: BrainfuckCompiler JumpInfo
popFwdJmpStack = do
  s <- gets fwdJmpStack
  let (h:t) = s
  modify \s ->s{fwdJmpStack=t}
  return h

mainModule :: [BFInst] -> ModuleBuilder ()
mainModule program = do
  getch <- extern "getchar" [] i32
  putch <- extern "putchar" [i32] i32
  let arrayType = (ArrayType 30000 i8)
  dataArray <- global "data" arrayType (AggregateZero arrayType)
  
  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 \_ -> mdo
    dp <- int32 0
    let compile = mapM_ compileInst program
        initialState =
          CS { dataPointer = dp
             , dataArray = dataArray
             , getCh = getch
             , putCh = putch
             , backJmpStack = []
             , fwdJmpStack = fwdJmpStack finalState
             }
    (_, finalState) <- runBrainfuckCompiler compile initialState
    ret =<< int32 0
    
  return ()

type CompilerContinuation = Maybe (Operand, Name, Name, [BFInst])

compileInst :: BFInst -> BrainfuckCompiler ()
compileInst = \case
  IncD -> named "+" do
    index <- dataIndex
    v <- load index 1
    v' <- add v =<< int8 1
    store index 1 v'
  DecD -> named "-" do
    index <- dataIndex
    v <- load index 1
    v' <- sub v =<< int8 1
    store index 1 v'
  DRig -> named ">" do
    dp <- getDp
    dp' <- add dp =<< int32 1
    setDp dp'
  DLef -> named "<" do
    dp <- getDp
    dp' <- sub dp =<< int32 1
    setDp dp'
  JmpF -> named "[" mdo
    index <- dataIndex
    v <- load index 1
    z <- int8 0
    eq <- icmp EQ z v
    condBr eq (jmpFrom jmpInfo) nextBlock
    prevBlock <- currentBlock
    nextBlock <- block
    dp <- getDp
    pushBackJmpStack (JI dp prevBlock nextBlock)
    jmpInfo <- popFwdJmpStack
    dp' <- phi [(dp, prevBlock), (jmpDp jmpInfo, jmpFrom jmpInfo)]
    setDp dp'
  JmpB -> named "]" mdo
    index <- dataIndex
    v <- load index 1
    z <- int8 0
    neq <- icmp NE z v
    condBr neq (jmpTo jmpInfo) nextBlock
    prevBlock <- currentBlock
    nextBlock <- block
    dp <- getDp
    pushFwdJmpStack (JI dp prevBlock nextBlock)
    jmpInfo <- popBackJmpStack
    dp' <- phi [(dp, prevBlock), (jmpDp jmpInfo, jmpFrom jmpInfo)]
    setDp dp'
    
  Inp -> named "," do
    index <- dataIndex
    v <- load index 1
    getch <- getChar
    r <- call getch []
    eof <- int32 (-1)
    c <- trunc r i8
    isEof <- icmp EQ r eof
    v' <- select isEof v c
    store index 1 v'
  Out -> named "." do
    index <- dataIndex
    v <- load index 1
    c <- sext v i32
    putch <- putChar
    call putch [(c, [])]
    return ()

dataIndex :: BrainfuckCompiler Operand
dataIndex = do z <- int32 0
               dp <- getDp
               da <- getDa
               gep da [z, dp]

named = flip const --flip LLVM.IRBuilder.Monad.named
