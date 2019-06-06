{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}

module Language.Brainfuck.Compiler.NaiveCompiler where

import Prelude hiding (getChar, putChar, Ordering(..), lookup)

import Language.Brainfuck.Parser
import Language.Brainfuck.Compiler.CodeGen

import LLVM.AST hiding (function)
import LLVM.AST.Type
import LLVM.AST.Constant
import LLVM.AST.Global
import qualified LLVM.AST.Global as Glob
import LLVM.AST.Instruction hiding (function)
import LLVM.AST.IntegerPredicate

import LLVM.IRBuilder hiding (named)
import qualified LLVM.IRBuilder.Monad (named)
import LLVM.IRBuilder.Module

import Control.Monad.State
import Data.Map
import Data.Maybe (fromJust)


data CompilerState = CS
  { dataPointer :: !Operand
  , dataArray :: !Operand
  , getCh :: !Operand
  , putCh :: !Operand
  , jmpStack :: ![JumpInfo]
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

pushJmpStack :: JumpInfo -> BrainfuckCompiler ()
pushJmpStack jmpInfo =
  modify \s@CS{..} -> s{jmpStack=jmpInfo:jmpStack}
popJmpStack :: BrainfuckCompiler JumpInfo
popJmpStack = do
  s <- gets jmpStack
  let (h:t) = s
  modify \s ->s{jmpStack=t}
  return h

mainModule :: [BFInst] -> ModuleBuilder ()
mainModule program = do

  PrimDefs{..} <- defaultDefs
  
  let arrayType = (ArrayType 30000 i8)
  dataArray <- global "data" arrayType (AggregateZero arrayType)
  
  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 \_ -> mdo
    h <- load stdout 1
    call setvbuf [ (h, [])
                 , (ConstantOperand (Null (ptr i8)), [])
                 , (noBuffering, [])
                 , (ConstantOperand (size_t 0), [])
                 ]
    dp <- int32 0
    let initialState =
          CS { dataPointer = dp
             , dataArray = dataArray
             , getCh = getch
             , putCh = putch
             , jmpStack = []
             }
    (_, finalState) <- runBrainfuckCompiler (compile program) initialState
    ret =<< int32 0
    
  return ()

compile :: [BFInst] -> BrainfuckCompiler (Maybe (JumpInfo, [BFInst]))
compile [] = return Nothing
compile (i:rest) = case i of
  IncD -> named "+" do
    index <- dataIndex
    v <- load index 1
    v' <- add v =<< int8 1
    store index 1 v'
    compile rest
  DecD -> named "-" do
    index <- dataIndex
    v <- load index 1
    v' <- sub v =<< int8 1
    store index 1 v'
    compile rest
  DRig -> named ">" do
    dp <- getDp
    dp' <- add dp =<< int32 1
    setDp dp'
    compile rest
  DLef -> named "<" do
    dp <- getDp
    dp' <- sub dp =<< int32 1
    setDp dp'
    compile rest
  JmpF -> named "[" mdo
    index <- dataIndex
    v <- load index 1
    z <- int8 0
    eq <- icmp EQ z v
    condBr eq (jmpTo jmpInfo) nextBlock
    prevBlock <- currentBlock
    nextBlock <- block
    dp <- getDp
    pushJmpStack (JI dp prevBlock nextBlock)
    dp' <- phi [(dp, prevBlock), (jmpDp jmpInfo, jmpFrom jmpInfo)]
    setDp dp'
    (jmpInfo, rest') <- fromJust <$> compile rest
    compile rest'
  JmpB -> named "]" mdo
    index <- dataIndex
    v <- load index 1
    z <- int8 0
    neq <- icmp NE z v
    condBr neq (jmpTo jmpInfo) nextBlock
    prevBlock <- currentBlock
    nextBlock <- block
    dp <- getDp
    jmpInfo <- popJmpStack
    dp' <- phi [(dp, prevBlock), (jmpDp jmpInfo, jmpFrom jmpInfo)]
    setDp dp'
    return (Just (JI dp prevBlock nextBlock, rest))
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
    compile rest
  Out -> named "." do
    index <- dataIndex
    v <- load index 1
    c <- sext v i32
    putch <- putChar
    call putch [(c, [])]
    compile rest

  where
    dataIndex :: BrainfuckCompiler Operand
    dataIndex = do z <- int32 0
                   dp <- getDp
                   da <- getDa
                   gep da [z, dp]

named = flip const --flip LLVM.IRBuilder.Monad.named
