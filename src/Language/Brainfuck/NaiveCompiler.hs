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

import LLVM.IRBuilder
import LLVM.IRBuilder.Module

import Control.Monad.State
import Data.Map


data CompilerState = CS
  { dataPointer :: !Operand
  , dataArray :: !Operand
  , getCh :: !Operand
  , putCh :: !Operand
  , jmpStack :: ![StackID]
  , stackC :: !StackID
  , fwdJmpMap :: JmpMap
  , timeTravelingFwdJmpMap :: JmpMap
  , backJmpMap :: JmpMap
  }

type StackID = Int
-- JmpMap: (dp of the block that jumps, block that jumps,
--          block that should be jumped to)
type JmpMap = Map StackID (Operand, Name, Name)


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
pushJmpStack :: BrainfuckCompiler StackID
pushJmpStack = do
  stackId <- stackC <$> get
  jmpTo <- fresh
  dpName <- fresh
  let dp = LocalReference i32 dpName
  modify \s@CS{..} -> s{jmpStack=(stackC:jmpStack)
                       ,stackC=stackC+1}
  return stackId
popJmpStack :: BrainfuckCompiler StackID
popJmpStack = do
  s <- jmpStack <$> get
  let (h:t) = s
  modify \s ->s{jmpStack=t}
  return h

addBackJmp :: StackID -> Operand -> Name -> Name -> BrainfuckCompiler ()
addBackJmp id op from to =
  modify \s@CS{..} -> s{backJmpMap=insert id (op, from, to) backJmpMap}
lookupBackJmp :: StackID -> BrainfuckCompiler (Operand, Name, Name)
lookupBackJmp id = do
  m <- backJmpMap <$> get
  let (Just ref) = lookup id m
  return ref
  
addFwdJmp :: StackID -> Operand -> Name -> Name -> BrainfuckCompiler ()
addFwdJmp id op from to =
  modify \s@CS{..} -> s{fwdJmpMap= insert id (op, from, to) fwdJmpMap}
lookupFwdJmp :: StackID -> BrainfuckCompiler (Operand, Name, Name)
lookupFwdJmp id = do
  m <- timeTravelingFwdJmpMap <$> get
  let (Just ref) = lookup id m
  return ref


mainModule :: [BFInst] -> ModuleBuilder ()
mainModule program = do
  getch <- extern "getchar" [] i32
  putch <- extern "putchar" [i32] i32
  let arrayType = (ArrayType 30000 i8)
  dataArray <- global "data" arrayType (AggregateZero arrayType)
  
  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 \_ -> mdo
    dp <- int32 0
    let compile = mapM_ compileInst program
        startState =
          CS { dataPointer = dp
             , dataArray = dataArray
             , getCh = getch
             , putCh = putch
             , jmpStack = []
             , stackC = 0
             , fwdJmpMap = empty
             , backJmpMap = empty
             , timeTravelingFwdJmpMap = fwdJmpMap finishedState
             }
    (_, finishedState) <- runBrainfuckCompiler compile startState
    ret =<< int32 0
    
  return ()

compileInst :: BFInst -> BrainfuckCompiler ()
compileInst = \case
  IncD -> do index <- indexDa
             v <- load index 1
             v' <- add v =<< int8 1
             store index 1 v'
             return ()
  DecD -> do index <- indexDa
             v <- load index 1
             v' <- sub v =<< int8 1
             store index 1 v'
             return ()
  DRig -> do dp <- getDp
             dp' <- add dp =<< int8 1
             setDp dp'
  DLef -> do dp <- getDp
             dp' <- sub dp =<< int8 1
             setDp dp'
  JmpF -> mdo stackID <- pushJmpStack
              index <- indexDa
              v <- load index 1
              z <- int32 0
              eq <- icmp EQ z v
              (incomingDp, comeFrom, futureBlock) <- lookupFwdJmp stackID
              condBr eq futureBlock nextBlock
              prevBlock <- currentBlock
              nextBlock <- block
              dp <- getDp
              addBackJmp stackID dp prevBlock nextBlock
              dp' <- phi [(dp, prevBlock), (incomingDp, comeFrom)]
              setDp dp'
              return ()
  JmpB -> mdo stackID <- popJmpStack
              index <- indexDa
              v <- load index 1
              z <- int32 0
              neq <- icmp NE z v
              (incomingDp, comeFrom, oldBlock) <- lookupBackJmp stackID
              condBr neq oldBlock nextBlock
              prevBlock <- currentBlock
              nextBlock <- block
              dp <- getDp
              addFwdJmp stackID dp prevBlock nextBlock
              dp' <- phi [(dp, prevBlock), (incomingDp, comeFrom)]
              setDp dp'
              return ()
  Inp -> do index <- indexDa
            v <- load index 1
            getch <- getChar
            r <- call getch []
            eof <- int32 (-1)
            c <- trunc r i8
            isEof <- icmp EQ r eof
            v' <- select isEof v c
            store index 1 v'
  Out -> do index <- indexDa
            v <- load index 1
            c <- sext v i32
            putch <- putChar
            call putch [(c, [])]
            return ()

indexDa :: BrainfuckCompiler Operand
indexDa = do z <- int32 0
             dp <- getDp
             da <- getDa
             gep da [z, dp]
