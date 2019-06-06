{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# language BlockArguments #-}
module Language.Brainfuck.Compiler where

import Language.Brainfuck.Parser
import Language.Brainfuck.Compiler.CodeGen

import Prelude hiding (length, Ordering(..))
import Data.Sequence
import Data.Foldable
import Text.Printf

import LLVM.IRBuilder
import LLVM.IRBuilder.Module
import LLVM.AST.Constant
import LLVM.AST.IntegerPredicate
import LLVM.AST.Name
import LLVM.AST.Operand
import LLVM.AST.Type

data BFIR = Modify !Int
          | Move !Int
          | LoopStart (Seq BFIR)
          | LoopEnd (Seq BFIR)
          | Loop BFSeq
          | Input
          | Output

instance Show BFIR where
  show = \case
    Modify n -> printf "(%+d)" n
    Move n -> printf "(<>%d)" n
    LoopStart s -> printf "{%s" (show (BFS s))
    LoopEnd s -> printf "%s}" (show (BFS s))
    Loop s -> printf "[%s]" (show s)
    Input -> ","
    Output -> "."

newtype BFSeq = BFS (Seq BFIR)

instance Show BFSeq where
  show (BFS s) = concatMap show s

instance Monoid BFSeq where
  mempty = BFS empty

instance Semigroup BFSeq where
  BFS a <> BFS b = BFS $ mergeBFSeq a b

programSize :: BFSeq -> Int
programSize (BFS p) = foldr' ((+) . irSize) 0 p
  where irSize (Loop s) = programSize s + 2
        irSize (LoopStart s) = programSize (BFS s) + 1
        irSize (LoopEnd s) = programSize (BFS s) + 1
        irSize _ = 1

bfInst2BFIR :: BFInst -> BFIR
bfInst2BFIR = \case
  IncD -> Modify 1
  DecD -> Modify (-1)
  DRig -> Move 1
  DLef -> Move (-1)
  JmpF -> LoopStart empty
  JmpB -> LoopEnd empty
  Inp  -> Input
  Out  -> Output

mergeBFSeq first second = case (first, second) of
  (Empty, b) -> b
  (a, Empty) -> a
  (a :|> Modify m, Modify n :<| b)
    | (m + n) == 0 -> mergeBFSeq a b
    | otherwise -> mergeBFSeq (a |> Modify (m+n)) b
  (a :|> Move m, Move n :<| b)
    | (m + n) == 0 -> mergeBFSeq a b
    | otherwise -> mergeBFSeq (a |> Move (m+n)) b
    
  (a :|> LoopStart ia, LoopEnd ib :<| b)
    -> mergeBFSeq (a |> Loop (BFS $ mergeBFSeq ia ib)) b
    
  (a, LoopStart ia :<| b) -> mergeBFSeq (a |> LoopStart ia) b
  
  (a :|> LoopEnd ib, b) -> mergeBFSeq a (LoopEnd ib <| b)
  
  (a :|> LoopStart ia, x :<| b)
    -> mergeBFSeq (a |> LoopStart (mergeBFSeq ia (singleton x))) b
    
  (a :|> x, LoopEnd ib :<| b)
    -> mergeBFSeq a (LoopEnd (mergeBFSeq (singleton x) ib) <| b)
    
  (a, x :<| b) -> mergeBFSeq (a |> x) b

inst2Seq = mconcat . map (BFS . singleton . bfInst2BFIR)

type WholeProgramPass = BFSeq -> BFSeq
type PartialPass = Seq BFIR -> Seq BFIR
type PairStep = BFIR -> BFIR -> Maybe (Seq BFIR)

stepTraversal :: PairStep -> WholeProgramPass
stepTraversal p (BFS s) = BFS $ go s
  where
    go (x :<| y :<| r) = case p x y of
                           Nothing -> x <| y <| go r
                           Just s -> go (s >< r)
    go s = s

deadStartLoop :: WholeProgramPass
deadStartLoop (BFS (Loop _ :<| r)) = deadStartLoop (BFS r)
deadStartLoop x = x

uselessConsequentLoopPass = stepTraversal p
  where p (Loop x) (Loop _) = Just (singleton $ Loop x)
        p _ _ = Nothing

optimize :: BFSeq -> BFSeq
optimize = uselessConsequentLoopPass . deadStartLoop

mainModule :: BFSeq -> ModuleBuilder ()
mainModule (BFS program) = do
  PrimDefs {..} <- defaultDefs

  let arrayType = ArrayType 30000 i8
  da <- global "data" arrayType (AggregateZero arrayType)

  function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 \_ -> do
    h <- load stdout 1
    call setvbuf [ (h, [])
                 , (nullPtr i8, [])
                 , (noBuffering, [])
                 , (size_t 0, [])
                 ]
    dp <- named (alloca i32 Nothing 1) "dp"
    compile (CC{..}) program
    ret =<< int32 0
  return ()

data CompilerConstants = CC { dp :: Operand
                            , da :: Operand
                            , getch :: Operand
                            , putch :: Operand
                            }

compile :: CompilerConstants
        -> Seq BFIR
        -> IRBuilderT (ModuleBuilder) ()
compile _ Empty = return ()
compile cc@CC{..} (i :<| rest) = case i of
  Modify n -> do
    daIndex <- load dp 1
    z <- int32 0
    index <- gep da [z, daIndex]
    v <- load index 1
    v' <- if n > 0 then
            add v =<< int8 (fromIntegral n)
          else
            sub v =<< int8 (fromIntegral (negate n))
    store index 1 v'
    compile cc rest
    
  Move n -> do
    daIndex <- load dp 1
    daIndex' <- if n > 0 then
                  add daIndex =<< int32 (fromIntegral n)
                else
                  sub daIndex =<< int32 (fromIntegral (negate n))
    store dp 1 daIndex'
    compile cc rest
    
  Loop (BFS s) -> mdo
    z <- int32 0
    z8 <- int8 0

    daIndex <- load dp 1
    index <- gep da [z, daIndex]
    v <- load index 1
    neq <- icmp NE v z8
    condBr neq loopStart loopEnd
    
    loopStart <- named block "loopStart"
    compile cc s

    daIndex' <- load dp 1
    index' <- gep da [z, daIndex']
    v' <- load index' 1
    neq' <- icmp NE v' z8
    condBr neq' loopStart loopEnd
    
    loopEnd <- named block "loopEnd"
    compile cc rest

  Input -> do
    c <- call getch []
    c' <- trunc c (i8)

    daIndex <- load dp 1
    z <- int32 0
    index <- gep da [z, daIndex]
    store index 1 c'
    compile cc rest

  Output -> do
    daIndex <- load dp 1
    z <- int32 0
    index <- gep da [z, daIndex]
    v <- load index 1
    c <- sext v i32
    call putch [(c, [])]
    compile cc rest
