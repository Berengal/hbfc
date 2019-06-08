{-# LANGUAGE LambdaCase #-}
module Language.Brainfuck.Compiler.BFIR where

import Language.Brainfuck.Parser

import Data.Foldable
import Data.Sequence
import Text.Printf

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

-- TODO This function is a mess, very bad asymptotics
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