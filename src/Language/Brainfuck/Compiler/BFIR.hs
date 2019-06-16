{-# LANGUAGE LambdaCase #-}
module Language.Brainfuck.Compiler.BFIR where

import           Language.Brainfuck.Parser

import           Data.Sequence
import           Text.Printf

data SimpleIR = Modify !Int
              | Move !Int
              | LoopStart (Seq SimpleIR)
              | LoopEnd (Seq SimpleIR)
              | Loop BFSeq
              | Input
              | Output
  deriving Eq

instance Show SimpleIR where
  show = \case
    Modify n    -> printf "(%+d)" n
    Move n      -> printf "(<>%d)" n
    LoopStart s -> printf "{%s" (show (BFS s))
    LoopEnd s   -> printf "%s}" (show (BFS s))
    Loop s      -> printf "[%s]" (show s)
    Input       -> ","
    Output      -> "."

newtype BFSeq = BFS (Seq SimpleIR) deriving (Eq)

instance Show BFSeq where
  show (BFS s) = concatMap show s

instance Monoid BFSeq where
  mempty = BFS empty

instance Semigroup BFSeq where
  BFS a <> BFS b = BFS $ mergeBFSeq a b


isValidBFSeq :: BFSeq -> Bool
isValidBFSeq (BFS bfSeq) = flip all bfSeq $ \case
  LoopStart _   -> False
  LoopEnd _     -> False
  Loop innerSeq -> isValidBFSeq innerSeq
  _             -> True


mergeBFSeq :: Seq SimpleIR -> Seq SimpleIR -> Seq SimpleIR
mergeBFSeq first second = case (first, second) of
  (Empty, b) -> b
  (a, Empty) -> a

  (a :|> Modify m, Modify n :<| b)
    | (m + n) == 0 -> a <> b
    | otherwise    -> (a |> Modify (m+n)) <> b

  (a :|> Move m, Move n :<| b)
    | (m + n) == 0 -> a <> b
    | otherwise    -> (a |> Move (m+n)) <> b

  (a :|> LoopStart ia, LoopEnd ib :<| b)
    -> (a |> Loop (BFS $ ia <> ib)) <> b

  (a, LoopStart ia :<| b) -> (a |> LoopStart ia) `mergeBFSeq` b
  (a :|> LoopEnd ib, b)   -> a `mergeBFSeq` (LoopEnd ib <| b)

  (a :|> LoopStart ia, x :<| b)
    -> (a |> LoopStart (ia `mergeBFSeq` singleton x)) `mergeBFSeq` b

  (a :|> x, LoopEnd ib :<| b)
    -> a `mergeBFSeq` (LoopEnd (singleton x `mergeBFSeq` ib) <| b)

  (a, x :<| b) -> (a |> x) <> b

bfInst2SimpleIR :: Char -> SimpleIR
bfInst2SimpleIR = \case
  '+' -> Modify 1
  '-' -> Modify (-1)
  '>' -> Move 1
  '<' -> Move (-1)
  '[' -> LoopStart empty
  ']' -> LoopEnd empty
  ',' -> Input
  '.' -> Output
  _   -> error "Invalid BF character (bfInst2SimpleIR)"

inst2Seq :: BFProgram -> BFSeq
inst2Seq = mconcat . map (BFS . singleton . bfInst2SimpleIR) . getBFProgram
