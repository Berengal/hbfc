{-# LANGUAGE LambdaCase #-}
module Language.Brainfuck.Compiler.BFIR where

import           Language.Brainfuck.Parser

import           Data.Foldable
import           Data.Sequence
import           Text.Printf

data BFIR = Modify !Int
          | Move !Int
          | LoopStart (Seq BFIR)
          | LoopEnd (Seq BFIR)
          | Loop BFSeq
          | Input
          | Output
  deriving Eq

instance Show BFIR where
  show = \case
    Modify n    -> printf "(%+d)" n
    Move n      -> printf "(<>%d)" n
    LoopStart s -> printf "{%s" (show (BFS s))
    LoopEnd s   -> printf "%s}" (show (BFS s))
    Loop s      -> printf "[%s]" (show s)
    Input       -> ","
    Output      -> "."

newtype BFSeq = BFS (Seq BFIR) deriving (Eq)

instance Show BFSeq where
  show (BFS s) = concatMap show s

instance Monoid BFSeq where
  mempty = BFS empty

instance Semigroup BFSeq where
  BFS a <> BFS b = BFS $ mergeBFSeq a b

programSize :: BFSeq -> Int
programSize (BFS p) = foldr' ((+) . irSize) 0 p
  where irSize (Loop s)      = programSize s + 2
        irSize (LoopStart s) = programSize (BFS s) + 1
        irSize (LoopEnd s)   = programSize (BFS s) + 1
        irSize _             = 1

isValidBFSeq :: BFSeq -> Bool
isValidBFSeq (BFS bfSeq) = flip all bfSeq $ \case
  LoopStart _     -> False
  LoopEnd _       -> False
  Loop (innerSeq) -> isValidBFSeq innerSeq
  _               -> True

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
  (Empty, b)       -> b
  (a, Empty)       -> a

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
    -> (a |> LoopStart (ia `mergeBFSeq` (singleton x))) `mergeBFSeq` b

  (a :|> x, LoopEnd ib :<| b)
    -> a `mergeBFSeq` (LoopEnd ((singleton x) `mergeBFSeq` ib) <| b)

  (a, x :<| b) -> (a |> x) <> b

inst2Seq :: [BFInst] -> BFSeq
inst2Seq = mconcat . map (BFS . singleton . bfInst2BFIR)
