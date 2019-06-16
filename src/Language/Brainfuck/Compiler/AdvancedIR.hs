module Language.Brainfuck.Compiler.AdvancedIR where

import qualified Language.Brainfuck.Compiler.BFIR as BFIR

import           Language.Brainfuck.Parser

import           Control.Monad.State
import           Data.Sequence


-- Movement commands are turned into offsets instead. Loops may require the base
-- index to be reset (i.e. when knownMovement is false)
data AdvancedIR
  = Modify { modifyAmount :: Int
           , offset       :: Int
           }
  | Set    { setAmount :: Int
           , offset    :: Int
           }
  | Input  { offset        :: Int
           }
  | Output { offset        :: Int
           }
  | Loop   { offset        :: Int
           , knownMovement :: Bool
           , body          :: Seq AdvancedIR
           }
  deriving Show

data RelativeMovement = Known Int
                      | Unknown
  deriving (Eq, Show)


fromBFProgram :: BFProgram -> Seq AdvancedIR
fromBFProgram (BFProgram prog) = fst (evalState (go prog) 0)
  where
    go :: String -> State Int (Seq AdvancedIR, String)
    go ('+':r) = do
      off <- get
      (rest, r') <- go r
      return (Modify 1 off <| rest, r')
    go ('-':r) = do
      off <- get
      (rest, r') <- go r
      return (Modify (-1) off <| rest, r')
    go ('>':r) = do
      off <- get
      put (off+1)
      (rest, r') <- go r
      return (Modify 0 (off+1) <| rest, r')
    go ('<':r) = do
      off <- get
      put (off-1)
      (rest, r') <- go r
      return (Modify 0 (off-1) <| rest , r')
    go (',':r) = do
      off <- get
      (rest, r') <- go r
      return (Input off <| rest, r')
    go ('.':r) = do
      off <- get
      (rest, r') <- go r
      return (Output off <| rest, r')
    go ('[':r) = do
      off <- get
      put 0
      (body, r') <- go r
      put 0
      (rest, r'') <- go r'
      return (Loop off False body <| rest, r'')
    go (']':r) =
      return (Empty, r)
    go [] = return (Empty, [])

fromSimpleIR :: Seq BFIR.SimpleIR -> Seq AdvancedIR
fromSimpleIR = flip evalState 0 . mapM translateSingle

translateSingle :: BFIR.SimpleIR -> State Int AdvancedIR
translateSingle (BFIR.Modify n) = do
  offset <- get
  return (Modify n offset)
translateSingle (BFIR.Move n) = do
  offset <- get
  put (offset + n)
  return (Modify 0 (offset + n))
translateSingle BFIR.Input = gets Input
translateSingle BFIR.Output = gets Output
translateSingle (BFIR.Loop (BFIR.BFS innerSeq)) = do
  offset <- get
  body <- mapM translateSingle innerSeq
  put 0
  return (Loop offset False body)
translateSingle _ = error "Partial simple IR (translateSingle)"

mergeMoveModifySet :: AdvancedIR -> AdvancedIR -> Maybe (Seq AdvancedIR)

-- Move/modify same cell = add both modifications
mergeMoveModifySet (Modify amtA offA)  (Modify amtB offB)
  | offA == offB =
    Just $ singleton (Modify (amtA + amtB) offA)

-- Set then modify same cell = add modification to set
mergeMoveModifySet (Set amtA offA) (Modify amtB offB)
  | offA == offB =
    Just $ singleton (Set (amtA + amtB) offA)

-- Modify then set cell = ignore modify
mergeMoveModifySet (Modify _ offA) (Set amtB offB)
  | offA == offB =
    Just $ singleton (Set amtB offA)

-- Set then reset = ignore first set
mergeMoveModifySet (Set _ offA) (Set amtB offB)
  | offA == offB =
    Just $ singleton (Set amtB offA)
mergeMoveModifySet _ _ = Nothing

data OptimizationPass = PairPass PairPass
type PairPass = AdvancedIR -> AdvancedIR -> Maybe (Seq AdvancedIR)

-- Gives the pass two instructions at a time. If the pass does nothing every
-- instruction will be passed once as the second argument, then once more as the
-- first. If the pass does replace the pair, the replacement will be
-- concatenated to the front of the rest of the instructions, and the pass will
-- resume from the start of its own replacement.
-- Loops will be given once as the
-- second argument, then the pass will be run on the loop, then the transformed
-- loop will be given as the first argument.
-- The first instruction will only be given once, as the first argument, and
-- likewise tha last instruction will only be applied once, as the second argument.
runPairPass :: PairPass -> Seq AdvancedIR -> Seq AdvancedIR
runPairPass pass (Loop off rel body :<| y :<| rest)
  = let loop' = Loop off rel (runPairPass pass body)
    in case pass loop' y of
      Nothing          -> loop' <| runPairPass pass (y <| rest)
      Just replacement -> runPairPass pass (replacement <> rest)
runPairPass pass (Loop off rel body :<| Empty)
  = singleton (Loop off rel (runPairPass pass body))
runPairPass pass (x :<| y :<| rest)
  = case pass x y of
      Nothing          -> x <| runPairPass pass (y <| rest)
      Just replacement -> runPairPass pass (replacement <> rest)
runPairPass pass rest = rest
