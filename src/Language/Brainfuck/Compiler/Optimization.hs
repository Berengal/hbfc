module Language.Brainfuck.Compiler.Optimization where

import           Language.Brainfuck.Compiler.AdvancedIR

import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe

data OptimizationPass = SinglePass SinglePass
                      | PairPass PairPass
                      | SequencePass SequencePass
type PairPass = AdvancedIR -> AdvancedIR -> Maybe ([AdvancedIR])
type SinglePass = AdvancedIR -> Maybe ([AdvancedIR])
type SequencePass = [AdvancedIR] -> Maybe ([AdvancedIR])

runOptimizationPasses :: [OptimizationPass] -> [AdvancedIR] -> [AdvancedIR]
runOptimizationPasses passes program = foldl' doPass program passes
  where
    doPass program (SinglePass pass)   = runSinglePass pass program
    doPass program (PairPass pass)     = runPairPass pass program
    doPass program (SequencePass pass) = runSequencePass pass program

basePass = [PairPass mergeModifySet]

multiplyPass = [ SinglePass findLoopMovement
               , SinglePass loopToMult
               , SequencePass propagateBaseIndex
               ]

reorderModifyPass = [ SequencePass sortModifys
                    , PairPass mergeModifySet
                    ]

simpleOptimize = basePass ++ reorderModifyPass
mediumOptimize = simpleOptimize ++ multiplyPass ++ reorderModifyPass

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
runPairPass :: PairPass -> [AdvancedIR] -> [AdvancedIR]
runPairPass pass (Loop off knownMovement body : y : rest)
  = let loop' = Loop off knownMovement (runPairPass pass body)
    in case pass loop' y of
      Nothing          -> loop' : runPairPass pass (y : rest)
      Just replacement -> runPairPass pass (replacement <> rest)
runPairPass pass (Loop off knownMovement body : [])
  = [Loop off knownMovement (runPairPass pass body)]
runPairPass pass (x : y : rest)
  = case pass x y of
      Nothing          -> x : runPairPass pass (y : rest)
      Just replacement -> runPairPass pass (replacement <> rest)
runPairPass _ rest = rest

-- Runs a single-instruction pass over the sequence. Loops are passed twice:
-- Once before any pass is run on the loop body and once after.
runSinglePass :: SinglePass -> [AdvancedIR] -> [AdvancedIR]
runSinglePass pass (loop@(Loop off knownMovement body) : rest)
  = case pass loop of
      Just replacement -> runSinglePass pass (replacement <> rest)
      Nothing -> let loop' = Loop off knownMovement (runSinglePass pass body)
                 in case pass loop' of
                      Nothing -> loop' : runSinglePass pass rest
                      Just replacement -> replacement <> runSinglePass pass rest
runSinglePass pass (x : rest)
  = fromMaybe [x] (pass x) <> runSinglePass pass rest
runSinglePass _ [] = []

runSequencePass :: SequencePass -> [AdvancedIR] -> [AdvancedIR]
runSequencePass pass seq =
  let seq' = fmap (onLoops (runSequencePass pass)) seq
  in case pass seq' of
    Nothing          -> seq'
    Just replacement -> replacement
  where
    onLoops func (Loop off mov body) = Loop off mov (func body)
    onLoops _ whatever               = whatever

mergeModifySet :: AdvancedIR -> AdvancedIR -> Maybe ([AdvancedIR])

-- Modify zero + move/modify = ignore first, add movement to second
mergeModifySet (Modify 0 offASinglePassr movA) (Modify amtB offB movB)
  = Just [Modify amtB offB (movA + movB)]

-- Move/modify same cell = add both modifications
mergeModifySet (Modify amtA offA movA)  (Modify amtB offB movB)
  | offA == offB =
    Just [Modify (amtA + amtB) offA (movA + movB)]

-- Set then modify same cell = add modification to set
mergeModifySet (Set amtA offA movA) (Modify amtB offB movB)
  | offA == offB =
    Just [Set (amtA + amtB) offA (movA + movB)]

-- Modify then set cell = ignore modify
mergeModifySet (Modify _ offA movA) (Set amtB offB movB)
  | offA == offB =
    Just [Set amtB offA (movA + movB)]

-- Set then reset = ignore first set
mergeModifySet (Set _ offA movA) (Set amtB offB movB)
  | offA == offB =
    Just [Set amtB offA (movA + movB)]
mergeModifySet _ _ = Nothing

findLoopMovement :: SinglePass
findLoopMovement (Loop off Unknown body)
  = let innerSum = sum (map relativeMovement body)
    in if innerSum == 0
       then Just $ [(Loop off 0 body)]
       else Nothing
findLoopMovement _ = Nothing

loopToMult :: SinglePass
loopToMult (Loop off 0 body)
  | all isSetOrModify body
  = Just (map makeMult notBase ++ [Set 0 off 0, BaseIndex off 0])
  where
    (base, notBase) = partition ((==0) . offset) body
    step = negate (foldl' sumSetModify 0 base)
    sumSetModify _ (Set n _ _)    = n
    sumSetModify n (Modify m _ _) = n + m
    makeMult (Set amt offM _)    = Set amt (off + offM) 0
    makeMult (Modify amt offM _) = Multiply (off + offM) off amt step 0
loopToMult _ = Nothing

propagateBaseIndex :: SequencePass
propagateBaseIndex = Just . go 0
  where go n (BaseIndex off _ : rest) = go (n + off) rest
        go n (i:is)                   = i{offset = offset i + n} : go n is
        go n []                       = []

sortModifys :: SequencePass
sortModifys = Just
              . concatMap (sortOn offset)
              . groupBy ((==) `on` isSetOrModify)


isSetOrModify :: AdvancedIR -> Bool
isSetOrModify (Modify _ _ _) = True
isSetOrModify (Set _ _ _)    = True
isSetOrModify _              = False
