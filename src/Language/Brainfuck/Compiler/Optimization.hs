module Language.Brainfuck.Compiler.Optimization where

import           Language.Brainfuck.Compiler.IR
import           Language.Brainfuck.Compiler.Optimization.Passes

import           Data.Foldable
import           Data.Maybe
import           Data.Sequence                                   hiding (sortOn)
import           Prelude                                         hiding (filter)


multiplyPass :: [OptimizationPass]
multiplyPass = [ SinglePass filterZeroBaseIndex
               , SinglePass loopToMult
               , SequencePass propagateBaseIndex
               ]

reorderModifyPass :: [OptimizationPass]
reorderModifyPass = [ SequencePass sortModifys
                    , PairPass mergeModifySet
                    ]

deadCodePass :: [OptimizationPass]
deadCodePass = [ WholeProgramPass deadInitialLoop
               , PairPass deadSecondLoop
               , SinglePass filterZeroBaseIndex
               , SinglePass filterZeroModify
               ]

basePass :: [OptimizationPass]
basePass = [ PairPass mergeModifySet
           ]
simpleOptimize :: [OptimizationPass]
simpleOptimize = basePass ++ reorderModifyPass
mediumOptimize :: [OptimizationPass]
mediumOptimize = simpleOptimize
                 ++ deadCodePass
                 ++ reorderModifyPass
                 ++ multiplyPass
                 ++ reorderModifyPass
aggressiveOptimize :: [OptimizationPass]
aggressiveOptimize = mediumOptimize



runOptimizationPasses :: [OptimizationPass] -> Seq IntermediateCode -> Seq IntermediateCode
runOptimizationPasses passes program = foldl' doPass program passes
  where
    doPass program (SinglePass pass)       = runSinglePass pass program
    doPass program (PairPass pass)         = runPairPass pass program
    doPass program (SequencePass pass)     = runSequencePass pass program
    doPass program (WholeProgramPass pass) = runWholeProgramPass pass program

runWholeProgramPass :: WholeProgramPass -> Seq IntermediateCode -> Seq IntermediateCode
runWholeProgramPass pass program = fromMaybe program (pass program)

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
runPairPass :: PairPass -> Seq IntermediateCode -> Seq IntermediateCode
runPairPass pass (Loop off body :<|y :<|rest)
  = let loop' = Loop off (runPairPass pass body)
    in case pass loop' y of
      Nothing          -> loop' :<|runPairPass pass (y :<|rest)
      Just replacement -> runPairPass pass (replacement <> rest)
runPairPass pass (Loop off body :<| Empty)
  = singleton (Loop off (runPairPass pass body))
runPairPass pass (x :<|y :<|rest)
  = case pass x y of
      Nothing          -> x :<|runPairPass pass (y :<|rest)
      Just replacement -> runPairPass pass (replacement <> rest)
runPairPass _ rest = rest

-- Runs a single-instruction pass over the sequence. Loops are passed twice:
-- Once before any pass is run on the loop body and once after.
runSinglePass :: SinglePass -> Seq IntermediateCode -> Seq IntermediateCode
runSinglePass pass (loop@(Loop off body) :<|rest)
  = case pass loop of
      Just replacement -> runSinglePass pass (replacement <> rest)
      Nothing -> let loop' = Loop off (runSinglePass pass body)
                 in case pass loop' of
                      Nothing -> loop' :<|runSinglePass pass rest
                      Just replacement -> replacement <> runSinglePass pass rest
runSinglePass pass (x :<|rest)
  = fromMaybe (singleton x) (pass x) <> runSinglePass pass rest
runSinglePass _ Empty = Empty

-- Runs the pass on every sequence recursively. Inner sequences are processed
-- before outer ones.
runSequencePass :: SequencePass -> Seq IntermediateCode -> Seq IntermediateCode
runSequencePass pass seq =
  let seq' = fmap (onLoops (runSequencePass pass)) seq
  in case pass seq' of
    Nothing          -> seq'
    Just replacement -> replacement
  where
    onLoops func (Loop off body) = Loop off (func body)
    onLoops _ whatever               = whatever
