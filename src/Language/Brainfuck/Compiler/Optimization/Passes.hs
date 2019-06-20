module Language.Brainfuck.Compiler.Optimization.Passes where

import           Language.Brainfuck.Compiler.IR

import           Data.Foldable
import           Data.Function
import           Data.List                      hiding (filter, partition)
import           Data.Sequence                  hiding (sortOn)

data OptimizationPass = SinglePass SinglePass
                      | PairPass PairPass
                      | SequencePass SequencePass
                      | WholeProgramPass WholeProgramPass

type PairPass = IntermediateCode -> IntermediateCode -> Maybe (Seq IntermediateCode)
type SinglePass = IntermediateCode -> Maybe (Seq IntermediateCode)
type SequencePass = Seq IntermediateCode -> Maybe (Seq IntermediateCode)
type WholeProgramPass = Seq IntermediateCode -> Maybe (Seq IntermediateCode)


mergeModifySet :: IntermediateCode -> IntermediateCode -> Maybe (Seq IntermediateCode)
-- Modify zero + move/modify = ignore first, add movement to second
mergeModifySet (Modify 0 offA ) (Modify amtB offB )
  = Just $ singleton (Modify amtB offB)
-- Move/modify same cell = add both modifications
mergeModifySet (Modify amtA offA )  (Modify amtB offB )
  | offA == offB =
    Just $ singleton (Modify (amtA + amtB) offA)
-- Set then modify same cell = add modification to set
mergeModifySet (Set amtA offA ) (Modify amtB offB )
  | offA == offB =
    Just $ singleton (Set (amtA + amtB) offA)
-- Modify then set cell = ignore modify
mergeModifySet (Modify _ offA ) (Set amtB offB )
  | offA == offB =
    Just $ singleton (Set amtB offA)
-- Set then reset = ignore first set
mergeModifySet (Set _ offA ) (Set amtB offB )
  | offA == offB =
    Just $ singleton (Set amtB offA)
mergeModifySet _ _ = Nothing

loopToMult :: SinglePass
loopToMult (Loop off body)
  | all isSetOrModify body
  = Just (fmap makeMult notBase <> fromList [Set 0 off, BaseIndex off])
  where
    (base, notBase) = partition ((==0) . offset) body
    step = negate (foldl' sumSetModify 0 base)
    sumSetModify _ (Set n _)    = n
    sumSetModify n (Modify m _) = n + m
    makeMult (Set amt offM )   = Set amt (off + offM)
    makeMult (Modify amt offM) = Multiply (off + offM) off amt step
loopToMult _ = Nothing

filterZeroBaseIndex :: SinglePass
filterZeroBaseIndex (BaseIndex 0) = Just Empty
filterZeroBaseIndex _             = Nothing

propagateBaseIndex :: SequencePass
propagateBaseIndex = Just . go 0
  where
    go n (BaseIndex off :<| Empty) = BaseIndex (off + n) :<| Empty
    go n (BaseIndex off :<| rest) = go (n + off) rest
    go n (Loop off body :<| rest) = Loop (off + n) body :<| go 0 rest
    go n (Multiply off offFrom scale step :<| rest)
      = Multiply (off + n) (offFrom + n) scale step :<| go n rest
    go n (i :<| rest) = i{offset = offset i + n} :<| go n rest
    go n Empty = Empty

sortModifys :: SequencePass
sortModifys = Just
              . fromList
              . concatMap (applyWhen (isSetOrModify . head) (sortOn offset))
              . groupBy ((==) `on` isSetOrModify)
              . toList
  where
    applyWhen test func x | test x    = func x
                          | otherwise = x


isSetOrModify :: IntermediateCode -> Bool
isSetOrModify (Modify _ _) = True
isSetOrModify (Set _ _)    = True
isSetOrModify _            = False

deadInitialLoop :: WholeProgramPass
deadInitialLoop = Just . dropWhileL isLoop
  where isLoop (Loop _ _) = True
        isLoop _          = False

deadSecondLoop :: PairPass
deadSecondLoop loop@(Loop _ _) (Loop _ _) = Just (singleton loop)
deadSecondLoop _ _                        = Nothing

filterZeroModify :: SinglePass
filterZeroModify (Modify 0 _) = Just Empty
filterZeroModify _            = Nothing
