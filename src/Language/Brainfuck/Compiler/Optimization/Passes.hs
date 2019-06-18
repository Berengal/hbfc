module Language.Brainfuck.Compiler.Optimization.Passes where

import Language.Brainfuck.Compiler.IR

import Data.Sequence hiding (sortOn)
import Data.Foldable
import Data.List hiding (filter, partition)
import Data.Function

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
mergeModifySet (Modify 0 offA movA) (Modify amtB offB movB)
  = Just $ singleton (Modify amtB offB (movA + movB))
-- Move/modify same cell = add both modifications
mergeModifySet (Modify amtA offA movA)  (Modify amtB offB movB)
  | offA == offB =
    Just $ singleton (Modify (amtA + amtB) offA (movA + movB))
-- Set then modify same cell = add modification to set
mergeModifySet (Set amtA offA movA) (Modify amtB offB movB)
  | offA == offB =
    Just $ singleton (Set (amtA + amtB) offA (movA + movB))
-- Modify then set cell = ignore modify
mergeModifySet (Modify _ offA movA) (Set amtB offB movB)
  | offA == offB =
    Just $ singleton (Set amtB offA (movA + movB))
-- Set then reset = ignore first set
mergeModifySet (Set _ offA movA) (Set amtB offB movB)
  | offA == offB =
    Just $ singleton (Set amtB offA (movA + movB))
mergeModifySet _ _ = Nothing

findLoopMovement :: SinglePass
findLoopMovement (Loop off Unknown body)
  = let innerSum = sum (fmap relativeMovement body)
    in if innerSum == 0
       then Just $ singleton (Loop off 0 body)
       else Nothing
findLoopMovement _ = Nothing

loopToMult :: SinglePass
loopToMult (Loop off 0 body)
  | all isSetOrModify body
  = Just (fmap makeMult notBase <> fromList [Set 0 off 0, BaseIndex off 0])
  where
    (base, notBase) = partition ((==0) . offset) body
    step = negate (foldl' sumSetModify 0 base)
    sumSetModify _ (Set n _ _)    = n
    sumSetModify n (Modify m _ _) = n + m
    makeMult (Set amt offM _)    = Set amt (off + offM) 0
    makeMult (Modify amt offM _) = Multiply (off + offM) off amt step 0
loopToMult _ = Nothing

filterZeroBaseIndex :: SinglePass
filterZeroBaseIndex (BaseIndex 0 _ ) = Just Empty
filterZeroBaseIndex _ = Nothing

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
isSetOrModify (Modify _ _ _) = True
isSetOrModify (Set _ _ _)    = True
isSetOrModify _              = False

deadInitialLoop :: WholeProgramPass
deadInitialLoop = Just . dropWhileL isLoop
  where isLoop (Loop _ _ _) = True
        isLoop _ = False

deadSecondLoop :: PairPass
deadSecondLoop loop@(Loop _ _ _) (Loop _ _ _) = Just (singleton loop)
deadSecondLoop _ _ = Nothing

filterZeroModify :: SinglePass
filterZeroModify (Modify 0 _ 0) = Just Empty
filterZeroModify _ = Nothing
