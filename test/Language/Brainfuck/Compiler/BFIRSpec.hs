{-# LANGUAGE BlockArguments #-}
module Language.Brainfuck.Compiler.BFIRSpec where

import           Data.Sequence
import           Prelude                          hiding (splitAt)
import           Test.Hspec
import           Test.QuickCheck

import           Language.Brainfuck.Compiler.BFIR

spec :: Spec
spec = do
  describe "BFSeq" $ do
    describe "mappend" $ do
      it "should be true that mappend . splitAt n = id"
        (property prop_BFSeq_mappendId)

prop_BFSeq_mappendId (BFS sequence) n =
  uncurry mappend (splitAt n sequence) == sequence

instance Arbitrary BFSeq where
  arbitrary = BFS <$> arbitrary

instance Arbitrary BFIR where
  arbitrary = frequency [ (3, Modify <$> arbitrary)
                        , (3, Move <$> arbitrary)
                        , (1, pure Input)
                        , (1, pure Output)
                        , (2, getSize >>= \size ->
                            resize
                            (round (fromIntegral size * 0.50))
                            (Loop <$> arbitrary))
                        ]

