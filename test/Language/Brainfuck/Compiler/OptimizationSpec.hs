module Language.Brainfuck.Compiler.OptimizationSpec where

import           Language.Brainfuck.Compiler.AdvancedIR
import           Language.Brainfuck.Compiler.Optimization
import           Language.Brainfuck.Compiler.Optimization.Passes
import           Language.Brainfuck.Parser

import           Data.Sequence
import           Test.Hspec

spec = describe "Optimizations" $ do
  describe "loopToMult optimization" $ do
    it "should convert simple loops to multiplications" $ do
      let (Just program) = runPairPass mergeModifySet . fromBFProgram <$> parse "[>+++>++<<-]"
          expected = fromList [Multiply 1 0 3 1 0, Multiply 2 0 2 1 0, Set 0 0 0, BaseIndex 0 0]
          actual = runOptimizationPasses multiplyPass program
      actual `shouldBe` expected
