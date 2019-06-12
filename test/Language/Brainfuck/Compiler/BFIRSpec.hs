{-# LANGUAGE BlockArguments #-}
module Language.Brainfuck.Compiler.BFIRSpec where

import           Control.Monad
import           Data.Sequence                    hiding (replicateM)
import           Prelude                          hiding (splitAt)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                  hiding ((><))

import           Language.Brainfuck.Compiler.BFIR
import           Language.Brainfuck.Parser

spec :: Spec
spec = do
  describe "BFSeq" $ do
    modifyMaxSuccess (const 1000) $ describe "mappend" $ do
      it "uncurry mappend . splitAt n = id"
        (property prop_BFSeq_mappendId)

      it "should produce valid sequences from valid source"
        (property prop_BFSeq_validFromValid)

prop_BFSeq_mappendId (BFS sequence) n =
  uncurry mappend (splitAt n sequence) == sequence

prop_BFSeq_validFromValid (ValidBFInst xs) =
  isValidBFSeq (inst2Seq xs)

instance Arbitrary BFSeq where
  arbitrary = sized $ \size -> do
    loopSize <- round <$> choose (0 :: Double, fromIntegral size / 1.25)
    loop <- if loopSize == 0
            then return Empty
            else resize loopSize (singleton . Loop <$> arbitrary)
    preSize <- choose (0, size - loopSize)
    let postSize = size - preSize - loopSize

    let chooseIR = oneof [ Modify <$> arbitrary
                         , Move <$> arbitrary
                         , pure Input
                         , pure Output
                         ]

    preSequence <- fromList <$> replicateM preSize chooseIR
    postSequence <- fromList <$> replicateM postSize chooseIR
    return (BFS (preSequence <> loop <> postSequence))

newtype ValidBFInst = ValidBFInst {getValidBFInst :: [BFInst]}
  deriving Show

instance Arbitrary ValidBFInst where
  arbitrary = sized $ \size ->
    if size == 0 then return (ValidBFInst ([])) else do

    loopSize <- if size < 5
                then return 0
                else round <$> choose (0 :: Double, fromIntegral size / 1.25)
    loop <- if size < 5
                then return []
                else getValidBFInst <$> resize loopSize arbitrary

    preSize <- choose (0, size - loopSize)
    let postSize = size - loopSize - preSize

    let chooseInst = elements [IncD, DecD, DRig, DLef, Inp, Out]

    preList <- replicateM preSize chooseInst
    postList <- replicateM postSize chooseInst
    return (ValidBFInst (preList ++ [JmpF] ++ loop ++ [JmpB] ++ postList))

newtype UnknownBFInst = UnknownBFInst {getInvalidBFInst :: [BFInst]}
  deriving Show

instance Arbitrary UnknownBFInst where
  arbitrary = UnknownBFInst <$> listOf (elements [IncD, DecD, DRig, DLef, Inp, Out])
