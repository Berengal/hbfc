module Language.Brainfuck.Compiler.IR where

import           Language.Brainfuck.Parser

import           Control.Monad.State
import           Data.Sequence


-- | Movement commands are turned into offsets instead. The offsets are based
-- off of a base index, which is updated by loops with unknown movement or the
-- BaseIndex instruction. In addition to the basic instructions (modify, input,
-- output and loop) there are also Set and Multiply instructions produced by
-- optimization passes.
data IntermediateCode
  = Modify    { modifyAmount     :: Int
              , offset           :: Int
              , relativeMovement :: RelativeMovement
              }
  | Set       { setAmount        :: Int
              , offset           :: Int
              , relativeMovement :: RelativeMovement
              }
    -- | Multiply = offset + (offsetFrom / step) * scale
  | Multiply  { offset           :: Int
              , offsetFrom       :: Int
              , scale            :: Int
              , step             :: Int
              , relativeMovement :: RelativeMovement
              }
  | Input     { offset           :: Int
              , relativeMovement :: RelativeMovement
              }
  | Output    { offset           :: Int
              , relativeMovement :: RelativeMovement
              }
  | BaseIndex { offset           :: Int -- ^ Sets the base index to be = the offset
              , relativeMovement :: RelativeMovement
              }
  | Loop      { offset           :: Int
              , relativeMovement :: RelativeMovement
              , body             :: Seq IntermediateCode
              }
  deriving (Show, Eq)

data RelativeMovement = Known Int
                      | Unknown
  deriving (Eq, Show)

doRelMov _ Unknown _           = Unknown
doRelMov _ _ Unknown           = Unknown
doRelMov f (Known a) (Known b) = Known (f a b)
instance Num RelativeMovement where
  a + b = doRelMov (+) a b
  a - b = doRelMov (-) a b
  a * b = doRelMov (*) a b
  fromInteger = Known . fromInteger
  abs (Known n) = Known (abs n)
  abs Unknown   = Unknown
  signum (Known n) = Known (signum n)
  signum Unknown   = Unknown

fromBFProgram :: BFProgram -> Seq IntermediateCode
fromBFProgram (BFProgram prog) = fst (evalState (go prog) 0)
  where
    go :: String -> State Int (Seq IntermediateCode, String)
    go ('+':r) = do
      off <- get
      (rest, r') <- go r
      return (Modify 1 off (Known 0) :<| rest, r')
    go ('-':r) = do
      off <- get
      (rest, r') <- go r
      return (Modify (-1) off (Known 0) :<| rest, r')
    go ('>':r) = do
      off <- get
      put (off+1)
      (rest, r') <- go r
      return (Modify 0 (off+1) (Known 1) :<| rest, r')
    go ('<':r) = do
      off <- get
      put (off-1)
      (rest, r') <- go r
      return (Modify 0 (off-1) (Known (-1)) :<| rest , r')
    go (',':r) = do
      off <- get
      (rest, r') <- go r
      return (Input off (Known 0) :<| rest, r')
    go ('.':r) = do
      off <- get
      (rest, r') <- go r
      return (Output off (Known 0) :<| rest, r')
    go ('[':r) = do
      off <- get
      put 0
      (body, r') <- go r
      index <- get
      put 0
      (rest, r'') <- go r'
      return (Loop off Unknown (body |> BaseIndex index 0) :<| rest, r'')
    go (']':r) =
      return (Empty, r)
    go [] = return (Empty, [])
    go _ = error "Invalid BFProgram (IR.fromBFProgram)"
