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
  = Modify    { modifyAmount :: Int
              , offset       :: Int
              }
  | Set       { setAmount :: Int
              , offset    :: Int
              }
    -- | Multiply = offset + (offsetFrom / step) * scale
  | Multiply  { offset     :: Int
              , offsetFrom :: Int
              , scale      :: Int
              , step       :: Int
              }
  | Input     { offset           :: Int
              }
  | Output    { offset           :: Int
              }
  | BaseIndex { offset           :: Int -- ^ Sets the base index to be = the offset
              }
  | Loop      { offset :: Int
              , body   :: Seq IntermediateCode
              }
  deriving (Show, Eq)


fromBFProgram :: BFProgram -> Seq IntermediateCode
fromBFProgram (BFProgram prog) = fst (evalState (go prog) 0)
  where
    go :: String -> State Int (Seq IntermediateCode, String)
    go ('+':r) = do
      off <- get
      (rest, r') <- go r
      return (Modify 1 off :<| rest, r')
    go ('-':r) = do
      off <- get
      (rest, r') <- go r
      return (Modify (-1) off :<| rest, r')
    go ('>':r) = do
      off <- get
      put (off+1)
      (rest, r') <- go r
      return (Modify 0 (off+1) :<| rest, r')
    go ('<':r) = do
      off <- get
      put (off-1)
      (rest, r') <- go r
      return (Modify 0 (off-1) :<| rest , r')
    go (',':r) = do
      off <- get
      (rest, r') <- go r
      return (Input off :<| rest, r')
    go ('.':r) = do
      off <- get
      (rest, r') <- go r
      return (Output off :<| rest, r')
    go ('[':r) = do
      off <- get
      put 0
      (body, r') <- go r
      index <- get
      put 0
      (rest, r'') <- go r'
      return (Loop off (body |> BaseIndex index ) :<| rest, r'')
    go (']':r) =
      return (Empty, r)
    go [] = return (Empty, [])
    go _ = error "Invalid BFProgram (IR.fromBFProgram)"
