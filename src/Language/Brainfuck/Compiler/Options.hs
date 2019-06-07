
module Language.Brainfuck.Compiler.Options where

import Data.Word
import System.IO

data CompilerOpts = CO
  { dataArray :: DataArrayOpt
  , cellSize :: CellSize
  , inputSource :: Handle
  , outputFormat :: OutputFormat
  , outputDestination :: Handle
  } deriving (Show)

data DataArrayOpt = ArbitrarySizeArray
                  | FiniteSizeArray {size :: Word32, start :: Word32}
  deriving Show

data CellSize = I8 | I32 | I64 | Unbounded
  deriving Show

data OutputFormat = IRAssembly | IRBitCode | NativeAssembly | Object
  deriving Show

