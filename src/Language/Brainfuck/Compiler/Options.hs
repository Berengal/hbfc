module Language.Brainfuck.Compiler.Options where

import Data.Word
import System.IO

data CodeGenOpts = CGO
  { dataArray :: DataArrayOpt
  , cellSize :: CellSize
  } deriving Show

data CompilerOpts = CO
  { inputSource :: FilePath
  , outputFormat :: OutputFormat
  , outputDestination :: Maybe FilePath
  , optLevel :: OptLevel
  , codeGenOpts :: CodeGenOpts
  } deriving Show

data DataArrayOpt = ArbitrarySizeArray
                  | FiniteSizeArray {size :: Word32, start :: Word32}
  deriving Show

data CellSize = I8 | I32 | I64 | Unbounded
  deriving Show

data OutputFormat = IRAssembly | IRBitCode | NativeAssembly | Object
  deriving Show

data OptLevel = None | Simple | Medium | Aggressive
  deriving Show

defaultDataArraySize = 30000
defaultDataArrayPosition = 0
defaultDataArrayOpt = FiniteSizeArray
  { size = defaultDataArraySize
  , start = defaultDataArrayPosition}
defaultCellSize = I8
defaultOutputFormat = Object
defaultOptLevel = None

defaultCodeGenOpts = CGO
  { dataArray = defaultDataArrayOpt
  , cellSize = defaultCellSize}

defaultCompilerOpts inputSource = CO
  { inputSource = inputSource
  , outputFormat = defaultOutputFormat
  , outputDestination = Nothing
  , optLevel = defaultOptLevel
  , codeGenOpts = defaultCodeGenOpts
  }

