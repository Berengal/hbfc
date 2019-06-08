module Language.Brainfuck.Compiler.Options where

import Data.Word
import System.IO

data CodeGenOptions = CGO
  { dataSize :: DataArraySize
  , dataStart :: Word32
  , cellSize :: CellSize
  } deriving Show

data CompilerOptions = CO
  { inputSource :: FilePath
  , outputFormat :: OutputFormat
  , outputDestination :: Maybe FilePath
  , optimizationLevel :: OptimizationLevel
  , codeGenOptions :: CodeGenOptions
  } deriving Show

data DataArraySize = InfiniteSizeArray
                   | FiniteSizeArray Word32
  deriving Show

data CellSize = I8 | I32 | I64 | Unbounded
  deriving Show

data OutputFormat = IRAssembly | IRBitCode | NativeAssembly | Object
  deriving Show

data OptimizationLevel = None | Simple | Medium | Aggressive
  deriving Show

defaultDataArraySize = FiniteSizeArray 30000
defaultDataArrayPosition = 0
defaultCellSize = I8
defaultOutputFormat = Object
defaultOutputDestination = Nothing
defaultOptimizationLevel = None

defaultCodeGenOptions = CGO
  { dataSize = defaultDataArraySize
  , dataStart = defaultDataArrayPosition
  , cellSize = defaultCellSize}

defaultCompilerOpts inputSource = CO
  { inputSource = inputSource
  , outputFormat = defaultOutputFormat
  , outputDestination = defaultOutputDestination
  , optimizationLevel = defaultOptimizationLevel
  , codeGenOptions = defaultCodeGenOptions
  }

