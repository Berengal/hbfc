module Language.Brainfuck.Compiler.Options where

import           Data.Word

data CodeGenOptions = CGO
  { dataSize    :: DataArraySize
  , dataStart   :: Word32
  , cellSize    :: CellSize
  , eofBehavior :: EofBehavior
  } deriving (Show, Eq)

data CompilerOptions = CO
  { inputSource       :: FilePath
  , outputFormat      :: OutputFormat
  , outputDestination :: Maybe FilePath
  , optimizationLevel :: OptimizationLevel
  , llvmOptimization  :: OptimizationLevel
  , codeGenOptions    :: CodeGenOptions
  } deriving (Show, Eq)

data DataArraySize = FiniteSizeArray Word32
                   | InfiniteSizeArray
  deriving (Show, Eq)

data CellSize = I8 | I32 | I64 | Unbounded
  deriving (Show, Eq, Ord)

data EofBehavior = NoChange
                 | SetZero
                 | SetEOF
  deriving (Show, Eq, Ord)

data OutputFormat = IRAssembly | IRBitCode | NativeAssembly | Object | Executable
  deriving (Show, Eq)

data OptimizationLevel = None | Simple | Medium | Aggressive
  deriving (Show, Eq, Ord)

defaultDataArraySize     = FiniteSizeArray 30000
defaultDataArrayPosition = 0
defaultCellSize          = I32
defaultEofBehavior       = NoChange
defaultOutputFormat      = Executable
defaultOutputDestination = Nothing
defaultOptimizationLevel = Simple

defaultCodeGenOptions = CGO
  { dataSize    = defaultDataArraySize
  , dataStart   = defaultDataArrayPosition
  , cellSize    = defaultCellSize
  , eofBehavior = defaultEofBehavior
  }

defaultCompilerOpts inputSource = CO
  { inputSource       = inputSource
  , outputFormat      = defaultOutputFormat
  , outputDestination = defaultOutputDestination
  , optimizationLevel = defaultOptimizationLevel
  , llvmOptimization  = defaultOptimizationLevel
  , codeGenOptions    = defaultCodeGenOptions
  }
