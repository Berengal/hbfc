{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module BFUtils where

import           Language.Brainfuck.Compiler.Options

import qualified LLVM.CodeGenOpt                     as CodeGenOpt
import qualified LLVM.CodeModel                      as CodeModel
import qualified LLVM.Relocation                     as Reloc
import           LLVM.Target

import           Data.Maybe
import           Options.Applicative
import           System.FilePath


withTargetMachineOptions :: OptimizationLevel -> (TargetMachine -> IO a) -> IO a
withTargetMachineOptions optimizationLevel action = do
  initializeNativeTarget
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options -> do
  withTargetMachine
    target
    triple
    cpu
    features
    options
    Reloc.PIC
    CodeModel.Default
    optLevel
    action
  where
    optLevel = case optimizationLevel of
      None       -> CodeGenOpt.None
      Simple     -> CodeGenOpt.Less
      Medium     -> CodeGenOpt.Default
      Aggressive -> CodeGenOpt.Aggressive

opts :: ParserInfo CompilerOptions
opts = info (compilerOptions <**> helper)
  ( fullDesc
  <> progDesc "Compile a brainfuck program"
  <> header "bfc - An optimizing brainfuck compiler")

compilerOptions :: Parser CompilerOptions
compilerOptions = do
  optimizationLevel <- option optl
    (short 'O'
    <> help "Optimization level (0-3)"
    <> value defaultOptimizationLevel)
  llvmOptimization <- (Just <$> option optl
    (long "llvm-opt"
    <> help "LLVM optimization level (0-3). Same as regular optimization by default"))
    <|> pure Nothing
  codeGenOptions <- do
    datasize <- option arrs
      ( long "datasize"
        <> short 's'
        <> value defaultDataArraySize
        <> help "Array size in number of cells (or infinite/0)")
    pos <- option auto
      ( long "position"
        <> short 'p'
        <> value defaultDataArrayPosition
        <> help "Start position of data pointer")
    cellsize <- option cell
      ( long "cellsize"
        <> short 'b'
        <> value defaultCellSize
        <> help "Cell size in number of bits (8|32|64|unbounded/0)")
    eofBehavior <- option eof
      ( long "eof-behavior"
        <> value defaultEofBehavior
        <> help ("Behavior when reading EOF (0|EOF|no-change)\n"
                 ++ "Determined the value that will be read in when the input stream"
                 ++ " has reached EOF. 0 sets it to zero, EOF sets it to -1 and"
                 ++ " no-change leaves it unchanged."))
    pure (CGO datasize pos cellsize eofBehavior)
  outputFormat <- option outf
    ( long "format"
      <> short 'f'
      <> value defaultOutputFormat
      <> help "Output format (ll|bc|s|o|exe)")
  outputDestination <- (Just <$> strOption
    ( short 'o'
      <> metavar "FILE"
      <> help "Output file (use '-' for stdout)"
    )) <|> pure Nothing
  inputSource <- strArgument
    (metavar "SOURCE"
    <> help "input file")
  pure CO{ outputDestination = getOutDest outputDestination inputSource outputFormat
         , llvmOptimization = fromMaybe optimizationLevel llvmOptimization
         ,..}
  where
    arrs = eitherReader $ \case
      ('i':_) -> Right InfiniteSizeArray
      "0"     -> Right InfiniteSizeArray
      (reads -> [(s,[])]) -> Right (FiniteSizeArray s)
      _       -> Left "Invalid array size. Valid sizes: (1-(2^32-1)|infinite/0)"
    optl = eitherReader $ \case
      "0" -> Right None
      "1" -> Right Simple
      "2" -> Right Medium
      "3" -> Right Aggressive
      _   -> Left "Invalid optimization level. Valid levels: (0-3)"
    cell = eitherReader $ \case
      "8"  -> Right I8
      "32" -> Right I32
      "64" -> Right I64
      "0"  -> Right Unbounded
      ('u':_) -> Right Unbounded
      _    -> Left "Invalid cell size. Valid sizes: (8|32|64|0|unbounded)"
    eof = eitherReader $ \case
      "0"     -> Right SetZero
      "EOF"   -> Right SetEOF
      ('n':_) -> Right NoChange
      _       -> Left "Invalid eof behavior. Valid behaviors: (0|EOF|no-change)"
    outf = eitherReader $ \case
      "ll"    -> Right IRAssembly
      "bc"    -> Right IRBitCode
      "s"     -> Right NativeAssembly
      "o"     -> Right Object
      ('e':_) -> Right Executable
      _       -> Left "Invalid output format. Valid formats: (ll|bc|s|o|exe)"
    getOutDest (Just "-") _ _  = Nothing
    getOutDest (Just dest) _ _ = Just dest
    getOutDest _ source format = Just (getTargetName source format)

getTargetName :: FilePath -> OutputFormat -> FilePath
getTargetName source format = takeBaseName source `replaceExtension` formatExt
  where
    formatExt = case format of
      IRAssembly     -> "ll"
      IRBitCode      -> "bc"
      NativeAssembly -> "s"
      Object         -> "o"
      Executable     -> ""
