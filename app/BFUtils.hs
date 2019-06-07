{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module BFUtils where

import Language.Brainfuck.Compiler.Options

import LLVM
import LLVM.Target
import LLVM.Context
import LLVM.Module
import LLVM.IRBuilder
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt

import Options.Applicative

import Data.ByteString.Short


withRelocatableCode :: (TargetMachine -> IO a) -> IO a
withRelocatableCode f = do
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  cpuFeatures <- getHostCPUFeatures
  (target,_) <- lookupTarget Nothing triple
  withTargetOptions $ \opts ->
    withTargetMachine target triple cpu cpuFeatures opts
      Reloc.PIC CodeModel.Default CodeGenOpt.None f


compilerOpts :: Parser CompilerOpts
compilerOpts = do
  optLevel <- option optl
    (short 'O'
    <> help "Optimization level (0-3)"
    <> value defaultOptLevel)
  codeGenOpts <- do
    datasize <- option auto
      ( long "datasize"
        <> short 's'
        <> value defaultDataArraySize
        <> help "Array size in number of cells")
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
    pure (CGO (FiniteSizeArray datasize pos) cellsize)
  outputFormat <- option outf
    ( long "format"
    <> short 'f'
    <> value defaultOutputFormat
    <> help "Output format (ll|bc|s|o)")
  outputDestination <- (Just <$> strOption
    ( short 'o'
      <> metavar "FILE"
      <> help "Output file"
    )) <|> pure Nothing
  inputSource <- strArgument
    (metavar "SOURCE"
    <> help "input file")
  return CO{..}
  where
    optl = eitherReader \case
      "0" -> Right None
      "1" -> Right Simple
      "2" -> Right Medium
      "3" -> Right Aggressive
      _   -> Left "Invalid optimization level. Valid levels: (0-3)"
    cell = eitherReader \case
      "8"  -> Right I8
      "32" -> Right I32
      "64" -> Right I64
      "0"  -> Right Unbounded
      ('u':_) -> Right Unbounded
      _    -> Left "Invalid cell size. Valid sizes: (8|32|64|0|unbounded)"
    outf = eitherReader \case
      "ll" -> Right IRAssembly
      "bc" -> Right IRBitCode
      "s"  -> Right NativeAssembly
      "o"  -> Right Object
      _    -> Left "Invalid output format. Valid formats: (ll|bc|s|o)"

opts = info (compilerOpts <**> helper)
  ( fullDesc
  <> progDesc "Compile a brainfuck program"
  <> header "bfc - An optimizing brainfuck compiler")
