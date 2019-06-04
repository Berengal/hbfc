module BFUtils where

import LLVM
import LLVM.Target
import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt


import Data.ByteString.Short


withRelocatableCode :: (TargetMachine -> IO a) -> IO a
withRelocatableCode f = do
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  cpuFeatures <- getHostCPUFeatures
  (target,_) <- lookupTarget Nothing triple
  withTargetOptions $ \opts ->
    withTargetMachine target triple cpu cpuFeatures opts Reloc.PIC CodeModel.Default CodeGenOpt.None f
