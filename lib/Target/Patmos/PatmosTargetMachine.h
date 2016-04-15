//==-- PatmosTargetMachine.h - Define TargetMachine for Patmos ---*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the Patmos specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_TARGETMACHINE_H_
#define _LLVM_TARGET_PATMOS_TARGETMACHINE_H_

#include "PatmosSubtarget.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
class Triple;

/// PatmosTargetMachine
///
class PatmosTargetMachine : public LLVMTargetMachine {
  std::unique_ptr<TargetLoweringObjectFile> TLOF;
  mutable StringMap<std::unique_ptr<PatmosSubtarget>> SubtargetMap;

public:
  PatmosTargetMachine(const Target &T, Triple TT,
                      StringRef CPU, StringRef FS,
                      TargetOptions O,
                      Reloc::Model RM, CodeModel::Model CM,
                      CodeGenOpt::Level L);

  const PatmosSubtarget *getSubtargetImpl(const Function &F) const override;

  /// createPassConfig - Create a pass configuration object to be used by
  /// addPassToEmitX methods for generating a pipeline of CodeGen passes.
  TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

  PatmosTargetObjectFile *getObjFileLowering() const override {
    return static_cast<PatmosTargetObjectFile*>(TLOF.get());
  }
}; // PatmosTargetMachine.

} // end namespace llvm

#endif // _LLVM_TARGET_PATMOS_TARGETMACHINE_H_
