//===-- PatmosTargetMachine.cpp - Define TargetMachine for Patmos ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Top-level implementation for the Patmos target.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosTargetMachine.h"
#include "llvm/PassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

extern "C" void LLVMInitializePatmosTarget() {
  // Register the target.
  RegisterTargetMachine<PatmosTargetMachine> X(ThePatmosTarget);
}

namespace {
  /// Patmos Code Generator Pass Configuration Options.
  class PatmosPassConfig : public TargetPassConfig {
  public:
    PatmosPassConfig(PatmosTargetMachine *TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM) {}

    PatmosTargetMachine &getPatmosTargetMachine() const {
      return getTM<PatmosTargetMachine>();
    }

    const PatmosSubtarget &getPatmosSubtarget() const {
      return *getPatmosTargetMachine().getSubtargetImpl();
    }

    virtual bool addInstSelector() {
      PM->add(createPatmosISelDag(getPatmosTargetMachine()));
      return false;
    }

    virtual bool addPreEmitPass();
  };
} // namespace

PatmosTargetMachine::PatmosTargetMachine(const Target &T,
                                         StringRef TT,
                                         StringRef CPU,
                                         StringRef FS,
                                         TargetOptions O, 
                                         Reloc::Model RM, CodeModel::Model CM,
                                         CodeGenOpt::Level L)
  : LLVMTargetMachine(T, TT, CPU, FS, O, RM, CM, L),
    Subtarget(TT, CPU, FS),
    // FIXME: Check TargetData string.
    DataLayout("e-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:64-n32"),
    InstrInfo(*this), TLInfo(*this), TSInfo(*this),
    FrameLowering(Subtarget),
    ELFWriterInfo(*this),
    InstrItins(Subtarget.getInstrItineraryData()) {
}


TargetPassConfig *PatmosTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new PatmosPassConfig(this, PM);
}

/// addPreEmitPass - This pass may be implemented by targets that want to run
/// passes immediately before machine code is emitted.  This should return
/// true if -print-machineinstrs should print out the code after the passes.
bool PatmosPassConfig::addPreEmitPass(){
  PM->add(createPatmosDelaySlotFillerPass(getPatmosTargetMachine()));
  return true;
}

