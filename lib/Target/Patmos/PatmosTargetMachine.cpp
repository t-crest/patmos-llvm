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
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/StringExtras.h"

using namespace llvm;

extern "C" void LLVMInitializePatmosTarget() {
  // Register the target.
  RegisterTargetMachine<PatmosTargetMachine> X(ThePatmosTarget);
}

// TODO move options for stack cache size, data cache, ... here too (or move options to subtarget? meh..)

static const unsigned DEFAULT_METHOD_CACHE_BLOCK_SIZE = 128;
static const unsigned DEFAULT_INSTR_CACHE_BLOCK_SIZE = 8;

/// MethodCacheBlockSize - Block size of the method cache in bytes.
static cl::opt<unsigned> InstrCacheBlockSize("mpatmos-icache-block-size",
                           cl::desc("Size of the instruction cache blocks in bytes "
                                    "(defaults to 128 for method cache, 8 otherwise)."));

/// MethodCacheBlockSize - Total size of the method cache in bytes.
static cl::opt<unsigned> InstrCacheSize("mpatmos-icache-size",
                           cl::init(4096),
                           cl::desc("Total size of the instruction cache in bytes."));

/// MethodCacheBlockSize - Total size of the method cache in bytes.
static cl::opt<unsigned> FBlockAlign("mpatmos-fblock-align",
                           cl::init(128),
                           cl::desc("Alignment of function blocks in bytes (defaults to -mpatmos-icache-block-size)"));



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

    /// addPreEmitPass - This pass may be implemented by targets that want to run
    /// passes immediately before machine code is emitted.  This should return
    /// true if -print-machineinstrs should print out the code after the passes.
    virtual bool addPreEmitPass(){
      PM->add(createPatmosDelaySlotFillerPass(getPatmosTargetMachine()));
      return true;
    }
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
    DataLayout("E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f64:32:64-n32"),
    InstrInfo(*this), TLInfo(*this), TSInfo(*this),
    FrameLowering(Subtarget),
    InstrItins(Subtarget.getInstrItineraryData()) {
}


TargetPassConfig *PatmosTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new PatmosPassConfig(this, PM);
}

unsigned PatmosTargetMachine::getInstrCacheBlockSize() {
  unsigned Value;
  if (InstrCacheBlockSize.getNumOccurrences() > 0) {
    // Set by command line option
    Value = InstrCacheBlockSize;
  } else {
    // get default value depending on cache type
    Value = Subtarget.hasMethodCache() ? DEFAULT_METHOD_CACHE_BLOCK_SIZE :
                                         DEFAULT_INSTR_CACHE_BLOCK_SIZE;
  }
  return Value;
}

unsigned PatmosTargetMachine::getInstrCacheSize() {
  return InstrCacheSize;
}

unsigned PatmosTargetMachine::getFunctionBlockAlign() {
  if (FBlockAlign.getNumOccurrences() > 0) {
    // Set by command line option
    return FBlockAlign;
  }
  // TODO maybe use smaller default alignment than block size for method cache?
  return getInstrCacheBlockSize();
}


