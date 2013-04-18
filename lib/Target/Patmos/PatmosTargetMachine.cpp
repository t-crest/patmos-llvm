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
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/StringExtras.h"

using namespace llvm;


extern "C" void LLVMInitializePatmosTarget() {
  // Register the target.
  RegisterTargetMachine<PatmosTargetMachine> X(ThePatmosTarget);
}

namespace {
  /// EnableStackCacheAnalysis - Option to enable the analysis of Patmos' stack
  /// cache usage.
  static cl::opt<bool> EnableStackCacheAnalysis(
    "mpatmos-enable-stack-cache-analysis",
    cl::init(false),
    cl::desc("Enable the Patmos stack cache analysis."),
    cl::Hidden);
  static cl::opt<bool> DisableIfConverter(
      "mpatmos-disable-ifcvt",
      cl::init(false),
      cl::desc("Disable if-converter for Patmos."),
      cl::Hidden);

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
      addPass(createPatmosISelDag(getPatmosTargetMachine()));
      return false;
    }

    /// addPreEmitPass - This pass may be implemented by targets that want to run
    /// passes immediately before machine code is emitted.  This should return
    /// true if -print-machineinstrs should print out the code after the passes.
    virtual bool addPreEmitPass(){

      if (EnableStackCacheAnalysis) {
        addPass(createPatmosStackCacheAnalysis(getPatmosTargetMachine()));
      }

      addPass(createPatmosDelaySlotFillerPass(getPatmosTargetMachine()));
      addPass(createPatmosFunctionSplitterPass(getPatmosTargetMachine()));

      return true;
    }

    /// addSerializePass - Install a pass that serializes the internal representation
    /// of the compiler to PML format
    virtual bool addSerializePass(std::string& OutFile,
                                  ArrayRef<std::string> Roots,
                                  std::string &BitcodeFile) {
      if (OutFile.empty())
        return false;
      if (Roots.empty()) {
        addPass(createPatmosExportPass(getPatmosTargetMachine(), OutFile, BitcodeFile));
      } else {
        addPass(createPatmosModuleExportPass(getPatmosTargetMachine(), OutFile, BitcodeFile, Roots));
      }
      return true;
    }

    /// addPreSched2 - This method may be implemented by targets that want to
    /// run passes after prolog-epilog insertion and before the second instruction
    /// scheduling pass.  This should return true if -print-machineinstrs should
    /// print after these passes.
    virtual bool addPreSched2() {
      if (getOptLevel() != CodeGenOpt::None && !DisableIfConverter) {
        addPass(&IfConverterID);
      }
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

    // Keep this in sync with clang/lib/Basic/Targets.cpp and
    // compiler-rt/lib/patmos/*.ll
    // Note: Both ABI and Preferred Alignment must be 32bit for all supported
    // types, backend does not support different stack alignment.
    DL("E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:32-f64:32:32-a0:0:32-s0:32:32-v64:32:32-v128:32:32-n32"),

    InstrInfo(*this), TLInfo(*this), TSInfo(*this),
    FrameLowering(*this),
    InstrItins(Subtarget.getInstrItineraryData()) {
}

TargetPassConfig *PatmosTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new PatmosPassConfig(this, PM);
}

