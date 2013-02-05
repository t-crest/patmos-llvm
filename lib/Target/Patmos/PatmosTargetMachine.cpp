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
#include "PatmosSinglePathInfo.h"
#include "llvm/PassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/UnifyFunctionExitNodes.h"


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

  static cl::opt<std::string> SerializeMachineCode(
    "mpatmos-serialize",
    cl::desc("Export PML specification of generated machine code to FILE"),
    cl::init(""));

  static cl::list<std::string> SerializeRoots(
    "mpatmos-serialize-roots",
    cl::desc("Export only methods reachable from given functions"),
    cl::Hidden);

  /// Patmos Code Generator Pass Configuration Options.
  class PatmosPassConfig : public TargetPassConfig {

  private:
    PatmosSinglePathInfo PSPI;

  public:
    PatmosPassConfig(PatmosTargetMachine *TM, PassManagerBase &PM)
      : TargetPassConfig(TM, PM), PSPI( PatmosSinglePathInfo(*TM) ) {}

    PatmosTargetMachine &getPatmosTargetMachine() const {
      return getTM<PatmosTargetMachine>();
    }

    const PatmosSubtarget &getPatmosSubtarget() const {
      return *getPatmosTargetMachine().getSubtargetImpl();
    }

    /// addModulePass - Add a machine-level module pass to the pass manager and
    /// ensure that the MachineFunctionAnalysis is preserved/rebuilt.
    void addModulePass(ModulePass *MP)
    {
      // ensure that MachineFunctionAnalysis is preserved across the module pass
      PM->add(createPatmosPreserveFunctionPass());

      // add the module pass
      PM->add(MP);

      // rebuild the MachineFunctionAnalysis
      PM->add(new MachineFunctionAnalysis(getPatmosTargetMachine()));
    }

    virtual bool addInstSelector() {
      PM->add(createPatmosISelDag(getPatmosTargetMachine()));
      return false;
    }

    /// addPreISelPasses - This method should add any "last minute" LLVM->LLVM
    /// passes (which are run just before instruction selector).
    virtual bool addPreISel() {
      if (PSPI.enabled()) {
        // Single-path transformation requires a single exit node
        PM->add(createUnifyFunctionExitNodesPass());
        // Single-path transformation currently cannot deal with
        // switch/jumptables -> lower them to ITEs
        PM->add(createLowerSwitchPass());
        return true;
      }
      return false;
    }

    /// addPreEmitPass - This pass may be implemented by targets that want to run
    /// passes immediately before machine code is emitted.  This should return
    /// true if -print-machineinstrs should print out the code after the passes.
    virtual bool addPreEmitPass(){
      PM->add(createPatmosDelaySlotFillerPass(getPatmosTargetMachine()));
      PM->add(createPatmosFunctionSplitterPass(getPatmosTargetMachine()));

      if (EnableStackCacheAnalysis) {
        addModulePass(createPatmosStackCacheAnalysis(getPatmosTargetMachine()));
      }

      if (!SerializeMachineCode.empty()) {
        if (SerializeRoots.empty()) {
          PM->add(createPatmosExportPass(getPatmosTargetMachine(),
                                         SerializeMachineCode));
        } else {
          addModulePass(createPatmosModuleExportPass(getPatmosTargetMachine(),
                                         SerializeMachineCode, SerializeRoots));
        }
      }

      return true;
    }

    /// addPreRegAlloc - This method may be implemented by targets that want to
    /// run passes immediately before register allocation. This should return
    /// true if -print-machineinstrs should print after these passes.
    virtual bool addPreRegAlloc() {
      return false;
    }

    /// addPostRegAlloc - This method may be implemented by targets that want to
    /// run passes after register allocation pass pipeline but before
    /// prolog-epilog insertion.  This should return true if -print-machineinstrs
    /// should print after these passes.
    virtual bool addPostRegAlloc() {
      return false;
    }

    /// addPreSched2 - This method may be implemented by targets that want to
    /// run passes after prolog-epilog insertion and before the second instruction
    /// scheduling pass.  This should return true if -print-machineinstrs should
    /// print after these passes.
    virtual bool addPreSched2() {
      if (PSPI.enabled()) {
        PM->add(createPatmosSPPredicatePass(getPatmosTargetMachine(), PSPI));
        PM->add(createPatmosSPReducePass(getPatmosTargetMachine(), PSPI));
      } else {
        if (getOptLevel() != CodeGenOpt::None) {
          addPass(IfConverterID);
        }
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
    DataLayout("E-S32-p:32:32:32-i8:8:8-i16:16:16-i32:32:32-i64:32:32-f64:32:32-a0:0:32-s0:32:32-v64:32:32-v128:32:32-n32"),

    InstrInfo(*this), TLInfo(*this), TSInfo(*this),
    FrameLowering(*this),
    InstrItins(Subtarget.getInstrItineraryData()) {
}

TargetPassConfig *PatmosTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new PatmosPassConfig(this, PM);
}

