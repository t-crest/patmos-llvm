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
#include "PatmosMachineScheduler.h"
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

static ScheduleDAGInstrs *createPatmosVLIWMachineSched(MachineSchedContext *C) {
  ScheduleDAGMI *PS = new PatmosVLIWScheduler(C, new PatmosVLIWSchedStrategy());
  return PS;
}

static MachineSchedRegistry
SchedCustomRegistry("patmos", "Run Patmos's custom scheduler",
                    createPatmosVLIWMachineSched);


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
  private:
    const std::string DefaultRoot;

  public:
    PatmosPassConfig(PatmosTargetMachine *TM, PassManagerBase &PM)
     : TargetPassConfig(TM, PM), DefaultRoot("main")
    {
      // Enable preRA MI scheduler.
      if (TM->getSubtargetImpl()->usePreRAMIScheduler(getOptLevel())) {
        enablePass(&MachineSchedulerID);
        MachineSchedRegistry::setDefault(createPatmosVLIWMachineSched);
      }
      if (TM->getSubtargetImpl()->usePostRAMIScheduler(getOptLevel())) {
        // TODO setup MI scheduler as post-RA scheduler
        // (substitute PostRASchdeulerID)

      }
    }

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

    /// addPreISelPasses - This method should add any "last minute" LLVM->LLVM
    /// passes (which are run just before instruction selector).
    virtual bool addPreISel() {
      if (PatmosSinglePathInfo::isEnabled()) {
        // Single-path transformation requires a single exit node
        addPass(createUnifyFunctionExitNodesPass());
        // Single-path transformation currently cannot deal with
        // switch/jumptables -> lower them to ITEs
        addPass(createLowerSwitchPass());
        return true;
      }
      return false;
    }


    /// addPreEmitPass - This pass may be implemented by targets that want to run
    /// passes immediately before machine code is emitted.  This should return
    /// true if -print-machineinstrs should print out the code after the passes.
    virtual bool addPreEmitPass(){

      // Post-RA MI Scheduler does bundling and delay slots itself. Otherwise,
      // add passes to handle them.
      if (!getPatmosSubtarget().usePostRAMIScheduler(getOptLevel())) {

        if (getPatmosSubtarget().enableBundling(getOptLevel())) {
          addPass(createPatmosPacketizer(getPatmosTargetMachine()));
        }

        // disable the filler if we have bundles, the filler does not handle
        // them properly at the moment.
        addPass(createPatmosDelaySlotFillerPass(getPatmosTargetMachine(),
                                                false));
      }

      if (getPatmosSubtarget().enableBundling(getOptLevel())) {
        addPass(createPatmosBundleSanitizer(getPatmosTargetMachine()));
      }

      // All passes below this line must handle delay slots and bundles
      // correctly.

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


      addPass(createPatmosModuleExportPass(
          getPatmosTargetMachine(),
          OutFile, BitcodeFile,
          Roots.empty() ? ArrayRef<std::string>(DefaultRoot) : Roots
          ));

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
      addPass(createPatmosSinglePathInfoPass(getPatmosTargetMachine()));
      addPass(createPatmosSPPreparePass(getPatmosTargetMachine()));
      return false;
    }


    /// addPreSched2 - This method may be implemented by targets that want to
    /// run passes after prolog-epilog insertion and before the second instruction
    /// scheduling pass.  This should return true if -print-machineinstrs should
    /// print after these passes.
    virtual bool addPreSched2() {
      if (PatmosSinglePathInfo::isEnabled()) {
        addPass(createPatmosSinglePathInfoPass(getPatmosTargetMachine()));
        addPass(createPatmosSPReducePass(getPatmosTargetMachine()));
      } else {
        if (getOptLevel() != CodeGenOpt::None && !DisableIfConverter) {
          addPass(&IfConverterID);
        }
      }

      if (EnableStackCacheAnalysis) {
        addPass(createPatmosStackCacheAnalysis(getPatmosTargetMachine()));
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

