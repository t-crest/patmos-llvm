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
#include "SinglePath/PatmosSinglePathInfo.h"
#include "PatmosSchedStrategy.h"
#include "PatmosStackCacheAnalysis.h"
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
  // TODO instead of the generic ScheduleDAGMI, we might want to use a different
  // scheduler that allows for VLIW bundling or something, similar to the
  // PatmosPostRAScheduler. Should still be split into a generic ScheduleDAG
  // scheduler and a specialised PatmosSchedStrategy.
  ScheduleDAGMI *PS = new ScheduleDAGMI(C, new PatmosVLIWSchedStrategy());
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
      }
      if (TM->getSubtargetImpl()->usePatmosPostRAScheduler(getOptLevel())) {
        initializePatmosPostRASchedulerPass(*PassRegistry::getPassRegistry());
        substitutePass(&PostRASchedulerID, &PatmosPostRASchedulerID);
      }
    }

    PatmosTargetMachine &getPatmosTargetMachine() const {
      return getTM<PatmosTargetMachine>();
    }

    const PatmosSubtarget &getPatmosSubtarget() const {
      return *getPatmosTargetMachine().getSubtargetImpl();
    }

    virtual ScheduleDAGInstrs *
    createMachineScheduler(MachineSchedContext *C) const {
      return createPatmosVLIWMachineSched(C);
    }

    virtual bool addInstSelector() {
      addPass(createPatmosISelDag(getPatmosTargetMachine()));
      return false;
    }

    //
    /// addPreISelPasses - This method should add any "last minute" LLVM->LLVM
    /// passes (which are run just before instruction selector).
    virtual bool addPreISel() {
      if (PatmosSinglePathInfo::isEnabled()) {
        // Single-path transformation requires a single exit node
        addPass(createUnifyFunctionExitNodesPass());
        // Single-path transformation currently cannot deal with
        // switch/jumptables -> lower them to ITEs
        addPass(createLowerSwitchPass());
        addPass(createPatmosSPClonePass());
        return true;
      }
      return false;
    }

    /// addPreRegAlloc - This method may be implemented by targets that want to
    /// run passes immediately before register allocation. This should return
    /// true if -print-machineinstrs should print after these passes.
    virtual bool addPreRegAlloc() {
      // For -O0, add a pass that removes dead instructions to avoid issues
      // with spill code in naked functions containing function calls with
      // unused return values.
      if (getOptLevel() == CodeGenOpt::None) {
        addPass(&DeadMachineInstructionElimID);
      }
      return true;
    }

    /// addPostRegAlloc - This method may be implemented by targets that want to
    /// run passes after register allocation pass pipeline but before
    /// prolog-epilog insertion.  This should return true if -print-machineinstrs
    /// should print after these passes.
    virtual bool addPostRegAlloc() {
      if (PatmosSinglePathInfo::isEnabled()) {
        addPass(createPatmosSPMarkPass(getPatmosTargetMachine()));
        addPass(createPatmosSinglePathInfoPass(getPatmosTargetMachine()));
        addPass(createPatmosSPPreparePass(getPatmosTargetMachine()));
        addPass(createPatmosSinglePathInfoPass(getPatmosTargetMachine()));
        addPass(createPatmosSPBundlingPass(getPatmosTargetMachine()));
      }
      return false;
    }


    /// addPreSched2 - This method may be implemented by targets that want to
    /// run passes after prolog-epilog insertion and before the second instruction
    /// scheduling pass.  This should return true if -print-machineinstrs should
    /// print after these passes.
    virtual bool addPreSched2() {
      addPass(createPatmosPMLProfileImport(getPatmosTargetMachine()));

      if (PatmosSinglePathInfo::isEnabled()) {
        addPass(createPatmosSinglePathInfoPass(getPatmosTargetMachine()));
        addPass(createPatmosSPReducePass(getPatmosTargetMachine()));
      } else {
        if (getOptLevel() != CodeGenOpt::None && !DisableIfConverter) {
          addPass(&IfConverterID);
          // If-converter might create unreachable blocks (bug?), need to be
          // removed before function splitter
          addPass(&UnreachableMachineBlockElimID);
        }
        if (getOptLevel() != CodeGenOpt::None) {
          // Add the standard basic block placement before the post-RA scheduler
          // as it creates and removes branches.
          TargetPassConfig::addBlockPlacement();
        }
      }

      // this is pseudo pass that may hold results from SC analysis
      // (currently for PML export)
      addPass(createPatmosStackCacheAnalysisInfo(getPatmosTargetMachine()));

      if (EnableStackCacheAnalysis) {
        addPass(createPatmosStackCacheAnalysis(getPatmosTargetMachine()));
      }
      return true;
    }


    virtual void addBlockPlacement() {
      // The block placement passes are added after the post-RA scheduler.
      // We do want to have our branches created by this pass scheduled by the
      // post-RA scheduler and we do not handle delay-slots in
      // PatmosInstrInfo.InsertBranch|RemoveBranch, so we disable the default
      // pass here and add them in PreSched2 instead.
    }


    /// addPreEmitPass - This pass may be implemented by targets that want to run
    /// passes immediately before machine code is emitted.  This should return
    /// true if -print-machineinstrs should print out the code after the passes.
    virtual bool addPreEmitPass(){

      // Post-RA MI Scheduler does bundling and delay slots itself. Otherwise,
      // add passes to handle them.
      if (!getPatmosSubtarget().usePatmosPostRAScheduler(getOptLevel())) {
        addPass(createPatmosDelaySlotFillerPass(getPatmosTargetMachine(),
                                            getOptLevel() == CodeGenOpt::None));
      }

      // All passes below this line must handle delay slots and bundles
      // correctly.

      if (getPatmosSubtarget().hasMethodCache()) {
        addPass(createPatmosFunctionSplitterPass(getPatmosTargetMachine()));
      }

      addPass(createPatmosDelaySlotKillerPass(getPatmosTargetMachine()));

      addPass(createPatmosEnsureAlignmentPass(getPatmosTargetMachine()));

      // following pass is a peephole pass that does neither modify
      // the control structure nor the size of basic blocks.
      addPass(createPatmosBypassFromPMLPass(getPatmosTargetMachine()));

      return true;
    }

    /// addSerializePass - Install a pass that serializes the internal representation
    /// of the compiler to PML format
    virtual bool addSerializePass(std::string& OutFile,
                                  ArrayRef<std::string> Roots,
                                  std::string &BitcodeFile,
				  bool SerializeAll) {
      if (OutFile.empty())
        return false;


      addPass(createPatmosModuleExportPass(
          getPatmosTargetMachine(),
          OutFile, BitcodeFile,
          Roots.empty() ? ArrayRef<std::string>(DefaultRoot) : Roots,
	  SerializeAll
          ));

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
    InstrItins(Subtarget.getInstrItineraryData())
{
  initAsmInfo();
}

TargetPassConfig *PatmosTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new PatmosPassConfig(this, PM);
}
