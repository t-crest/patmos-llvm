//===-- PatmosBypassFromPML.cpp - Patmos bypass from PML ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass imports a PML file with annotated aiT analysis results,
// scans them for memory accesses with unknown (large) address ranges
// and rewrites the memory instructions to bypassed loads.
// The rationale is to avoid destroying the state of the data cache by
// unanalyzable accesses, such that the analysis becomes more precise.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-bypass-from-pml"
#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/PMLImport.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"



using namespace llvm;

STATISTIC( RewrittenLoads, "Number of load instructions rewritten to bypass");


static cl::opt<bool> EnableBypassFromPML(
  "mpatmos-enable-bypass-from-pml",
  cl::init(false),
  cl::desc("Enable rewriting unanalyzable (aiT) memory accesses "
           "to bypass the cache."),
  cl::Hidden);

namespace {


  class PatmosBypassFromPML : public MachineFunctionPass {
  private:
    // Pass ID
    static char ID;

    // bypassCacheLoads - Rewrite the loads in a given MBB for which
    // there exist value facts classifiying the load as load
    // from an unpredictable address
    bool bypassCacheLoads(MachineBasicBlock &MBB, PMLMCQuery *Query) {
      //TODO implement
      return false;
    }

  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    PatmosTargetMachine &TM;
    const PatmosInstrInfo *TII;
    const TargetRegisterInfo *TRI;

    PatmosBypassFromPML(PatmosTargetMachine &tm)
      : MachineFunctionPass(ID), TM(tm),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        TRI(tm.getRegisterInfo()) {
      // we have to initialize the PMLImport Pass
      initializePMLImportPass(*PassRegistry::getPassRegistry());
    }

    virtual const char *getPassName() const {
      return "Patmos Bypass From PML";
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<PMLImport>();
      AU.setPreservesAll();
      MachineFunctionPass::getAnalysisUsage(AU);
    }


    bool runOnMachineFunction(MachineFunction &MF) {

      if (!EnableBypassFromPML) return false;

      PMLImport &PI = getAnalysis<PMLImport>();

      PMLMCQuery *Query = PI.createMCQuery(*this, MF);

      bool Changed = false;
      DEBUG( dbgs() << "\n[BypassFromPML] "
                    << MF.getFunction()->getName() << "\n" );

      for (MachineFunction::iterator FI = MF.begin(), FE = MF.end();
           FI != FE; ++FI)
        Changed |= bypassCacheLoads(*FI, Query);

      if (Query) delete Query;

      return Changed;
    }

  };


  char PatmosBypassFromPML::ID = 0;
} // end of anonymous namespace

/// createPatmosBypassFromPMLPass - Returns a pass that fills in delay
/// slots in Patmos MachineFunctions
///
FunctionPass *llvm::createPatmosBypassFromPMLPass(PatmosTargetMachine &tm) {
  return new PatmosBypassFromPML(tm);
}


