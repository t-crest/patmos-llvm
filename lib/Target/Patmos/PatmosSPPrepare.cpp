//===-- PatmosSPPrepare.cpp - Prepare for Single-Path conversion ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass prepares functions marked for single-path conversion.
// It creates predicate spill slots and loop counter slots where necessary.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
//#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include <map>
#include <sstream>
#include <iostream>


using namespace llvm;


// anonymous namespace
namespace {

  class PatmosSPPrepare : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;

    /// doPrepareFunction - Reduce a given MachineFunction
    void doPrepareFunction(MachineFunction &MF);

  public:
    /// PatmosSPPrepare - Initialize with PatmosTargetMachine
    PatmosSPPrepare(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Single-Path Prepare";
    }

    /// getAnalysisUsage - Specify which passes this pass depends on
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<PatmosSinglePathInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }


    /// runOnMachineFunction - Run the SP converter on the given function.
    virtual bool runOnMachineFunction(MachineFunction &MF) {
      PatmosSinglePathInfo &PSPI = getAnalysis<PatmosSinglePathInfo>();
      bool changed = false;
      // only convert function if specified on command line
      if ( PSPI.isEnabled(MF) ) {
        DEBUG( dbgs() << "[Single-Path] Preparing "
                      << MF.getFunction()->getName() << "\n" );
        doPrepareFunction(MF);
        changed |= true;
      }
      return changed;
    }
  };

  char PatmosSPPrepare::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosSPPreparePass - Returns a new PatmosSPPrepare
/// \see PatmosSPPrepare
FunctionPass *llvm::createPatmosSPPreparePass(const PatmosTargetMachine &tm) {
  return new PatmosSPPrepare(tm);
}

///////////////////////////////////////////////////////////////////////////////



void PatmosSPPrepare::doPrepareFunction(MachineFunction &MF) {

  PatmosSinglePathInfo &PSPI = getAnalysis<PatmosSinglePathInfo>();

  MachineFrameInfo &MFI = *MF.getFrameInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  SPNode *root = PSPI.getRootNode();

  std::vector<unsigned> requiredPreds;

  // for all (sub-)SPNodes
  for (df_iterator<SPNode*> I = df_begin(root), E = df_end(root); I!=E; ++I) {
    SPNode *N = *I;
    MachineBasicBlock *Header = N->getHeader();
    unsigned preds = PSPI.getNumPredicates(N);
    unsigned d = N->getDepth();

    DEBUG( dbgs() << "[MBB#" << Header->getNumber()
                  << "]: d=" << d << ", " << preds << "\n");

    // keep track of the maximum required number of predicates for each SPNode
    if (d+1 > requiredPreds.size()) {
      requiredPreds.push_back(preds);
    } else {
      if (requiredPreds[d] < preds)
        requiredPreds[d] = preds;
    }
  }

  for(unsigned i=0; i<requiredPreds.size(); i++) {
    int fi;
    DEBUG( dbgs() << "[" << i << "]: " << requiredPreds[i] << "\n");
    // create a stack slot for S0 (7 assignable predicates) + each additional
    // started 32 bit predicates:

    fi = MFI.CreateStackObject(8, 1, false);
    PMFI.SinglePathSpillFIs.push_back(fi);

    const TargetRegisterClass *RC = &Patmos::RRegsRegClass;
    for (unsigned j=0; j<(requiredPreds[i]+25)/32; j++) {
      fi = MFI.CreateStackObject(RC->getSize(), RC->getAlignment(), false);
      PMFI.SinglePathSpillFIs.push_back(fi);
    }

  }

}


///////////////////////////////////////////////////////////////////////////////

