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
#include "llvm/IR/Function.h"
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

    unsigned getNumUnusedPRegs(MachineFunction &MF) const;

  public:
    /// PatmosSPPrepare - Initialize with PatmosTargetMachine
    PatmosSPPrepare(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())) {
      (void) TM;
    }

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
      // only convert function if marked
      if ( PSPI.isConverting(MF) ) {
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

  PatmosSinglePathInfo *PSPI = &getAnalysis<PatmosSinglePathInfo>();

  MachineFrameInfo &MFI = *MF.getFrameInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  const TargetRegisterClass *RC = &Patmos::RRegsRegClass;

  std::vector<unsigned> requiredPreds;

  // for all (sub-)SPScopes
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
      I!=E; ++I) {
    SPScope *S = *I;
    unsigned preds = S->getNumPredicates();
    unsigned d = S->getDepth();

    DEBUG( dbgs() << "[MBB#" << S->getHeader()->getMBB()->getNumber()
                  << "]: d=" << d << ", " << preds << "\n");

    // keep track of the maximum required number of predicates for each SPScope
    if (d+1 > requiredPreds.size()) {
      requiredPreds.push_back(preds);
    } else {
      if (requiredPreds[d] < preds)
        requiredPreds[d] = preds;
    }
  }

  // create a loop counter slot for each nesting level (no slot required for 0)
  for (unsigned i = 0; i < requiredPreds.size() - 1; i++) {
    int fi = MFI.CreateStackObject(RC->getSize(), RC->getAlignment(), false);
    PMFI.addSinglePathFI(fi);
  }

  // store the start index of the S0 spill slots
  PMFI.startSinglePathS0Spill();

  // create for each nesting level but the innermost one a byte-sized
  // spill slot for S0 in use
  for(unsigned i=0; i<requiredPreds.size()-1; i++) {
    int fi = MFI.CreateStackObject(1, 1, false);
    PMFI.addSinglePathFI(fi);
  }

  // store the start index of the excess spill slots
  PMFI.startSinglePathExcessSpill();


  // compute the required number of spill bits, depending on the number
  // of allocatable pred regs
  int numAllocatablePRegs = getNumUnusedPRegs(MF);
  int numSpillSlotsReq = 0;
  for(unsigned i=0; i<requiredPreds.size(); i++) {
    DEBUG( dbgs() << "[" << i << "]: " << requiredPreds[i] << "\n");

    int cnt = requiredPreds[i] - numAllocatablePRegs;
    if (cnt>0) {
      // for exchanging locations, we might need an additional
      // temporary location
      numSpillSlotsReq += (cnt+1);
    }
  }

  DEBUG( dbgs() << "Computed number of allocatable PRegs: "
                << numAllocatablePRegs
                << "\nRequired predicate spill slots (bits): "
                << numSpillSlotsReq << "\n");

  // create them as multiples of RRegs size
  for (unsigned j=0;
       j <= (numSpillSlotsReq+31)/(8*RC->getSize());
       j++) {
    int fi = MFI.CreateStackObject(RC->getSize(), RC->getAlignment(), false);
    PMFI.addSinglePathFI(fi);
  }

  // if another (_sp_-)function is called, reserve space for re-/storing R9
  if (MFI.hasCalls()) {
    PMFI.startSinglePathCallSpill();
    int fi = MFI.CreateStackObject(RC->getSize(), RC->getAlignment(), false);
    PMFI.addSinglePathFI(fi);
  }
}


unsigned PatmosSPPrepare::getNumUnusedPRegs(MachineFunction &MF) const {
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  unsigned count = 0;
  // Get the unused predicate registers
  for (TargetRegisterClass::iterator I=Patmos::PRegsRegClass.begin(),
      E=Patmos::PRegsRegClass.end(); I!=E; ++I ) {
    if (RegInfo.reg_empty(*I) && *I!=Patmos::P0) {
      count++;
    }
  }
  return count;
}
///////////////////////////////////////////////////////////////////////////////

