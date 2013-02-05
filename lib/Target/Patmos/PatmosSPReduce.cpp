//===-- PatmosSPReduce.cpp - Reduce the CFG for Single-Path code ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass reduces functions marked for single-path conversion.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
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

  class PatmosSPReduce : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;

    PatmosSinglePathInfo &PSPI;

  protected:
    /// Reduce a given MachineFunction
    void doReduceFunction(MachineFunction &MF);

    /// Put the MBBs in a topological order
    void linearizeMBBs(MachineFunction &MF,
                       std::vector<MachineBasicBlock*> &order);

    /// Merge the linear sequence of MBBs to a single MBB
    void mergeMBBs(MachineFunction &MF,
                   std::vector<MachineBasicBlock*> &order);
  public:
    /// PatmosSPReduce - Initialize with PatmosTargetMachine
    PatmosSPReduce(const PatmosTargetMachine &tm, PatmosSinglePathInfo &pspi) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        PSPI(pspi) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Single-Path Reducer";
    }

    /// getAnalysisUsage - Specify which passes this pass depends on
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      MachineFunctionPass::getAnalysisUsage(AU);
    }


    /// runOnMachineFunction - Run the SP converter on the given function.
    virtual bool runOnMachineFunction(MachineFunction &MF) {
      bool changed = false;
      // only convert function if specified on command line
      if ( PSPI.isToConvert(MF) ) {
        DEBUG( dbgs() << "[Single-Path] Reducing "
                      << MF.getFunction()->getName() << "\n" );
        doReduceFunction(MF);
        changed |= true;
      }
      return changed;
    }
  };

  char PatmosSPReduce::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosSPReducePass - Returns a new PatmosSPReduce
/// \see PatmosSPReduce
FunctionPass *llvm::createPatmosSPReducePass(const PatmosTargetMachine &tm,
                                             PatmosSinglePathInfo &pspi) {
  return new PatmosSPReduce(tm, pspi);
}

///////////////////////////////////////////////////////////////////////////////



void PatmosSPReduce::doReduceFunction(MachineFunction &MF) {

  std::vector<MachineBasicBlock*> order;

  // first linearize, updating successors accordingly
  linearizeMBBs(MF, order);
  // then merge to a single basic block
  mergeMBBs(MF, order);
}

void PatmosSPReduce::linearizeMBBs(MachineFunction &MF,
                                   std::vector<MachineBasicBlock*> &order) {

  // Topological order of MBBs - following only works for DAGs (without loops)
  // Replace the edges of the CFG by a linear toposorted sequence
  // FIXME order in hyperblocks
  ReversePostOrderTraversal<MachineFunction*> RPOT(&MF);
  order.insert(order.begin(), RPOT.begin(), RPOT.end());

  std::vector<MachineBasicBlock*>::iterator I = order.begin(),
                                            E = order.end();
  MachineBasicBlock *MBB = *I;
  while ( ++I != E) {
    MachineBasicBlock *NextMBB = *I;

    // first, remove all successors ...
    for ( MachineBasicBlock::succ_iterator SI = MBB->succ_begin();
          SI != MBB->succ_end();
          SI = MBB->removeSuccessor(SI) );
    // ... then add NextBB as single successor
    MBB->addSuccessor(NextMBB);

    // Correct the layout and remove the branch instructions
    NextMBB->moveAfter(MBB);
    TII->RemoveBranch(*MBB);

    DEBUG_TRACE( MBB->dump() );

    // move forward
    MBB = NextMBB;
  }
  DEBUG_TRACE( MBB->dump() );
}


void PatmosSPReduce::mergeMBBs(MachineFunction &MF,
                               std::vector<MachineBasicBlock*> &order) {
  std::vector<MachineBasicBlock*>::iterator I = order.begin(),
                                            E = order.end();

  MachineBasicBlock *BaseMBB = *I;
  // iterate through order of MBBs
  while (++I != E) {
    // get MBB of iterator
    MachineBasicBlock *MBB = *I;
    // we are supposed to be on a linear sequence!
    assert(BaseMBB->succ_size() == 1);
    DEBUG_TRACE( dbgs() << "  Merge MBB#" << MBB->getNumber() << "\n" );
    // transfer the instructions
    BaseMBB->splice(BaseMBB->end(), MBB, MBB->begin(), MBB->end());
    // remove the edge between BaseMBB and MBB
    BaseMBB->removeSuccessor(BaseMBB->succ_begin());
    // BaseMBB gets the successors of MBB instead
    assert(BaseMBB->succ_empty());
    BaseMBB->transferSuccessors(MBB);
    // remove MBB from MachineFunction
    MF.erase(MBB);
  }
  // invalidate order
  order.clear();
}

