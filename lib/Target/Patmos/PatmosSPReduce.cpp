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
#include "llvm/ADT/BitVector.h"
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

    /// Reduce a given MachineFunction
    void doReduceFunction(MachineFunction &MF);

#if 0
    /// insertPredDefinitions - Insert predicate register definitions at the
    /// edges computed in K. Also insert initializations for predicates in K
    /// for which the bit is set in needsInit.
    /// The vreg for use in a specific MBB is returned in pred_use_vregs.
    void insertPredDefinitions(MachineFunction &MF, K_t &K, R_t &R,
                               BitVector &needsInit, R_t &pred_use_vregs)
                               const;

    /// applyPredicates - predicate instructions of MBBs according
    /// to pred_use_vregs
    void applyPredicates(MachineFunction &MF, R_t &pred_use_vregs) const;
#endif

    /// Put the MBBs in a topological order
    void linearizeMBBs(MachineFunction &MF,
                       std::vector<MachineBasicBlock*> &order);

    /// Merge the linear sequence of MBBs to a single MBB
    void mergeMBBs(MachineFunction &MF,
                   std::vector<MachineBasicBlock*> &order);
  public:
    /// PatmosSPReduce - Initialize with PatmosTargetMachine
    PatmosSPReduce(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Single-Path Reducer";
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
FunctionPass *llvm::createPatmosSPReducePass(const PatmosTargetMachine &tm) {
  return new PatmosSPReduce(tm);
}

///////////////////////////////////////////////////////////////////////////////



void PatmosSPReduce::doReduceFunction(MachineFunction &MF) {

  PatmosSinglePathInfo &PSPI = getAnalysis<PatmosSinglePathInfo>();

  // let's see if we get the order right
  std::vector<const MachineBasicBlock *> order;
  PSPI.getRootNode()->getOrder(order);
  for(unsigned i=0; i<order.size(); i++) {
    dbgs() << " | " << order[i]->getNumber() << "\n";
  }

  // first linearize, updating successors accordingly
  //linearizeMBBs(MF, order);
  // then merge to a single basic block
  //mergeMBBs(MF, order);
}

#if 0

void PatmosSPReduce::insertPredDefinitions(MachineFunction &MF, K_t &K,
                                              R_t &R, BitVector &needsInit,
                                              R_t &pred_use_vregs) const {

  DEBUG( dbgs() << "Insert Predicate Definitions\n" );

  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  // For each predicate, insert defs in MBBs (before terminators)
  for (unsigned int i=0; i<K.size(); i++) {

    // check for definition edges
    if (K[i].size()==0) {
      DEBUG( dbgs() << "  Skip: no definition predicate for p" << i << "\n" );
      continue;
    }

    unsigned preg = RegInfo.createVirtualRegister(&Patmos::PRegsRegClass);

    // for each definition edge
    for (CD_map_entry_t::iterator EI=K[i].begin(); EI!=K[i].end(); ++EI) {
      MachineBasicBlock *MBBSrc = EI->first, *MBBDst = EI->second;
      // for AnalyzeBranch
      MachineBasicBlock *TBB = NULL, *FBB = NULL;
      SmallVector<MachineOperand, 4> Cond;
      if (!TII->AnalyzeBranch(*MBBSrc, TBB, FBB, Cond)) {
        // According to AnalyzeBranch spec, at a conditional branch,
        // Cond will hold the branch conditions
        // Further, there are two cases for conditional branches:
        // 1. conditional+fallthrough:   TBB holds branch target
        // 2. conditional+unconditional: TBB holds target of conditional branch,
        //                               FBB the target of the unconditional one
        // Hence, the branch condition will always refer to the TBB edge.
        assert( !Cond.empty() && "AnalyzeBranch for SP-IfConversion failed; "
                                 "could not determine branch condition");
        if (MBBDst != TBB)  TII->ReverseBranchCondition(Cond);


        // insert the predicate definition before any branch at the MBB end
        MachineBasicBlock::iterator firstTI = MBBSrc->getFirstTerminator();
        DebugLoc DL(firstTI->getDebugLoc());
        MachineInstr *NewMI = AddDefaultPred(BuildMI(*MBBSrc, firstTI, DL,
                                 TII->get(Patmos::PMOV), preg))
                                .addOperand(Cond[0]).addOperand(Cond[1]);
        //TODO Is this a proper way dealing with kills?
        // Note that there might be additional definitions inserted in MBB
        // for other k \in K
        RegInfo.clearKillFlags(Cond[0].getReg());
        DEBUG( dbgs() << "  Insert in BB#" << MBBSrc->getNumber()
                      << ": " << *NewMI );

      } else {
        assert(0 && "AnalyzeBranch failed");
      }
    } // end for each definition edge

    if (needsInit.test(i)) {
      // initialized with F?
      DebugLoc DL; //TODO
      MachineInstr *InitMI = AddDefaultPred(BuildMI(MF.front(), MF.front().begin(), DL,
          TII->get(Patmos::PCLR), preg));
      DEBUG( dbgs() << "  Insert initialization in BB#"
          << MF.front().getNumber() << ": " << *InitMI );
    }

    // obtain virtual register for each MBB using the i-th predicate,
    // preserving correct SSA form (with SSA Updater)
    for (MachineFunction::iterator FI=MF.begin(); FI!=MF.end(); ++FI) {
      MachineBasicBlock *MBB = FI;
      if (R[MBB] != i) continue;
      pred_use_vregs[MBB] = preg;
      // TODO beautify
    }

  } // end for each predicate
}



void PatmosSPReduce::applyPredicates(MachineFunction &MF,
                                        R_t &pred_use_vregs) const {
  DEBUG( dbgs() << "Applying predicates to MBBs\n" );

  // for each MBB
  for (MachineFunction::iterator FI=MF.begin(); FI!=MF.end(); ++FI) {
    MachineBasicBlock *MBB = FI;

    // check for use predicate
    if (!pred_use_vregs.count(MBB)) {
      DEBUG( dbgs() << "  skip: no definitions for BB#" << MBB->getNumber()
                    << "\n" );
      continue;
    }

    // lookup for current MBB
    unsigned preg = pred_use_vregs[MBB];
    DEBUG( dbgs() << "  applying " << PrintReg(preg) << " to BB#"
                  << MBB->getNumber() << "\n" );

    // apply predicate to all instructions in block
    for( MachineBasicBlock::iterator MI = MBB->begin(),
            ME(MBB->getFirstTerminator()); MI != ME; ++MI) {
      assert(!MI->isBundle() &&
             "PatmosInstrInfo::PredicateInstruction() can't handle bundles");

      // check for terminators - return? //TODO
      if (MI->isReturn()) {
          DEBUG( dbgs() << "    skip return: " << *MI );
          continue;
      }
      // TODO properly handle calls

      if (MI->isPredicable()) {
        if (!TII->isPredicated(MI)) {
          // find first predicate operand
          int i = MI->findFirstPredOperandIdx();
          assert(i != -1);
          MachineOperand &PO1 = MI->getOperand(i);
          MachineOperand &PO2 = MI->getOperand(i+1);
          assert(PO1.isReg() && PO2.isImm() &&
                 "Unexpected Patmos predicate operand");
          PO1.setReg(preg);
          PO2.setImm(0);
        } else {
          //TODO handle already predicated instructions
          DEBUG( dbgs() << "    in BB#" << MBB->getNumber()
                        << ": instruction already predicated: " << *MI );
        }
      }
    } // for each instruction in MBB
  } // end for each MBB
}

#endif



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

