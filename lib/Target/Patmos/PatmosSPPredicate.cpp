//===-- PatmosSPPredicate.cpp - Predicate insts for single-path code ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass predicates instructions before register allocation.
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

  /// Pass to perform if-conversion for single-path code generation
  class PatmosSPPredicate : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;

    PatmosSinglePathInfo &PSPI;

  protected:
    /// Perform the conversion on a given MachineFunction
    void doConvertFunction(MachineFunction &MF);

  public:
    /// PatmosSPPredicate - Initialize with PatmosTargetMachine
    PatmosSPPredicate(const PatmosTargetMachine &tm, PatmosSinglePathInfo &pspi) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        PSPI(pspi) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Single-Path Converter";
    }

    /// getAnalysisUsage - Specify which passes this pass depends on
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<MachinePostDominatorTree>();
      AU.addRequired<MachineLoopInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }


    /// runOnMachineFunction - Run the SP converter on the given function.
    virtual bool runOnMachineFunction(MachineFunction &MF) {
      bool changed = false;
      // only convert function if specified on command line
      if ( PSPI.isToConvert(MF) ) {
        DEBUG( dbgs() << "Single-Path converting " << MF.getFunction()->getName() << "\n");
        doConvertFunction(MF);
        changed |= true;
      }
      return changed;
    }
  };

  char PatmosSPPredicate::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosSPPredicatePass - Returns a new PatmosSPPredicate
/// \see PatmosSPPredicate
FunctionPass *llvm::createPatmosSPPredicatePass(const PatmosTargetMachine &tm,
                                                PatmosSinglePathInfo &pspi) {
  return new PatmosSPPredicate(tm, pspi);
}


///////////////////////////////////////////////////////////////////////////////

void PatmosSPPredicate::doConvertFunction(MachineFunction &MF) {

  const Module *M = MF.getFunction()->getParent();

  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  MachinePostDominatorTree &PDT = getAnalysis<MachinePostDominatorTree>();
  MachineLoopInfo &LI = getAnalysis<MachineLoopInfo>();

  // Postdominator Tree
  assert( PDT.getRoots().size()==1 && "Function must have a single exit node!");
  PDT.print( dbgs(), M);


  // Get loop information
  int loopcnt = 0;
  for (MachineLoopInfo::iterator I=LI.begin(), E=LI.end(); I!=E; ++I) {
    MachineLoop *Loop = *I;
    Loop->dump();
    loopcnt++;
  }
  if (loopcnt > 0) {
    // TODO
    dbgs() << "Cannot process loops yet.\n";
    return;
  }


  typedef std::set<std::pair<MachineBasicBlock*,MachineBasicBlock*> > CD_map_entry_t;
  typedef std::map<MachineBasicBlock*, CD_map_entry_t>                CD_map_t;

  // CD: MBB -> set of edges
  CD_map_t CD;

  for (MachineFunction::iterator FI=MF.begin(); FI!=MF.end(); ++FI) {
    MachineBasicBlock *MBB = FI;
    MachineDomTreeNode *ipdom = PDT[MBB]->getIDom();

    for(MachineBasicBlock::succ_iterator SI=MBB->succ_begin(), SE=MBB->succ_end();
        SI!=SE; ++SI) {
      MachineBasicBlock *SMBB = *SI;
      if (!PDT.dominates(SMBB, MBB)) {
        dbgs() << " Consider " << MBB->getNumber() << " -> " << SMBB->getNumber() << "\n";
        MachineDomTreeNode *t = PDT[SMBB];
        while( t != ipdom) {
          dbgs() << "   Insert into CD(" << t->getBlock()->getNumber() << "): (" << MBB->getNumber() << "," << SMBB->getNumber() << ")\n";
          CD[t->getBlock()].insert( std::make_pair(MBB,SMBB) );
          CD[t->getBlock()].insert( std::make_pair(MBB,SMBB) );
          t = t->getIDom();
        }
      }
    }
  }


  // dump CD
  for (CD_map_t::iterator I=CD.begin(); I!=CD.end(); ++I) {
    dbgs() << I->first->getNumber() << ": { ";
    for (CD_map_entry_t::iterator EI=I->second.begin(); EI!=I->second.end(); ++EI) {
      dbgs() << "(" << EI->first->getNumber() << "," << EI->second->getNumber() << "), ";
    }
    dbgs() << "}\n";
  }


  // Decompose CD into K and R

  // build sets R, K
  typedef std::vector<CD_map_entry_t>       K_t;
  typedef std::map<MachineBasicBlock*, int> R_t;

  K_t K;
  R_t R;
  int p = 0;
  for (MachineFunction::iterator FI=MF.begin(); FI!=MF.end(); ++FI) {
    MachineBasicBlock *MBB = FI;
    CD_map_entry_t t = CD[MBB];
    int q=-1;
    // try to lookup the control dependence
    for (unsigned int i=0; i<K.size(); i++) {
        if ( t == K[i] ) {
          q = i;
          break;
        }
    }
    if (q != -1) {
      // we already have handled this dependence
      R[MBB] = q;
    } else {
      // new dependence set:
      K.push_back(t);
      R[MBB] = p++;
    }
  }

  // dump R, K
  dbgs() << "map R: MBB -> pN\n";
  for (R_t::iterator EI=R.begin(); EI!=R.end(); ++EI) {
    dbgs() << "R(" << EI->first->getNumber() << ") = p" << EI->second << "\n";
  }
  dbgs() << "map K: pN -> t \\in CD\n";
  for (unsigned int i=0; i<K.size(); i++) {
    dbgs() << "K(p" << i << ") -> {";
    for (CD_map_entry_t::iterator EI=K[i].begin(); EI!=K[i].end(); ++EI) {
      dbgs() << "(" << EI->first->getNumber() << "," << EI->second->getNumber() << "), ";
    }
    dbgs() << "}\n";
  }

  // Augment K
  // TODO


  // map for virtual predicate registers
  std::vector<unsigned> pred_vregs(K.size());

  // insert defs in MBBs (before terminators)
  for (unsigned int i=0; i<K.size(); i++) {
    // create virtual predicate registers
    unsigned preg = RegInfo.createVirtualRegister(&Patmos::PRegsRegClass);
    pred_vregs[i] = preg; // store for later use
    dbgs() << "Created virtual register " << PrintReg(preg) << " for p" << i << "\n";
    for (CD_map_entry_t::iterator EI=K[i].begin(); EI!=K[i].end(); ++EI) {
      MachineBasicBlock *MBBSrc = EI->first, *MBBDst = EI->second;
      // for AnalyzeBranch
      MachineBasicBlock *TBB = NULL, *FBB = NULL;
      SmallVector<MachineOperand, 4> Cond;
      if (!TII->AnalyzeBranch(*MBBSrc, TBB, FBB, Cond)) {
        // According to AnalyzeBranch spec, at a conditional branch, Cond will hold the branch conditions
        // Further, there are two cases for conditional branches:
        // 1. conditional+fallthrough:   TBB holds branch target
        // 2. conditional+unconditional: TBB holds target of conditional branch,
        //                               FBB the target of the unconditional one
        // Hence, the branch condition will always refer to the TBB edge.
        assert( !Cond.empty() && "AnalyzeBranch for SP-IfConversion failed; could not determine branch condition");
        if (MBBDst != TBB)  TII->ReverseBranchCondition(Cond);

        // we insert the predicate definition before any branch (or the end of the MBB)
        MachineBasicBlock::iterator firstTI = MBBSrc->getFirstTerminator();

        MachineInstr *NewMI = AddDefaultPred(BuildMI(*MBBSrc, firstTI, firstTI->getDebugLoc(), TII->get(Patmos::PMOV), preg))
                                  .addOperand(Cond[0]).addOperand(Cond[1]);
        //TODO Is this a proper way dealing with kills?
        // Note that there might be additional instructions inserted for other k \in K
        RegInfo.clearKillFlags(Cond[0].getReg());
        dbgs() << "Insert in BB#" << MBBSrc->getNumber() << ": " << *NewMI;
      } else {
        assert(0 && "AnalyzeBranch failed");
      }
    }
  }

  // TODO Input-independent control-flow?

  // apply predicates (vregs) to all instructions in blocks
  dbgs() << "Applying predicates to MBBs\n";
  for (R_t::iterator RI=R.begin(); RI!=R.end(); ++RI) {
    MachineBasicBlock *MBB = RI->first;
    int pred_idx = RI->second;


    // check for definition edges
    if (K[pred_idx].size()==0) {
      dbgs() << "  skip: no definition predicate for BB#" << MBB->getNumber() << "\n";
      continue;
    }

    unsigned preg = pred_vregs[pred_idx];
    dbgs() << "  applying " << PrintReg(preg) << " to BB#" << MBB->getNumber() << "\n";

    // Iterate the given basic block bottom-up, beginning before the terminators.
    // First, insert a pseudo with the virtual register of the BB as input operand.
    // This is required to be able to predicate instructions generated during register
    // allocation
    // \see PatmosInstrInfo::expandPostRAPseudo()
    MachineBasicBlock::iterator MI=MBB->getFirstTerminator();
    BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(Patmos::PSEUDO_SP_PRED_BBEND))
      .addReg(preg); // we don't set any kill flag; rely on the live-var analysis

    while (MI != MBB->getFirstNonPHI()) {
      --MI;
      assert(!MI->isBundle() && //FIXME
          "PatmosInstrInfo::PredicateInstruction() can't handle bundles");

      // check for terminators - return? //TODO
      if (MI->isReturn()) {
          dbgs() << "    skip return: " << *MI;
          continue;
      }

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
          dbgs() << "    in BB#" << MBB->getNumber() << ": instruction already predicated: " << *MI;
        }
      }
    }
    // After the PHI instructions, add a delimiter pseudo instruction.
    // No -> this is done in the Reduce phase, to avoid copies inserted before BBBEGIN
    //BuildMI(*MBB, MI, MI->getDebugLoc(), TII->get(Patmos::PSEUDO_SP_PRED_BBBEGIN));
  }

}
