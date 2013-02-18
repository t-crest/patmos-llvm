//===-- PatmosSPPredicate.cpp - Predicate insts for single-path code ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass predicates instructions before Post-RA scheduling.
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
#include <queue>
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

    /// Typedefs for control dependence:
    /// CD: MBB -> set of edges
    /// CD describes, which edges an MBB is control-dependent on.
    typedef std::set<std::pair<MachineBasicBlock*,
                               MachineBasicBlock*> > CD_map_entry_t;
    typedef std::map<MachineBasicBlock*, CD_map_entry_t> CD_map_t;

    /// Typedefs for R and K (decomposed CD)
    typedef std::vector<CD_map_entry_t>            K_t;
    typedef std::map<MachineBasicBlock*, unsigned> R_t;



    /// createSPNodeTree - Create an SPNode tree, return the root node.
    /// The tree needs to be destroyed by the client,
    /// by deleting the root node.
    SPNode *createSPNodeTree(const MachineFunction &MF) const;


    /// computeControlDependence - Compute CD for a given function.
    void computeControlDependence(MachineFunction &MF, CD_map_t &CD) const;

    /// decomposeControlDependence - Decompose CD into K and R
    void decomposeControlDependence(MachineFunction &MF, CD_map_t &CD,
                                    K_t &K, R_t &R) const;

    /// computeUpwardsExposedUses - Compute predicates which need to be
    /// initialized with 'false' as they are upwards exposed
    BitVector computeUpwardsExposedUses(MachineFunction &MF,
                                        K_t &K, R_t &R) const;

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

  protected:
    /// Perform the conversion on a given MachineFunction
    void doConvertFunction(MachineFunction &MF);

  public:
    /// PatmosSPPredicate - Initialize with PatmosTargetMachine
    PatmosSPPredicate(const PatmosTargetMachine &tm,
                      PatmosSinglePathInfo &pspi) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        PSPI(pspi) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Single-Path Predicator";
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
        DEBUG( dbgs() << "[Single-Path] Predicating "
                      << MF.getFunction()->getName() << "\n" );
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


// build the SPNode tree in DFS order, creating new SPNodes preorder
static
void createSPNodeSubtree(MachineLoop *loop, SPNode *parent,
                         std::map<const MachineLoop *, SPNode *> &M) {
  // We need to make some assumptions about the loops we can handle for now...
  // allow only one successor for SPNode
  assert( loop->getExitBlock() != NULL &&
          "Allow only one successor for loops!" );
  assert( loop->getExitingBlock() != NULL &&
          "Allow only exactly one exiting edge for loops!" );
  // for now, also:
  assert( loop->getHeader() == loop->getExitingBlock() &&
          "Allow only loops with Header == Exiting Block!" );

  SPNode *N = new SPNode(parent,
                         loop->getHeader(),
                         loop->getExitBlock(),
                         loop->getNumBackEdges()
                         );

  // update map: Loop -> SPNode
  M[loop] = N;

  for (MachineLoop::iterator I = loop->begin(), E = loop->end();
          I != E; ++I) {
    createSPNodeSubtree(*I, N, M);
  }
}


SPNode *PatmosSPPredicate::createSPNodeTree(const MachineFunction &MF) const {
  // Get loop information
  MachineLoopInfo &LI = getAnalysis<MachineLoopInfo>();

  // First, create a SPNode tree
  std::map<const MachineLoop *, SPNode *> M;

  SPNode *Root = new SPNode(NULL, &MF.front(), NULL, 0);

  M[NULL] = Root;

  // iterate over top-level loops
  for (MachineLoopInfo::iterator I=LI.begin(), E=LI.end(); I!=E; ++I) {
    MachineLoop *Loop = *I;
    createSPNodeSubtree(Loop, Root, M);
  }

  // Then, add MBBs to the corresponding SPNodes
  for (MachineFunction::const_iterator FI=MF.begin(), FE=MF.end();
          FI!=FE; ++FI) {
    const MachineBasicBlock *MBB = FI;
    const MachineLoop *Loop = LI[MBB]; // also accounts for NULL (no loop)
    M[Loop]->addMBB(MBB);
  }

  return Root;
}




void PatmosSPPredicate::doConvertFunction(MachineFunction &MF) {

  SPNode *Root = createSPNodeTree(MF);
  Root->dump();

  // let's see if we get the order right
  std::vector<const MachineBasicBlock *> order;
  Root->getOrder(order);
  for(unsigned i=0; i<order.size(); i++) {
    dbgs() << " | " << order[i]->getNumber() << "\n";
  }

  delete Root;

  // CD: MBB -> set of edges
  CD_map_t CD;
  computeControlDependence(MF, CD);

  // decompose CD
  K_t K;
  R_t R;
  decomposeControlDependence(MF, CD, K, R);


  // "Augment K"
  BitVector pred_initialize = computeUpwardsExposedUses(MF, K, R);

  // XXX for debugging
  //MF.viewCFGOnly();

  // contains the "right" virtual register for use of each MBB
  R_t pred_use_vregs;
  insertPredDefinitions(MF, K, R, pred_initialize, pred_use_vregs);


  // TODO Input-independent control-flow?

  applyPredicates(MF, pred_use_vregs);

}



// TODO Maybe an artificial entry node?
void PatmosSPPredicate::computeControlDependence(MachineFunction &MF,
                                                 CD_map_t &CD) const {
  // for CD, we need the Postdom-Tree
  MachinePostDominatorTree &PDT = getAnalysis<MachinePostDominatorTree>();
  assert(PDT.getRoots().size()==1 && "Function must have a single exit node!");


  DEBUG_TRACE( dbgs() << "Post-dominator tree:\n" );
  DEBUG_TRACE( PDT.print(dbgs(), MF.getFunction()->getParent()) );

  // build control dependence
  for (MachineFunction::iterator FI=MF.begin(), FE=MF.end(); FI!=FE; ++FI) {
    MachineBasicBlock *MBB = FI;
    MachineDomTreeNode *ipdom = PDT[MBB]->getIDom();

    for(MachineBasicBlock::succ_iterator SI=MBB->succ_begin(),
                                         SE=MBB->succ_end(); SI!=SE; ++SI) {
      MachineBasicBlock *SMBB = *SI;
      // exclude edges to post-dominaing successors
      if (!PDT.dominates(SMBB, MBB)) {
        // insert the edge MBB->SMBB to all controlled blocks
        for (MachineDomTreeNode *t = PDT[SMBB]; t != ipdom; t = t->getIDom()) {
          CD[t->getBlock()].insert( std::make_pair(MBB,SMBB) );
        }
      }
    } // end for all successors
  } // end for each MBB

  // add control dependence for entry edge NULL -> BB0
  {
    MachineBasicBlock *entryMBB = &MF.front();
    for ( MachineDomTreeNode *t = PDT[entryMBB]; t != NULL; t = t->getIDom() ) {
      CD[t->getBlock()].insert( std::make_pair(
                                  (MachineBasicBlock*)NULL, entryMBB)
                                  );
    }
  }


  DEBUG_TRACE({
    // dump CD
    dbgs() << "Control dependence:\n";
    for (CD_map_t::iterator I=CD.begin(), E=CD.end(); I!=E; ++I) {
      dbgs() << "BB#" << I->first->getNumber() << ": { ";
      for (CD_map_entry_t::iterator EI=I->second.begin(), EE=I->second.end();
           EI!=EE; ++EI) {
        dbgs() << "(" << ((EI->first) ? EI->first->getNumber() : -1) << ","
                      << EI->second->getNumber() << "), ";
      }
      dbgs() << "}\n";
    }
  });
}



void PatmosSPPredicate::decomposeControlDependence(MachineFunction &MF,
                                         CD_map_t &CD, K_t &K, R_t &R) const {
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
  } // end for each MBB

  DEBUG_TRACE({
    // dump R, K
    dbgs() << "Decomposed CD:\n";
    dbgs() << "map R: MBB -> pN\n";
    for (R_t::iterator RI=R.begin(), RE=R.end(); RI!=RE; ++RI) {
      dbgs() << "R(" << RI->first->getNumber() << ") = p" << RI->second << "\n";
    }
    dbgs() << "map K: pN -> t \\in CD\n";
    for (unsigned int i=0; i<K.size(); i++) {
      dbgs() << "K(p" << i << ") -> {";
      for (CD_map_entry_t::iterator EI=K[i].begin(), EE=K[i].end();
            EI!=EE; ++EI) {
        dbgs() << "(" << ((EI->first) ? EI->first->getNumber() : -1) << ","
                      << EI->second->getNumber() << "), ";
      }
      dbgs() << "}\n";
    }
  });
}



BitVector PatmosSPPredicate::computeUpwardsExposedUses(MachineFunction &MF,
                                                       K_t &K, R_t &R) const {
  // ... by solving data-flow equations (upwards-exposed uses)
  std::map<MachineBasicBlock*, BitVector> gen;
  std::map<MachineBasicBlock*, BitVector> kill;
  // fill gen/kill
  for (R_t::iterator EI=R.begin(), EE=R.end(); EI!=EE; ++EI) {
    MachineBasicBlock *MBB = EI->first;

    gen[MBB] = BitVector(K.size());
    kill[MBB] = BitVector(K.size());
    // put R(MBB) into gen (as use)
    gen[MBB].set( EI->second );
  }
  for (unsigned int i=0; i<K.size(); i++) {
    // each MBB defining a predicate kills a use
    for (CD_map_entry_t::iterator KI=K[i].begin(), KE=K[i].end();
         KI!=KE; ++KI) {
      if (!KI->first) continue; // skip entry edge
      kill[KI->first].set(i);
    }
  }
  DEBUG_TRACE({
    dbgs() << "Compute Upwards Exposed Uses\n";
    // dump gen/kill
    dbgs() << "DU: MBB -> gen/kill sets (bvlen " << K.size() << ")\n";
    for (MachineFunction::iterator FI=MF.begin(), FE=MF.end(); FI!=FE; ++FI) {
      MachineBasicBlock *MBB = FI;
      dbgs() << "  BB#" << MBB->getNumber() << " gen: {";
      for (unsigned i=0; i<gen[MBB].size(); i++) {
        if (gen[MBB].test(i)) dbgs() << " p" << i;
      }
      dbgs() << " }  kill: {";
      for (unsigned i=0; i<kill[MBB].size(); i++) {
        if (kill[MBB].test(i)) dbgs() << " p" << i;
      }
      dbgs() << " }\n";
    }
  });

  std::queue<MachineBasicBlock*> worklist;
  std::map<MachineBasicBlock*, BitVector> bvIn;
  // fill worklist initially in dfs postorder
  for (po_iterator<MachineBasicBlock *> POI = po_begin(&MF.front());
                                        POI != po_end(&MF.front());
                                        ++POI) {
      worklist.push(*POI);
      // initially, In = gen
      bvIn[*POI] = BitVector(gen[*POI]);
  }
  // first element is exit node. initialize properly (all)
  bvIn[worklist.front()].set();
  worklist.pop();
  // iterate.
  while (!worklist.empty()) {
    // pop first element
    MachineBasicBlock *MBB = worklist.front();
    worklist.pop();
    // effect
    BitVector bvOut = BitVector(K.size());
    for (MachineBasicBlock::succ_iterator SI=MBB->succ_begin();
         SI!=MBB->succ_end(); ++SI) {
      bvOut |= bvIn[*SI];
    }
    bvOut.reset(kill[MBB]);
    bvOut |= gen[MBB];
    if (bvOut != bvIn[MBB]) {
      DEBUG_TRACE({
          dbgs() << "  Update IN of BB#" << MBB->getNumber() << "{";
          for (unsigned i=0; i<bvOut.size(); i++) {
          if (bvOut.test(i)) dbgs() << " p" << i;
          }
          dbgs() << " }\n";
      });
      bvIn[MBB] = bvOut;
      // add predecessors to worklist
      for (MachineBasicBlock::pred_iterator PI=MBB->pred_begin();
           PI!=MBB->pred_end(); ++PI) {
          worklist.push(*PI);
      }
    }
  } // end of main iteration

  // Augmented elements
  BitVector *pred_initialize = &bvIn[&MF.front()];
  DEBUG_TRACE({
    // dump pN to be initialized
    dbgs() << "Initialization with F:";
    for (unsigned i=0; i<pred_initialize->size(); i++) {
      if (pred_initialize->test(i)) dbgs() << " p" << i;
    }
    dbgs() << "\n";
  });
  return *pred_initialize;
}



void PatmosSPPredicate::insertPredDefinitions(MachineFunction &MF, K_t &K,
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



void PatmosSPPredicate::applyPredicates(MachineFunction &MF,
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
