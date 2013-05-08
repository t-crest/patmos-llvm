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
#include "llvm/ADT/DepthFirstIterator.h"
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
//#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include <map>
#include <set>
#include <algorithm>
#include <sstream>
#include <iostream>


using namespace llvm;


// anonymous namespace
namespace {

  class RegAllocWalker;
  class LinearizeWalker;
  class RAInfo;

  typedef std::map<const SPNode*, RAInfo> RAInfoMap;

  class PatmosSPReduce : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    friend class RegAllocWalker;
    friend class LinearizeWalker;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;

    /// doReduceFunction - Reduce a given MachineFunction
    void doReduceFunction(MachineFunction &MF);

    /// computeRegAlloc - Compute RAInfo for each SPNode of the tree
    void computeRegAlloc(SPNode *root, RAInfoMap &RAInfos, const unsigned R);

    /// insertPredDefinitions - Insert predicate register definitions
    void insertPredDefinitions(RAInfo &R);

    void insertDefsForBV(MachineBasicBlock &MBB,
                         MachineBasicBlock::iterator MI,
                         const RAInfo &R,
                         const BitVector &bv,
                         const SmallVectorImpl<MachineOperand> &Cond);

    /// applyPredicates - Predicate instructions of MBBs
    void applyPredicates(RAInfo &R);

    /// mergeMBBs - Merge the linear sequence of MBBs as possible
    void mergeMBBs(MachineFunction &MF);

    /// getImm32FromBitvector - Returns an Imm32 mask for bits set in bv
    /// NB: for now, bv.size() <= 32
    uint32_t getImm32FromBitvector(BitVector bv) const;

    /// insertPredClr - Insert predicates clear instruction
    void insertPredClr(MachineBasicBlock *MBB, MachineBasicBlock::iterator MI,
                       BitVector bits);

    /// exractPReg - Extract the correct pred to PReg at the beginning of MBB
    void extractPReg(MachineBasicBlock *MBB, unsigned pred);

    /// hasLiveOutPReg - Check if an unavailable PReg must be preserved
    /// in S0 during predicate allocation SPNode on exiting the SPNode
    bool hasLiveOutPReg(const SPNode *N) const;

    // predicate registers
    // Predicate registers un-/used in the function,
    // which are un-/available for allocation here
    std::vector<unsigned> AvailPredRegs;
    std::vector<unsigned> UnavailPredRegs;

    // set of MachineInstrs which should not be predicated in predicate-apply
    // phase
    std::set<MachineInstr *> NoPredApply;

    unsigned GuardsReg; // RReg to hold all predicates
    unsigned PRTmp;     // temporary PReg

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
      if ( PSPI.isEnabled(MF) ) {
        DEBUG( dbgs() << "[Single-Path] Reducing "
                      << MF.getFunction()->getName() << "\n" );
        doReduceFunction(MF);
        changed |= true;
      }
      return changed;
    }
  };

///////////////////////////////////////////////////////////////////////////////

  class LinearizeWalker : public SPNodeWalker {
    private:
      virtual void nextMBB(MachineBasicBlock *);
      virtual void enterSubnode(SPNode *);
      virtual void exitSubnode(SPNode *);

      PatmosSPReduce &Pass;
      MachineFunction &MF;

      MachineBasicBlock *LastMBB;
      const RAInfoMap &RAInfos;
    public:
      explicit LinearizeWalker(PatmosSPReduce &pass, MachineFunction &mf,
                               const RAInfoMap &rainfos)
        : Pass(pass), MF(mf), LastMBB(NULL), RAInfos(rainfos) {}
  };


///////////////////////////////////////////////////////////////////////////////

  class LiveRange {
  friend class RAInfo;
  private:
    uint64_t uses, defs;
    unsigned len;
    void addUse(int pos) { uses |= (1 << pos); }
    void addDef(int pos) { defs |= (1 << pos); }
  public:
    LiveRange(unsigned length) : uses(0), defs(0), len(length) {
      assert(length <= 64 && "Not yet implemented");
    }
    bool isUse(int pos) const { return uses & (1 << pos); }
    bool isDef(int pos) const { return defs & (1 << pos); }
    bool lastUse(int pos) const { return (uses >> (pos+1)) == 0; }
    bool hasDefBefore(int pos) const { return (defs & ((1 << pos)-1)) != 0; }
    bool pastFirstUse(int pos) const {
      return (uses & ((1 << (pos+1))-1)) != 0;
    }
    bool hasNextUseBefore(int pos, const LiveRange &other) const {
      return llvm::CountTrailingZeros_64(uses >> pos)
                < llvm::CountTrailingZeros_64(other.uses >> pos);
    }
    std::string str(void) const {
      std::stringbuf buf;
      char kind[] = { '-', 'u', 'd', 'x' };
      for (unsigned i=0; i<len; i++) {
        int x = 0;
        if (uses & (1 << i)) x += 1;
        if (defs & (1 << i)) x += 2;
        buf.sputc(kind[x]);
      }
      return buf.str();
    }
  };


  class RAInfo {
    private:
      SPNode *Node;
      const std::vector<MachineBasicBlock*> &MBBs;
      std::vector<LiveRange> LRs;
      std::vector<int> DefLocs; // Pred -> loc
      std::set<unsigned> FreeLocs;

      // comparator for predicates, furthest next use
      struct FurthestNextUseComparator {
        RAInfo &RI;
        int pos;
        bool operator()(int a, int b) {
          return RI.LRs[a].hasNextUseBefore(pos, RI.LRs[b]);
        }
        FurthestNextUseComparator(RAInfo &ri, int p) : RI(ri), pos(p) {}
      };

      // store the last location in the loop of the header, if different from
      // its use location.
      void setLastHeaderLoc(int loc) {
        UseLoc &ul = UseLocs[Node->getHeader()];
        if (ul.loc != loc) {
          ul.load = loc;
        }
      }

      int getLoc(void) {
        if (!FreeLocs.empty()) {
          std::set<unsigned>::iterator it = FreeLocs.begin();
          FreeLocs.erase(it);
          return *it;
        }
        // create a new location
        return NumLocs++;
      }

      void freeLoc(unsigned loc) {
        assert(!FreeLocs.count(loc));
        FreeLocs.insert(loc);
      }

      bool hasFreePhys(unsigned numPhysRegs) {
        return (!FreeLocs.empty() && (*FreeLocs.begin() < numPhysRegs) )
          || (NumLocs < numPhysRegs);
      }

    public:
      struct UseLoc {
        int loc, load, spill;
        UseLoc(void) : loc(-1), load(-1), spill(-1) {}
      };
      std::map<const MachineBasicBlock*, UseLoc> UseLocs; // MBB -> loc
      unsigned NumLocs, CumLocs, Offset;


      explicit RAInfo(SPNode *N) :
        Node(N), MBBs(N->getBlocks()),
        LRs(N->getNumPredicates(), LiveRange(N->getBlocks().size()+1)),
        DefLocs(N->getNumPredicates(),-1),
        NumLocs(0), CumLocs(0), Offset(0) {}


      SPNode *getNode(void) const { return Node; }

      void createLiveRanges(void);

      void assignLocations(unsigned R);

      bool needsSpill(void) const {
        return (Offset == 0);
      }

      bool isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const {
        for(unsigned i=0; i<MBBs.size(); i++) {
          if (MBBs[i] == MBB) {
            return !LRs[pred].hasDefBefore(i);
          }
        }
        return false;
      }

      int getUseLoc(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          return UseLocs.at(MBB).loc + Offset;
        }
        return -1;
      }

      int getDefLoc(unsigned pred) const {
        return DefLocs[pred] + Offset;
      }

      void dump(void) const;
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

  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  // Get the unused predicate registers
  for (TargetRegisterClass::iterator I=Patmos::PRegsRegClass.begin(),
      E=Patmos::PRegsRegClass.end(); I!=E; ++I ) {
    if (RegInfo.reg_empty(*I) && *I!=Patmos::P0) {
      AvailPredRegs.push_back(*I);
      DEBUG( dbgs() << "PReg " << TM.getRegisterInfo()->getName(*I)
                    << " available\n" );
    } else {
      UnavailPredRegs.push_back(*I);
    }
  }
  GuardsReg = Patmos::R26;
  PRTmp     = AvailPredRegs.back();
  AvailPredRegs.pop_back();

  const unsigned R = AvailPredRegs.size();
  // harness:
  //const unsigned R = 1;

  /// Map to hold RA infos for each SPNode
  RAInfoMap RAInfos;

  SPNode *root = PSPI.getRootNode();

  computeRegAlloc(root, RAInfos, R);


  // Okay, now actually insert code.
  // for all (sub-)SPNodes
  for (df_iterator<SPNode*> I = df_begin(root), E = df_end(root); I!=E; ++I) {
    SPNode *N = *I;
    RAInfo &R = RAInfos.at(N);

    // Insert predicate definitions.
    insertPredDefinitions(R);

    // Apply predicates.
    // This will also predicate the predicate definitions inserted before,
    // with exceptions. TODO maybe reorder, predicate in insertPredDef...
    applyPredicates(R);

  }

  DEBUG( dbgs() << "Linearize MBBs\n" );
  LinearizeWalker LW(*this, MF, RAInfos);
  PSPI.walkRoot(LW);


  // Following function merges MBBs in the linearized CFG in order to
  // simplify it
  mergeMBBs(MF);

  // Finally, we assign numbers in ascending order to MBBs again.
  MF.RenumberBlocks();
}

void PatmosSPReduce::computeRegAlloc(SPNode *root, RAInfoMap &RAInfos,
                                     const unsigned R) {
  DEBUG( dbgs() << "RegAlloc\n" );

  // perform reg-allocation in post-order to compute cumulative location
  // numbers in one go
  for (po_iterator<SPNode*> I = po_begin(root), E = po_end(root); I!=E; ++I) {
    SPNode *N = *I;
    // create RAInfo for SPNode
    RAInfos.insert( std::make_pair(N, RAInfo(N)) );
    RAInfo &RI = RAInfos.at(N);

    RI.createLiveRanges();

    // we have built all live ranges in N, now it is time to perform a linear
    // scan and assign (abstract) locations for predicates at each block
    RI.assignLocations(R);

    // we have already visited all children (in the PO traversal),
    // synthesize cumulative # of locations
    unsigned maxChildPreds = 0;
    for(SPNode::child_iterator CI = N->child_begin(), CE = N->child_end();
        CI != CE; ++CI) {
      SPNode *CN = *CI;
      maxChildPreds = std::max(RAInfos.at(CN).CumLocs, maxChildPreds);
    }
    RI.CumLocs = RI.NumLocs + maxChildPreds;

  } // end of PO traversal for RegAlloc


  // Visit all nodes in DF order to compute offsets
  // (Offset is inherited during traversal)
  for (df_iterator<SPNode*> I = df_begin(root), E = df_end(root); I!=E; ++I) {
    SPNode *N = *I;

    RAInfo &RI = RAInfos.at(N);

    if (!N->isTopLevel()) {
      RAInfo &RP = RAInfos.at(N->getParent());
      // check if we must spill the PRegs
      // Parent.num + N.cum <= size  --> no spill!
      if ( RP.NumLocs + RI.CumLocs <= R ) {
        // compute offset
        RI.Offset = RP.NumLocs + RP.Offset;
      }
    }

    DEBUG( RI.dump() );
  } // end df


}

void PatmosSPReduce::insertPredDefinitions(RAInfo &R) {
  DEBUG( dbgs() << "Insert Predicate Definitions\n" );

  SPNode &N = *R.getNode();

  // For each MBB, check defs
  for (SPNode::iterator I=N.begin(), E=N.end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;
    const PredDefInfo *DI = N.getDefInfo(MBB);

    // check for definitions
    if (!DI) continue;

    DEBUG( dbgs() << " - MBB#" << MBB->getNumber() << "\n" );

    // get the branch condition
    SmallVector<MachineOperand, 2> Cond(DI->getCond());

    // insert the predicate definitions before any branch at the MBB end
    MachineBasicBlock::iterator firstTI = MBB->getFirstTerminator();

    bool condKill = Cond[0].isKill(); // store kill flag
    Cond[0].setIsKill(false);

    // copy branch condition to true edge definitions
    insertDefsForBV(*MBB, firstTI, R, DI->getTrue(), Cond);

    // reverse Cond in place
    TII->ReverseBranchCondition(Cond);

    // copy reversed branch condition to false edge definitions
    insertDefsForBV(*MBB, firstTI, R, DI->getFalse(), Cond);

    // restore kill flag at the last use
    prior(firstTI)->findRegisterUseOperand(Cond[0].getReg())
                  ->setIsKill(condKill);

  } // end for each MBB
}

void PatmosSPReduce::insertDefsForBV(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MI,
                                const RAInfo &R,
                                const BitVector &bv,
                                const SmallVectorImpl<MachineOperand> &Cond) {

  DebugLoc DL(MI->getDebugLoc());

  // get the pred reg for the current block
  int useloc = R.getUseLoc(&MBB);
  assert(useloc < (int)AvailPredRegs.size() && "Use loc must be a register");

  // If we encounter a definition of the predicate that is used for this MBB,
  // we need to place this definition as last instruction of the MBB
  // in order to stay semantically correct (for following definitions).
  // This pointer will be assigned, to such an instruction.
  MachineInstr *DefUseMI = NULL;

  for (int r=bv.find_first(); r!=-1; r=bv.find_next(r)) {
    int loc = R.getDefLoc(r);
    assert(loc != -1);
    assert(loc < (int)AvailPredRegs.size() && "Not yet implemented");

    MachineInstr *DefMI;

    if (R.getNode()->getNumDefEdges(r) > 1) {
      // if this is the first definition, we unconditionally need to
      // initialize it to false
      // we can skip this, if useloc==-1 (i.e. P0=true) anyway
      if (R.isFirstDef(&MBB, r) && !(useloc == -1)) {
        DebugLoc DL;
        MachineInstr *InitMI = AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::PCLR), AvailPredRegs[loc]));
        // the PCLR instruction must not be predicated
        NoPredApply.insert(InitMI);
      }
      DefMI = AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::PMOV), AvailPredRegs[loc]))
          .addOperand(Cond[0]).addOperand(Cond[1]);
    } else {
      DefMI = AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::PAND),  AvailPredRegs[loc]))
        .addReg( (useloc!=-1) ? AvailPredRegs[useloc] : Patmos::P0)
        .addImm(0) // current pred
        .addOperand(Cond[0]).addOperand(Cond[1]); // condition

      // the PAND instruction must not be predicated
      NoPredApply.insert(DefMI);

    }

    // remember this instruction if it has to be the last one
    if (useloc == loc) {
      DefUseMI = DefMI;
    }
  }
  // We have seen a definition of the currently in use predicate,
  // let's move this right to the end
  if (DefUseMI && !(static_cast<MachineInstr*>(prior(MI))==DefUseMI)) {
    MBB.splice(MI, &MBB, DefUseMI);
  }

}

void PatmosSPReduce::applyPredicates(RAInfo &R) {
  DEBUG( dbgs() << "Applying predicates to MBBs\n" );

  SPNode &N = *R.getNode();
  // for each MBB
  for (SPNode::iterator I=N.begin(), E=N.end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;

    // Subheaders are treated in "their" SPNode.
    // The necessary glue code (copy predicates etc) is done in the linearize
    // walk.
    if (N.isSubHeader(MBB))
      continue;

    int loc = R.getUseLoc(MBB);
    assert(loc != -1 || N.isTopLevel());
    assert(loc < (int)AvailPredRegs.size() && "Not yet implemented");

    // check for use predicate
    // TODO avoid hardcoding of p0
    if (loc==-1) {
      DEBUG_TRACE( dbgs() << "  skip: no guard for MBB#" << MBB->getNumber()
                    << "\n" );
      continue;
    }

    unsigned use_preg = AvailPredRegs[loc];
    DEBUG_TRACE( dbgs() << "  applying "
                        << TM.getRegisterInfo()->getName(use_preg)
                        << " to MBB#" << MBB->getNumber() << "\n" );

    // apply predicate to all instructions in block
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                     ME = MBB->getFirstTerminator();
                                     MI != ME; ++MI) {
      assert(!MI->isBundle() &&
             "PatmosInstrInfo::PredicateInstruction() can't handle bundles");

      // skip MIs in the tabu set
      if (NoPredApply.count(MI))
        continue;

      // check for terminators - return? //TODO
      if (MI->isReturn()) {
          DEBUG_TRACE( dbgs() << "    skip return: " << *MI );
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
          PO1.setReg(use_preg);
          PO2.setImm(0);
        } else {
          //TODO handle already predicated instructions better?
          DEBUG_TRACE( dbgs() << "    in MBB#" << MBB->getNumber()
                        << ": instruction already predicated: " << *MI );
          // read out the predicate
          int i = MI->findFirstPredOperandIdx();
          assert(i != -1);
          MachineOperand &PO1 = MI->getOperand(i);
          MachineOperand &PO2 = MI->getOperand(i+1);
          // build a new predicate := use_preg & old pred
          AddDefaultPred(BuildMI(*MBB, MI, MI->getDebugLoc(),
                              TII->get(Patmos::PAND), PRTmp))
                .addReg(use_preg).addImm(0)
                .addOperand(PO1).addOperand(PO2);
          PO1.setReg(PRTmp); // FIXME
          PO2.setImm(0);
        }
      }
    } // for each instruction in MBB
  } // end for each MBB
}




void PatmosSPReduce::mergeMBBs(MachineFunction &MF) {
  DEBUG( dbgs() << "Merge MBBs\n" );

  // first, obtain the sequence of MBBs in DF order (as copy!)
  std::vector<MachineBasicBlock*> order(df_begin(&MF.front()),
                                        df_end(  &MF.front()));


  std::vector<MachineBasicBlock*>::iterator I = order.begin(),
                                            E = order.end();

  MachineBasicBlock *BaseMBB = *I;
  DEBUG_TRACE( dbgs() << "Base MBB#" << BaseMBB->getNumber() << "\n" );
  // iterate through order of MBBs
  while (++I != E) {
    // get MBB of iterator
    MachineBasicBlock *MBB = *I;

    if (MBB->pred_size() == 1) {
      DEBUG_TRACE( dbgs() << "  Merge MBB#" << MBB->getNumber() << "\n" );
      // transfer the instructions
      BaseMBB->splice(BaseMBB->end(), MBB, MBB->begin(), MBB->end());
      // remove the edge between BaseMBB and MBB
      BaseMBB->removeSuccessor(MBB);
      // BaseMBB gets the successors of MBB instead
      BaseMBB->transferSuccessors(MBB);
      // remove MBB from MachineFunction
      MF.erase(MBB);

      if (BaseMBB->succ_size() > 1) {
        // we have encountered a backedge
        BaseMBB = *(++I);
        DEBUG_TRACE( dbgs() << "Base MBB#" << BaseMBB->getNumber() << "\n" );
      }
    } else {
      BaseMBB = MBB;
      DEBUG_TRACE( dbgs() << "Base MBB#" << BaseMBB->getNumber() << "\n" );
    }
  }
  // invalidate order
  order.clear();
}





uint32_t PatmosSPReduce::getImm32FromBitvector(BitVector bv) const {
  assert( bv.size() <= 32);
  uint32_t res = 0;
  for (unsigned i=0; i<bv.size() && i<32; i++) {
    if (bv.test(i)) {
      res |= (1 << i);
    }
  }
  return res;
}



void PatmosSPReduce::insertPredClr(MachineBasicBlock *MBB,
                                   MachineBasicBlock::iterator MI,
                                   BitVector bits) {
  uint32_t imm = getImm32FromBitvector(bits);
  AddDefaultPred(BuildMI(*MBB, MI, MI->getDebugLoc(),
        TII->get(Patmos::ANDl), GuardsReg))
    .addReg(GuardsReg).addImm(~imm); // bitwise negated imm
}


void PatmosSPReduce::extractPReg(MachineBasicBlock *MBB, unsigned pred) {
  DebugLoc DL;
  unsigned PReg = 0;
  MachineBasicBlock::iterator MI = MBB->begin();
  // LI $rtr, pred
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::LIi),
    Patmos::RTR)).addImm(pred);
  // BTEST $Guards, $rtr
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::BTEST),
    PReg)).addReg(GuardsReg).addReg(Patmos::RTR);
}



bool PatmosSPReduce::hasLiveOutPReg(const SPNode *N) const {
  MachineBasicBlock *SuccMBB = N->getSuccMBB();
  for (unsigned i=0; i<UnavailPredRegs.size(); i++) {
    if (SuccMBB->isLiveIn(UnavailPredRegs[i])) {
      return true;
    }
  }
  return false;
}


///////////////////////////////////////////////////////////////////////////////
//  RAInfo methods
///////////////////////////////////////////////////////////////////////////////



void RAInfo::createLiveRanges(void) {
  // create live range infomation for each predicate
  for (unsigned i=0, e=MBBs.size(); i<e; i++) {
    MachineBasicBlock *MBB = MBBs[i];
    // insert use
    LRs[Node->getPredUse(MBB)].addUse(i);
    // insert defs
    const PredDefInfo *DI = Node->getDefInfo(MBB);
    if (DI) {
      for (int r = DI->getBoth().find_first(); r != -1;
          r = DI->getBoth().find_next(r)) {
        LRs[r].addDef(i);
      }
    }
  }
  // add a use for header predicate
  if (!Node->isTopLevel()) {
    LRs[0].addUse(MBBs.size());
  }
}


void RAInfo::assignLocations(unsigned R) {
  // vector to keep track of locations during the scan
  std::vector<int> curLocs(Node->getNumPredicates(),-1);

  for (unsigned i=0, e=MBBs.size(); i<e; i++) {
    MachineBasicBlock *MBB = MBBs[i];

    // (1) handle use
    unsigned usePred = Node->getPredUse(MBB);
    // for the top-level entry, we don't need to assign a location,
    // as we will use p0 - TODO: interprocedural handling
    if (!(usePred==0 && Node->isTopLevel())) {
      UseLoc UL;

      int &curUseLoc = curLocs[usePred];

      assert( MBB == Node->getHeader() || i>0 );

      if ( MBB != Node->getHeader() ) {
        // each use must be preceded by a location assignment
        assert( curUseLoc >= 0 );
        // if previous location was not a register, we have to allocate
        // a register and/or possibly spill
        if ( curUseLoc >= (int)R ) {
          if (hasFreePhys(R)) {
            UL.load = curUseLoc;
            UL.loc = getLoc(); // gets a register
            // reassign
            curUseLoc = UL.loc;
          } else {
            // spill and reassign
            // order predicates wrt furthest next use
            std::vector<unsigned> order;
            for(unsigned j=0; j<LRs.size(); j++) {
              if (curLocs[j] < (int)R) {
                assert(curLocs[j] > -1);
                order.push_back(j);
              }
            }
            std::sort(order.begin(), order.end(),
                FurthestNextUseComparator(*this,i));
            unsigned furthestPred = order.back();
            int stackLoc = getLoc(); // new stack loc
            assert( stackLoc >= (int)R );
            freeLoc( curUseLoc );
            UL.load  = curUseLoc;
            UL.loc   = curLocs[furthestPred];
            // differentiate between already used and not yet used
            if (LRs[furthestPred].pastFirstUse(i)) {
              UL.spill = stackLoc;
            } else {
              DefLocs[furthestPred] = stackLoc;
            }
            curUseLoc = curLocs[furthestPred];
            curLocs[furthestPred] = stackLoc;
          }
        } else {
          // everything stays as is
          UL.loc = curUseLoc;
        }
      } else {
        assert(usePred == 0);
        // we get a loc for the header predicate
        curLocs[0] = DefLocs[0] = UL.loc = getLoc();
        assert(UL.loc == 0);
      }

      // (2) retire locations
      if (LRs[usePred].lastUse(i)) {
        freeLoc( curUseLoc );
        curUseLoc = -1;
      }
      UseLocs[MBB] = UL;
    }

    // (3) handle definitions in this basic block.
    //     if we need to get new locations for predicates (loc==-1),
    //     assign new ones in nearest-next-use order
    const PredDefInfo *DI = Node->getDefInfo(MBB);
    if (DI) {
      std::vector<unsigned> order;
      for (int r = DI->getBoth().find_first(); r != -1;
          r = DI->getBoth().find_next(r)) {
        if (curLocs[r] == -1) {
          // need to get a new loc for predicate r
          order.push_back(r);
        }
      }
      std::sort(order.begin(), order.end(),
          FurthestNextUseComparator(*this,i));
      // nearest use is in front
      for (unsigned j=0; j<order.size(); j++) {
        unsigned pred = order[j];
        curLocs[pred] = DefLocs[pred] = getLoc();
      }
    }

    //for (unsigned j=0; j<curLocs.size(); j++) {
    //  DEBUG( dbgs() << " " << curLocs[j] );
    //}
  }
  // What is the location of the header predicate after handling all blocks?
  // We store this location, as it is where the next iteration has to get it
  // from.
  setLastHeaderLoc(curLocs[0]);
}


void RAInfo::dump() const {
  dbgs() << "[MBB#"     << Node->getHeader()->getNumber()
         <<  "] depth=" << Node->getDepth() << "\n";

  for(unsigned i=0; i<LRs.size(); i++) {
    const LiveRange &LR = LRs[i];
    dbgs() << "  LR(p" << i << ") = [" << LR.str() << "]\n";
  }

  for (unsigned i=0, e=MBBs.size(); i<e; i++) {
    MachineBasicBlock *MBB = MBBs[i];

    dbgs() << "  " << i << "| MBB#" << MBB->getNumber();
    if (UseLocs.count(MBB)) {
      const UseLoc &UL = UseLocs.at(MBB);
      dbgs() << "  loc=" << UL.loc << " load=" << UL.load
          << " spill=" << UL.spill;
    }
    dbgs() << "\n";
  }

  dbgs() << "  DefLocs: ";
  for (unsigned j=0; j<DefLocs.size(); j++) {
    dbgs() << " p" << j << "=" << DefLocs[j];
  }
  dbgs() << "\n";

  dbgs() << "  NumLocs: " << NumLocs << "\n"
            "  CumLocs: " << CumLocs << "\n"
            "  Offset:  " << Offset  << "\n";
}



///////////////////////////////////////////////////////////////////////////////
//  LinearizeWalker methods
///////////////////////////////////////////////////////////////////////////////

void LinearizeWalker::nextMBB(MachineBasicBlock *MBB) {
  DEBUG_TRACE( dbgs() << "| MBB#" << MBB->getNumber() << "\n" );

  // remove all successors
  for ( MachineBasicBlock::succ_iterator SI = MBB->succ_begin();
        SI != MBB->succ_end();
        SI = MBB->removeSuccessor(SI) )
        ; // no body

  // remove the branch at the end of MBB
  Pass.TII->RemoveBranch(*MBB);

  if (LastMBB) {
    // add to the last MBB as successor
    LastMBB->addSuccessor(MBB);
    // move in the code layout
    MBB->moveAfter(LastMBB);
  }
  // keep track of tail
  LastMBB = MBB;
}


void LinearizeWalker::enterSubnode(SPNode *N) {

  // We don't create a preheader for entry.
  if (N->isTopLevel()) return;

  const TargetRegisterInfo *TRI = Pass.TM.getRegisterInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // insert loop preheader to spill predicates / load loop bound
  MachineBasicBlock *PrehdrMBB = MF.CreateMachineBasicBlock();
  MF.push_back(PrehdrMBB);


  const RAInfo &RI = RAInfos.at(N),
               &RP = RAInfos.at(N->getParent());

  DebugLoc DL;
  if (RI.needsSpill()) {
    // we create a SBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = PMFI.SinglePathSpillFIs[N->getDepth()-1];
    unsigned tmpReg = Patmos::R26;
    Pass.TII->copyPhysReg(*PrehdrMBB, PrehdrMBB->end(), DL,
        tmpReg, Patmos::S0, false);
    MachineInstr *storeMI =
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SBC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg);

    TRI->eliminateFrameIndex(storeMI, 0, NULL);
  }

  // copy the header predicate for the subloop
  const MachineBasicBlock *HeaderMBB = N->getHeader();
  int outerloc = RP.getUseLoc(HeaderMBB),
      innerloc = RI.getUseLoc(HeaderMBB);
  // TODO handle load from stack slot? (once enabled in regalloc)
  if (outerloc != innerloc) {
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
          Pass.TII->get(Patmos::PMOV), Pass.AvailPredRegs[innerloc]))
      .addReg( (outerloc!=-1) ? Pass.AvailPredRegs[outerloc] : Patmos::P0 )
      .addImm(0);
  }

  if (N->hasLoopBound()) {
    // Create an instruction to load the loop bound
    //FIXME load the actual loop bound
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
      Pass.TII->get(Patmos::LIi),
      Patmos::RTR)).addImm(1000);

  }

  // append the preheader
  nextMBB(PrehdrMBB);
}


void LinearizeWalker::exitSubnode(SPNode *N) {

  MachineBasicBlock *HeaderMBB = N->getHeader();
  DEBUG_TRACE( dbgs() << "NodeRange [MBB#" <<  HeaderMBB->getNumber()
                <<  ", MBB#" <<  LastMBB->getNumber() << "]\n" );

  if (N->isTopLevel()) return;

  const TargetRegisterInfo *TRI = Pass.TM.getRegisterInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // insert backwards branch to header at the last block
  // TODO loop iteration counts
  MachineBasicBlock *BranchMBB = MF.CreateMachineBasicBlock();
  MF.push_back(BranchMBB);
  // weave in before inserting the branch (otherwise it'll be removed again)
  nextMBB(BranchMBB);

  // now we can fill the MBB with instructions:


  const RAInfo &RI = RAInfos.at(N);

  DebugLoc DL;

  // get the header predicate and create a branch to the header
  int headerloc = RI.getUseLoc(HeaderMBB);
  // TODO get from stack slot
  // insert branch
  BuildMI(*BranchMBB, BranchMBB->end(), DL, Pass.TII->get(Patmos::BR))
    .addReg(Pass.AvailPredRegs[headerloc]).addImm(0)
    .addMBB(HeaderMBB);
  BranchMBB->addSuccessor(HeaderMBB);


  // FIXME in master: why do we add callee saved regs at restoreSpillRegs
  // as liveins???
  //assert(!Pass.hasLiveOutPReg(N) &&
  //        "Unimplemented: handling of live-out PRegs of loops");

  // create a post-loop MBB to restore the spill predicates, if necessary
  if (RI.needsSpill()) {
    MachineBasicBlock *PostMBB = MF.CreateMachineBasicBlock();
    MF.push_back(PostMBB);
    // we create a LBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = PMFI.SinglePathSpillFIs[N->getDepth()-1];
    unsigned tmpReg = Pass.GuardsReg;
    MachineInstr *loadMI =
      AddDefaultPred(BuildMI(*PostMBB, PostMBB->end(), DL,
            Pass.TII->get(Patmos::LBC), tmpReg))
      .addFrameIndex(fi).addImm(0); // address

    TRI->eliminateFrameIndex(loadMI, 0, NULL);
    // assign to S0
    Pass.TII->copyPhysReg(*PostMBB, PostMBB->end(), DL,
                          Patmos::S0, tmpReg, true);
    nextMBB(PostMBB);
  }
}

///////////////////////////////////////////////////////////////////////////////

