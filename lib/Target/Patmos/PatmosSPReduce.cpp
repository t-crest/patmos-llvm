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

  class LinearizeWalker;
  class RAInfo;

  class PatmosSPReduce : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    friend class LinearizeWalker;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;
    const TargetRegisterInfo *TRI;

    const PatmosMachineFunctionInfo *PMFI;

    /// doReduceFunction - Reduce a given MachineFunction
    void doReduceFunction(MachineFunction &MF);

    /// computeRegAlloc - Compute RAInfo for each SPNode of the tree
    void computeRegAlloc(SPNode *root);

    /// createRAInfo - Create a new RAInfo for an SPNode
    RAInfo &createRAInfo(SPNode *N);

    /// insertPredDefinitions - Insert predicate register definitions
    void insertPredDefinitions(SPNode *N);

    void insertDefsForBV(MachineBasicBlock &MBB,
                         MachineBasicBlock::iterator MI,
                         const RAInfo &R,
                         const BitVector &bv,
                         const SmallVectorImpl<MachineOperand> &Cond);

    /// applyPredicates - Predicate instructions of MBBs
    void applyPredicates(SPNode *N);

    /// insertUseSpillLoad - Insert Spill/Load code at the beginning of the
    /// given MBB
    void insertUseSpillLoad(const RAInfo &R, MachineBasicBlock *MBB);

    /// insertPredicateLoad - Insert code to load from a spill stack slot to
    /// a predicate register.
    void insertPredicateLoad(MachineBasicBlock *MBB,
                             MachineBasicBlock::iterator MI,
                             int loc, unsigned target_preg);

    /// getUsePReg - Returns the physical predicate register for the guard of
    /// the given MBB, if getP0 is set, it returns P0 if no guard is set for
    /// MBB, else NoRegister.
    unsigned getUsePReg(const RAInfo &R, const MachineBasicBlock *MBB,
                        bool getP0=false);

    /// mergeMBBs - Merge the linear sequence of MBBs as possible
    void mergeMBBs(MachineFunction &MF);

    /// hasLiveOutPReg - Check if an unavailable PReg must be preserved
    /// in S0 during predicate allocation SPNode on exiting the SPNode
    bool hasLiveOutPReg(const SPNode *N) const;

    /// Map to hold RA infos for each SPNode
    std::map<const SPNode*, RAInfo> RAInfos;

    // Predicate registers un-/used in the function,
    // which are un-/available for allocation here
    std::vector<unsigned> AvailPredRegs;
    std::vector<unsigned> UnavailPredRegs;

    unsigned GuardsReg; // RReg to hold all predicates
    unsigned PRTmp;     // temporary PReg

  public:
    /// PatmosSPReduce - Initialize with PatmosTargetMachine
    PatmosSPReduce(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
      TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
      TRI(tm.getRegisterInfo())
    {}

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
      PMFI = MF.getInfo<PatmosMachineFunctionInfo>();
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
    public:
      explicit LinearizeWalker(PatmosSPReduce &pass, MachineFunction &mf)
        : Pass(pass), MF(mf), LastMBB(NULL) {}
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

///////////////////////////////////////////////////////////////////////////////

  class RAInfo {
    public:
      // the SPNode this RAInfo belongs to
      const SPNode *Node;
    private:
      // The number of colors (physically available registers)
      const unsigned NumColors;
      // the sequence of MBBs in topological order (ref to Node->Blocks)
      const std::vector<MachineBasicBlock*> &MBBs;
      // the live ranges of predcicates (index = predicate number)
      std::vector<LiveRange> LRs;
      // each predicate has one definition location (index = predicate number)
      std::vector<int> DefLocs; // Pred -> loc
      std::set<unsigned> FreeLocs;
      unsigned NumLocs, CumLocs, Offset, SpillOffset;
      struct UseLoc {
        int loc, load, spill;
        UseLoc(void) : loc(-1), load(-1), spill(-1) {}
      };
      std::map<const MachineBasicBlock*, UseLoc> UseLocs; // MBB -> loc

      // comparator for predicates, furthest next use
      struct FurthestNextUseComparator {
        RAInfo &RI;
        int pos;
        bool operator()(int a, int b) {
          return RI.LRs[a].hasNextUseBefore(pos, RI.LRs[b]);
        }
        FurthestNextUseComparator(RAInfo &ri, int p) : RI(ri), pos(p) {}
      };

      void createLiveRanges(void);
      void assignLocations(void);

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
      explicit RAInfo(SPNode *N, unsigned numcolors) :
        Node(N), NumColors(numcolors), MBBs(N->getBlocks()),
        LRs(N->getNumPredicates(), LiveRange(N->getBlocks().size()+1)),
        DefLocs(N->getNumPredicates(),-1),
        NumLocs(0), CumLocs(0), Offset(0), SpillOffset(0) {
          createLiveRanges();
          assignLocations();
        }

      unsigned getCumLocs(void) const { return CumLocs; }
      void setCumLocs(unsigned maxFromChildren) {
        CumLocs = NumLocs + maxFromChildren;
      }


      void computePhysOffset(const RAInfo &ParentRI) {
        // check if we must spill the PRegs
        // Parent.num + N.cum <= size  --> no spill!
        if ( ParentRI.NumLocs + CumLocs <= NumColors ) {
          // compute offset
          Offset = ParentRI.NumLocs + ParentRI.Offset;
        }
      }

      // assignSpillOffset - Stores the offset provided as spill offset,
      // and updates the offset by adding the number of spilled predicates.
      void assignSpillOffset(unsigned &spillOffset) {
        // assign the spill offset, increment
        if (NumLocs > NumColors) {
          this->SpillOffset = spillOffset;
          spillOffset += NumLocs - NumColors;
        }
      }

      bool needsNodeSpill(void) const {
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

      bool hasSpillLoad(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          const UseLoc &ul = UseLocs.at(MBB);
          return (ul.spill!=-1) || (ul.load!=-1);
        }
        return false;
      }

      int getUseLoc(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          int loc = UseLocs.at(MBB).loc + Offset;
          assert( loc < (int)NumColors );
          return loc;
        }
        return -1;
      }

      int getLoadLoc(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          int loc = UseLocs.at(MBB).load;
          if (loc != -1) {
            assert( loc >= (int)NumColors );
            return (loc - NumColors) + SpillOffset;
          }
        }
        return -1;
      }

      int getSpillLoc(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          int loc = UseLocs.at(MBB).spill;
          if (loc != -1) {
            assert( loc >= (int)NumColors );
            return (loc - NumColors) + SpillOffset;
          }
        }
        return -1;
      }

      /// getDefLoc - get the definition location for a given predicate.
      /// The location is stored in loc, the function returns true if the
      // location is a physical register.
      bool getDefLoc(unsigned &loc, unsigned pred) const {
        int dloc = DefLocs[pred];
        assert(dloc != -1);
        bool isreg = dloc < (int)NumColors;
        if (isreg) {
          loc = dloc + Offset;
        } else {
          loc = (dloc - NumColors) + SpillOffset;
        }
        return isreg;
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

  RAInfos.clear();

  // Get the unused predicate registers
  for (TargetRegisterClass::iterator I=Patmos::PRegsRegClass.begin(),
      E=Patmos::PRegsRegClass.end(); I!=E; ++I ) {
    if (RegInfo.reg_empty(*I) && *I!=Patmos::P0) {
      AvailPredRegs.push_back(*I);
      DEBUG( dbgs() << "PReg " << TRI->getName(*I)
                    << " available\n" );
    } else {
      UnavailPredRegs.push_back(*I);
    }
  }
  GuardsReg = Patmos::R26;
  // Get a temporary predicate register, which must not be used for allocation
  PRTmp     = AvailPredRegs.back();
  AvailPredRegs.pop_back();


  SPNode *root = PSPI.getRootNode();

  computeRegAlloc(root);

  // Okay, now actually insert code, handling nodes in no particular order
  for (df_iterator<SPNode*> I = df_begin(root), E = df_end(root); I!=E; ++I) {
    SPNode *N = *I;

    // Predicate the instructions of blocks in N, also inserting spill/load
    // of predicates not in registers.
    applyPredicates(N);

    // Insert predicate definitions.
    insertPredDefinitions(N);
  }

  // Following walk of the SPNode tree linearizes the CFG structure,
  // inserting MBBs as required (preheader, spill/restore, loop counts, ...)
  DEBUG( dbgs() << "Linearize MBBs\n" );
  LinearizeWalker LW(*this, MF);
  PSPI.walkRoot(LW);

  // Following function merges MBBs in the linearized CFG in order to
  // simplify it
  mergeMBBs(MF);

  // Finally, we assign numbers in ascending order to MBBs again.
  MF.RenumberBlocks();
}


void PatmosSPReduce::computeRegAlloc(SPNode *root) {
  DEBUG( dbgs() << "RegAlloc\n" );

  // perform reg-allocation in post-order to compute cumulative location
  // numbers in one go
  for (po_iterator<SPNode*> I = po_begin(root), E = po_end(root); I!=E; ++I) {
    SPNode *N = *I;
    // create RAInfo for SPNode
    RAInfo &RI = createRAInfo(N);

    // Because this is a post-order traversal, we have already visited
    // all children. Synthesize the cumulative number of locations
    unsigned maxChildPreds = 0;
    for(SPNode::child_iterator CI = N->child_begin(), CE = N->child_end();
        CI != CE; ++CI) {
      SPNode *CN = *CI;
      maxChildPreds = std::max(RAInfos.at(CN).getCumLocs(), maxChildPreds);
    }
    RI.setCumLocs(maxChildPreds);

  } // end of PO traversal for RegAlloc


  // Visit all nodes in depth-first order to compute offsets:
  // - Offset is inherited during traversal
  // - SpillOffset is assigned increased from left to right
  unsigned spillLocCnt = 0;
  for (df_iterator<SPNode*> I = df_begin(root), E = df_end(root); I!=E; ++I) {
    SPNode *N = *I;

    RAInfo &RI = RAInfos.at(N);

    // compute offset for physically available registers
    if (!N->isTopLevel()) {
      RI.computePhysOffset( RAInfos.at(N->getParent()) );
    }
    // assign and update spillLocCnt
    RI.assignSpillOffset(spillLocCnt);

    DEBUG( RI.dump() );
  } // end df


}



RAInfo& PatmosSPReduce::createRAInfo(SPNode *N) {
  assert(!RAInfos.count(N) && "Already having RAInfo for Node!");
  // The number of colors for allocation is AvailPredRegs.size()
  RAInfos.insert( std::make_pair(N, RAInfo(N,
                                           AvailPredRegs.size()
                                           )) );
  return RAInfos.at(N);
}


void PatmosSPReduce::insertPredDefinitions(SPNode *N) {
  DEBUG( dbgs() << "Insert Predicate Definitions\n" );

  RAInfo &R = RAInfos.at(N);

  // For each MBB, check defs
  for (SPNode::iterator I=N->begin(), E=N->end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;
    const PredDefInfo *DI = N->getDefInfo(MBB);

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
    //prior(firstTI)->findRegisterUseOperand(Cond[0].getReg())
    //              ->setIsKill(condKill);
    for (MachineBasicBlock::iterator lastMI = prior(firstTI),
                                     firstMI = MBB->begin();
                                     lastMI != firstMI; --lastMI) {
      MachineOperand *MO;
      if ((MO = lastMI->findRegisterUseOperand(Cond[0].getReg())) != NULL) {
        MO->setIsKill(condKill);
        break;
      }
    }

  } // end for each MBB
}

void PatmosSPReduce::insertDefsForBV(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MI,
                                const RAInfo &R,
                                const BitVector &bv,
                                const SmallVectorImpl<MachineOperand> &Cond) {

  DebugLoc DL(MI->getDebugLoc());

  // get the pred reg for the current block
  unsigned use_preg = getUsePReg(R, &MBB, true);

  // If we encounter a definition of the predicate that is used for this MBB,
  // we need to place this definition as last instruction of the MBB
  // in order to stay semantically correct (for following definitions).
  // This pointer will be assigned, to such an instruction.
  MachineInstr *DefUseMI = NULL;

  for (int r=bv.find_first(); r!=-1; r=bv.find_next(r)) {
    unsigned loc;

    // Get the location for predicate r. The function returns true
    // if it is a register, otherwise false. The resulting location is
    // stored in loc.
    if (R.getDefLoc(loc, r)) {

      // The definition location of the predicate is a physical register.
      MachineInstr *DefMI;
      if (R.Node->getNumDefEdges(r) > 1) {
        // if this is the first definition, we unconditionally need to
        // initialize it to false
        // we can skip this, if use_preg=true anyway
        if (R.isFirstDef(&MBB, r) && (use_preg != Patmos::P0)) {
          // the PCLR instruction must not be predicated
          AddDefaultPred(BuildMI(MBB, MI, DL,
                TII->get(Patmos::PCLR), AvailPredRegs[loc]));
        }
        DefMI = BuildMI(MBB, MI, DL,
            TII->get(Patmos::PMOV), AvailPredRegs[loc])
          // guard operand
          .addReg(use_preg).addImm(0)
          .addOperand(Cond[0]).addOperand(Cond[1]); // condition
      } else {
        // the PAND instruction must not be predicated
        DefMI = AddDefaultPred(BuildMI(MBB, MI, DL,
              TII->get(Patmos::PAND), AvailPredRegs[loc]))
          // current guard as source
          .addReg(use_preg).addImm(0)
          .addOperand(Cond[0]).addOperand(Cond[1]); // condition
      }

      // remember this instruction if it has to be the last one
      if (use_preg == AvailPredRegs[loc]) {
        DefUseMI = DefMI;
      }
    } else {
      // The definition location of the predicate is a spill location.
      int fi = PMFI->SinglePathSpillFIs[PMFI->SinglePathSpillSlotOffset
        + (loc/32)];
      unsigned tmpReg = GuardsReg;
      uint32_t or_bitmask = 1 << (loc%32);
      // load from stack slot
      MachineInstr *loadMI = AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::LWC), tmpReg))
        .addFrameIndex(fi).addImm(0); // address
      TRI->eliminateFrameIndex(loadMI, 0, NULL);
      // clear bit on first definition (unconditionally)
      if (R.isFirstDef(&MBB, r)) {
        // R &= ~(1 << loc)
        AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::ANDl), tmpReg))
          .addReg(tmpReg).addImm( ~or_bitmask );
      }
      // FIXME use new instruction for next two steps
      // compute combined predicate (guard && condition)
      AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::PAND), PRTmp))
        .addReg(use_preg).addImm(0) // guard
        .addOperand(Cond[0]).addOperand(Cond[1]); // condition
      // set bit
      // if (guard && cond) R |= (1 << loc)
      unsigned or_opcode = (isUInt<12>(or_bitmask))? Patmos::ORi : Patmos::ORl;
      BuildMI(MBB, MI, DL, TII->get(or_opcode), tmpReg)
        .addReg(PRTmp).addImm(0) // if (guard && cond) == true
        .addReg(tmpReg)
        .addImm( or_bitmask );
      // store back to stack slot
      MachineInstr *storeMI = AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::SWC)))
        .addFrameIndex(fi).addImm(0) // address
        .addReg(tmpReg);
      TRI->eliminateFrameIndex(storeMI, 0, NULL);
    }
  }

  // We have seen a definition of the currently in use predicate,
  // let's move this right to the end
  if (DefUseMI && !(static_cast<MachineInstr*>(prior(MI))==DefUseMI)) {
    MBB.splice(MI, &MBB, DefUseMI);
  }
}


void PatmosSPReduce::applyPredicates(SPNode *N) {
  DEBUG( dbgs() << "Applying predicates to MBBs\n" );

  const RAInfo &R = RAInfos.at(N);

  // for each MBB
  for (SPNode::iterator I=N->begin(), E=N->end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;

    // Subheaders are treated in "their" SPNode.
    // The necessary glue code (copy predicates etc) is done in the linearize
    // walk.
    if (N->isSubHeader(MBB))
      continue;

    unsigned use_preg = getUsePReg(R, MBB);
    assert(use_preg != Patmos::NoRegister || N->isTopLevel());

    // check for use predicate
    if (use_preg==Patmos::NoRegister) {
      DEBUG_TRACE( dbgs() << "  skip: no guard for MBB#" << MBB->getNumber()
                    << "\n" );
      continue;
    }

    DEBUG_TRACE( dbgs() << "  applying "
                        << TRI->getName(use_preg)
                        << " to MBB#" << MBB->getNumber() << "\n" );

    // apply predicate to all instructions in block
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                     ME = MBB->getFirstTerminator();
                                     MI != ME; ++MI) {
      assert(!MI->isBundle() &&
             "PatmosInstrInfo::PredicateInstruction() can't handle bundles");

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

    // insert spill and load instructions for the guard register
    if (R.hasSpillLoad(MBB)) {
      insertUseSpillLoad(R, MBB);
    }
  } // end for each MBB
}


unsigned PatmosSPReduce::getUsePReg(const RAInfo &R,
                                    const MachineBasicBlock *MBB,
                                    bool getP0) {
  int loc = R.getUseLoc(MBB);
  if (loc != -1) {
    assert(loc < (int)AvailPredRegs.size());
    return AvailPredRegs[loc];
  }
  return (getP0) ? Patmos::P0 : Patmos::NoRegister;
}

void PatmosSPReduce::insertUseSpillLoad(const RAInfo &R,
                                        MachineBasicBlock *MBB) {

    int load  = R.getLoadLoc(MBB);
    int spill = R.getSpillLoc(MBB);

    assert( spill!=-1  || load!=-1 );

    // if spill is set, also load must be set
    assert( spill==-1 || load!=-1 );

    unsigned use_preg = getUsePReg(R, MBB);
    assert(use_preg != Patmos::NoRegister);

    MachineBasicBlock::iterator firstMI = MBB->begin();
    DebugLoc DL;

    // insert spill code
    if (spill != -1) {
      int fi = PMFI->SinglePathSpillFIs[PMFI->SinglePathSpillSlotOffset
        + (spill/32)];
      // load from stack slot
      MachineInstr *loadMI = AddDefaultPred(BuildMI(*MBB, firstMI, DL,
            TII->get(Patmos::LWC), GuardsReg))
        .addFrameIndex(fi).addImm(0); // address
      TRI->eliminateFrameIndex(loadMI, 0, NULL);
      // set/clear bit
      //FIXME use new instruction for that
      // if (guard) R |= (1 << spill)
      uint32_t or_bitmask = 1 << (spill%32);
      unsigned or_opcode = (isUInt<12>(or_bitmask))? Patmos::ORi : Patmos::ORl;
      BuildMI(*MBB, firstMI, DL, TII->get(or_opcode), GuardsReg)
        .addReg(use_preg).addImm(0) // if guard == true
        .addReg(GuardsReg)
        .addImm( or_bitmask );
      // if (!guard) R &= ~(1 << spill)
      BuildMI(*MBB, firstMI, DL, TII->get(Patmos::ANDl), GuardsReg)
        .addReg(use_preg).addImm(1) // if guard == false
        .addReg(GuardsReg)
        .addImm( ~or_bitmask );
      // store back to stack slot
      MachineInstr *storeMI = AddDefaultPred(BuildMI(*MBB, firstMI, DL,
            TII->get(Patmos::SWC)))
        .addFrameIndex(fi).addImm(0) // address
        .addReg(GuardsReg);
      TRI->eliminateFrameIndex(storeMI, 0, NULL);
    }

    // insert load code
    if (load != -1) {
      insertPredicateLoad(MBB, firstMI, load, use_preg);
    }
}


void PatmosSPReduce::insertPredicateLoad(MachineBasicBlock *MBB,
                                         MachineBasicBlock::iterator MI,
                                         int loc, unsigned target_preg) {
  assert(loc != -1);
  DebugLoc DL;
  int fi = PMFI->SinglePathSpillFIs[PMFI->SinglePathSpillSlotOffset+(loc/32)];
  // load from stack slot
  MachineInstr *loadMI = AddDefaultPred(BuildMI(*MBB, MI, DL,
        TII->get(Patmos::LWC), GuardsReg))
    .addFrameIndex(fi).addImm(0); // address
  TRI->eliminateFrameIndex(loadMI, 0, NULL);
  // test bit
  // LI $rtr, load
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::LIi),
        Patmos::RTR)).addImm(loc%32);
  // BTEST $Guards, $rtr
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::BTEST),
        target_preg)).addReg(GuardsReg).addReg(Patmos::RTR);
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


void RAInfo::assignLocations(void) {
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
        if ( curUseLoc >= (int)NumColors ) {
          if (hasFreePhys(NumColors)) {
            UL.load = curUseLoc;
            UL.loc = getLoc(); // gets a register
            // reassign
            curUseLoc = UL.loc;
          } else {
            // spill and reassign
            // order predicates wrt furthest next use
            std::vector<unsigned> order;
            for(unsigned j=0; j<LRs.size(); j++) {
              if (curLocs[j] < (int)NumColors) {
                assert(curLocs[j] > -1);
                order.push_back(j);
              }
            }
            std::sort(order.begin(), order.end(),
                FurthestNextUseComparator(*this,i));
            unsigned furthestPred = order.back();
            int stackLoc = getLoc(); // new stack loc
            assert( stackLoc >= (int)NumColors );
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

  dbgs() << "  DefLocs:     ";
  for (unsigned j=0; j<DefLocs.size(); j++) {
    dbgs() << " p" << j << "=" << DefLocs[j];
  }
  dbgs() << "\n";

  dbgs() << "  NumLocs:      " << NumLocs << "\n"
            "  CumLocs:      " << CumLocs << "\n"
            "  Offset:       " << Offset  << "\n"
            "  SpillOffset:  " << SpillOffset  << "\n";
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

  // insert loop preheader to spill predicates / load loop bound
  MachineBasicBlock *PrehdrMBB = MF.CreateMachineBasicBlock();
  MF.push_back(PrehdrMBB);


  const RAInfo &RI = Pass.RAInfos.at(N),
               &RP = Pass.RAInfos.at(N->getParent());

  DebugLoc DL;
  if (RI.needsNodeSpill()) {
    // we create a SBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = Pass.PMFI->SinglePathSpillFIs[N->getDepth()-1];
    unsigned tmpReg = Pass.GuardsReg;
    Pass.TII->copyPhysReg(*PrehdrMBB, PrehdrMBB->end(), DL,
        tmpReg, Patmos::S0, false);
    MachineInstr *storeMI =
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SBC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg);

    Pass.TRI->eliminateFrameIndex(storeMI, 0, NULL);
  }

  // copy the header predicate for the subloop
  const MachineBasicBlock *HeaderMBB = N->getHeader();
  unsigned outer_preg = Pass.getUsePReg(RP, HeaderMBB, true),
           inner_preg = Pass.getUsePReg(RI, HeaderMBB, true);
  assert(inner_preg != Patmos::P0);
  // TODO handle load from stack slot? (once enabled in regalloc)
  if (outer_preg != inner_preg) {
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
          Pass.TII->get(Patmos::PMOV), inner_preg))
      .addReg( outer_preg ).addImm(0);
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

  // insert backwards branch to header at the last block
  // TODO loop iteration counts
  MachineBasicBlock *BranchMBB = MF.CreateMachineBasicBlock();
  MF.push_back(BranchMBB);
  // weave in before inserting the branch (otherwise it'll be removed again)
  nextMBB(BranchMBB);

  // now we can fill the MBB with instructions:


  const RAInfo &RI = Pass.RAInfos.at(N);

  DebugLoc DL;

  // get the header predicate and create a branch to the header
  unsigned header_preg = Pass.getUsePReg(RI, HeaderMBB);
  assert(header_preg != Patmos::NoRegister);
  // get from stack slot, if necessary
  int loadloc = RI.getLoadLoc(HeaderMBB);
  if (loadloc != -1) {
      Pass.insertPredicateLoad(BranchMBB, BranchMBB->end(),
                               loadloc, header_preg);
  }
  // TODO copy between pregs?
  // insert branch
  BuildMI(*BranchMBB, BranchMBB->end(), DL, Pass.TII->get(Patmos::BR))
    .addReg(header_preg).addImm(0)
    .addMBB(HeaderMBB);
  BranchMBB->addSuccessor(HeaderMBB);


  assert(!Pass.hasLiveOutPReg(N) &&
         "Unimplemented: handling of live-out PRegs of loops");

  // create a post-loop MBB to restore the spill predicates, if necessary
  if (RI.needsNodeSpill()) {
    MachineBasicBlock *PostMBB = MF.CreateMachineBasicBlock();
    MF.push_back(PostMBB);
    // we create a LBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = Pass.PMFI->SinglePathSpillFIs[N->getDepth()-1];
    unsigned tmpReg = Pass.GuardsReg;
    MachineInstr *loadMI =
      AddDefaultPred(BuildMI(*PostMBB, PostMBB->end(), DL,
            Pass.TII->get(Patmos::LBC), tmpReg))
      .addFrameIndex(fi).addImm(0); // address

    Pass.TRI->eliminateFrameIndex(loadMI, 0, NULL);
    // assign to S0
    Pass.TII->copyPhysReg(*PostMBB, PostMBB->end(), DL,
                          Patmos::S0, tmpReg, true);
    nextMBB(PostMBB);
  }
}

///////////////////////////////////////////////////////////////////////////////

