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
// It operates on the information regarding SPScopes and (abstract) predicates
// obtained from PatmosSinglePathInfo, in following phases:
// (1) Predicate register allocation is performed with the predicate
//     registers unused in this function, the information is stored in an
//     RAInfo object for every SPScope.
// (2) Code for predicate definitions/spill/load is inserted in MBBs for
//     every SPScope, and instructions of their basic blocks are predicated.
// (3) The CFG is actually "reduced" or linearized, by putting alternatives
//     in sequence. This is done by a walk over the SPScope tree, which also
//     inserts MBBs around loops for predicate spilling/restoring,
//     setting/loading loop bounds, etc.
// (4) MBBs are merged and renumbered, as finalization step.
//
// TODO: It is desirable to have an optimization pass before (4) to
//       remove unnecessary loads/stores to spill slots introduced in (2)-(3).
//
// TODO: No actual loop bounds are loaded, as this information is not
//       available yet
//
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#define USE_BCOPY

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
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
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

STATISTIC( RemovedBranchInstrs, "Number of branch instructions removed");
STATISTIC( InsertedInstrs,      "Number of instructions inserted");

STATISTIC( PredSpillLocs, "Number of required spill bits for predicates");
STATISTIC( NoSpillScopes,
                  "Number of SPScopes (loops) where S0 spill can be omitted");

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
    const PatmosRegisterInfo *TRI;

    // The pointer to the PatmosMachinFunctionInfo is set upon running on a
    // particular function. It contains information about stack slots for
    // predicate spilling and loop bounds.
    const PatmosMachineFunctionInfo *PMFI;

    PatmosSinglePathInfo *PSPI;

    /// doReduceFunction - Reduce a given MachineFunction
    void doReduceFunction(MachineFunction &MF);

    /// computeRegAlloc - Compute RAInfo for each SPScope of the tree
    void computeRegAlloc(SPScope *root);

    /// createRAInfo - Helper function to create a new RAInfo for an SPScope
    /// and insert it in the RAInfos map of the pass.
    /// Returns a reference to the newly created RAInfo.
    RAInfo &createRAInfo(SPScope *S);

    /// getEdgeCondition - Get the predicate operand corresponding
    /// to a edge (predicate operand is true -> edge is taken)
    /// Side effect: branch conditions where the register operand
    /// contained a kill flag are stored in KilledCondRegs.
    void getEdgeCondition(const SPScope::Edge &E,
                          SmallVectorImpl<MachineOperand> &Cond);

    /// insertPredDefinitions - Insert predicate register definitions
    /// to MBBs of the given SPScope.
    void insertPredDefinitions(SPScope *S, MachineFunction &MF);

    /// insertDefEdge - insert instructions for definition of a predicate
    /// by a definition edge.
    /// @param S local scope
    /// @param Node node of local scope that defines a local predicate
    /// @param pred the predicate which is defined
    /// @param e the definition edge (NB if Node is not a subloop, then
    ///          the source of the edge and Node are equal, otherwise the
    ///          the edge is an exit edge of he subloop)
    void insertDefEdge(SPScope *S, MachineBasicBlock &Node,
                       unsigned pred, const SPScope::Edge e);

    /// insertDefToStackLoc - insert a predicate definition to a predicate
    /// which is located on a stack spill location
    /// @param MBB the machine basic block at which end the definition
    ///            should be placed
    /// @param stloc the stack location (index)
    /// @param guard the guard of MBB
    /// @param Cond the condition which should be assigned to the predicate
    /// @param isFirstDef true if the definition is the first definition
    ///                   in the local scope
    void insertDefToStackLoc(MachineBasicBlock &MBB, unsigned stloc,
                             unsigned guard,
                             const SmallVectorImpl<MachineOperand> &Cond,
                             bool isFirstDef);

    /// insertDefToS0SpillSlot - insert a predicate definition to a S0 spill
    /// slot
    /// @param MBB the machine basic block at which end the definition
    ///            should be placed
    /// @param slot the slot number (depth)
    /// @param regloc the reg location (index)
    /// @param guard the guard of MBB
    /// @param Cond the condition which should be assigned to the predicate
    /// @param isFirstDef true if the definition is the first definition
    ///                   in the local scope
    void insertDefToS0SpillSlot(MachineBasicBlock &MBB, unsigned slot,
                    unsigned regloc, unsigned guard,
                    const SmallVectorImpl<MachineOperand> &Cond,
                    bool isFirstDef);

    /// insertDefToRegLoc - insert a predicate definition to a predicate
    /// which is located in a physical register
    /// @param MBB the machine basic block at which end the definition
    ///            should be placed
    /// @param regloc the reg location (index)
    /// @param guard the guard of MBB
    /// @param Cond the condition which should be assigned to the predicate
    /// @param isMultiDef true if the predicate has multiple definitions
    /// @param isFirstDef true if the definition is the first definition
    ///                   in the local scope
    void insertDefToRegLoc(MachineBasicBlock &MBB, unsigned regloc,
                           unsigned guard,
                           const SmallVectorImpl<MachineOperand> &Cond,
                           bool isMultiDef, bool isFirstDef);

    /// moveDefUseGuardInstsToEnd - move instructions, which define a predicate
    /// register that is also their guard to the end of their MBB.
    /// The instructions were collected in insertDefToRegLoc() calls
    /// in the private member DefUseGuardInsts.
    void moveDefUseGuardInstsToEnd(void);

    /// fixupKillFlagOfCondRegs - predicate registers, which are killed at the
    /// branch at the end of the MBB and used in predicate definitions, are
    /// collected in the private member KilledCondRegs.
    /// As the branches are removed, the kill flags need to be hoisted
    /// appropriately.
    void fixupKillFlagOfCondRegs(void);

    /// applyPredicates - Predicate instructions of MBBs in the given SPScope.
    void applyPredicates(SPScope *S, MachineFunction &MF);

    /// insertUseSpillLoad - Insert Spill/Load code at the beginning of the
    /// given MBB, according to R.
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

    /// collectReturnInfoInsts - Collect instructions that store/restore
    /// return information in ReturnInfoInsts
    void collectReturnInfoInsts(MachineFunction &MF);

    /// hasLiveOutPReg - Check if an unavailable PReg must be preserved
    /// in S0 during predicate allocation SPScope on exiting the SPScope
    bool hasLiveOutPReg(const SPScope *S) const;

    /// Map to hold RA infos for each SPScope
    std::map<const SPScope*, RAInfo> RAInfos;

    // Predicate registers un-/used in the function,
    // which are un-/available for allocation here
    std::vector<unsigned> AvailPredRegs;
    std::vector<unsigned> UnavailPredRegs;

    unsigned GuardsReg; // RReg to hold all predicates
    unsigned PRTmp;     // temporary PReg

    // Instructions which define a predicate register that is also their guard.
    // Collected while insertDefToRegLoc(), read and cleared in
    // moveDefUseGuardInsts().
    std::vector<MachineInstr *> DefUseGuardInsts;

    // Branches that set the kill flag on condition operands are remembered,
    // as the branches themselves are removed. The last use of these
    // conditions before the branch will be set the kill flag
    std::map<MachineBasicBlock *, MachineOperand> KilledCondRegs;

    // To preserve the call hierarchy (calls are unconditional in single-path
    // code) instructions that store/restore return information (s7+s8)
    // need to be excluded from predication
    std::set<const MachineInstr *> ReturnInfoInsts;

  public:
    /// PatmosSPReduce - Initialize with PatmosTargetMachine
    PatmosSPReduce(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
      TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
      TRI(static_cast<const PatmosRegisterInfo*>(tm.getRegisterInfo()))
    {
      (void) TM; // silence "unused"-warning
    }

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
      PSPI = &getAnalysis<PatmosSinglePathInfo>();
      PMFI = MF.getInfo<PatmosMachineFunctionInfo>();
      bool changed = false;
      // only convert function if marked
      if ( PSPI->isConverting(MF) ) {
        DEBUG( dbgs() << "[Single-Path] Reducing "
                      << MF.getFunction()->getName() << "\n" );
        doReduceFunction(MF);
        changed |= true;
      }
      return changed;
    }
  };

///////////////////////////////////////////////////////////////////////////////

  /// LinearizeWalker - Class to linearize the CFG during a walk of the SPScope
  /// tree.
  class LinearizeWalker : public SPScopeWalker {
    private:
      virtual void nextMBB(MachineBasicBlock *);
      virtual void enterSubscope(SPScope *);
      virtual void exitSubscope(SPScope *);

      // reference to the pass, to get e.g. RAInfos
      PatmosSPReduce &Pass;
      // reference to the machine function, for inserting MBBs
      MachineFunction &MF;

      MachineBasicBlock *LastMBB; // state: last MBB re-inserted
    public:
      explicit LinearizeWalker(PatmosSPReduce &pass, MachineFunction &mf)
        : Pass(pass), MF(mf), LastMBB(NULL) {}
  };


///////////////////////////////////////////////////////////////////////////////

  /// LiveRange - Class to hold live range information for a predicate in
  /// an RAInfo object.
  class LiveRange {
  friend class RAInfo;
  private:
    uint64_t uses, defs;
    unsigned long len;
    void addUse(long pos) { uses |= (1LL << pos); }
    void addDef(long pos) { defs |= (1LL << pos); }
  public:
    LiveRange(unsigned long length) : uses(0), defs(0), len(length) {
      assert(length <= 64 && "Not yet implemented");
    }
    bool isUse(long pos) const { return uses & (1LL << pos); }
    bool isDef(long pos) const { return defs & (1LL << pos); }
    bool lastUse(long pos) const { return (uses >> (pos+1LL)) == 0; }
    bool hasDefBefore(long pos) const {
      return (defs & ((1LL << pos)-1LL)) != 0;
    }
    bool pastFirstUse(long pos) const {
      return (uses & ((1LL << (pos+1LL))-1LL)) != 0;
    }
    bool hasNextUseBefore(long pos, const LiveRange &other) const {
      return llvm::countTrailingZeros<uint64_t>(uses >> pos)
                < llvm::countTrailingZeros<uint64_t>(other.uses >> pos);
    }
    std::string str(void) const {
      std::stringbuf buf;
      char kind[] = { '-', 'u', 'd', 'x' };
      for (unsigned long i = 0; i < len; i++) {
        int x = 0;
        if (uses & (1LL << i)) x += 1;
        if (defs & (1LL << i)) x += 2;
        buf.sputc(kind[x]);
      }
      return buf.str();
    }
  };

///////////////////////////////////////////////////////////////////////////////

  /// RAInfo - Class to hold register allocation information for a SPScope
  class RAInfo {
    public:
      // the SPScope this RAInfo belongs to
      const SPScope *Scope;
    private:
      // The number of colors (physically available registers)
      const unsigned NumColors;
      // the sequence of MBBs in topological order (ref to Scope->Blocks)
      const std::vector<MachineBasicBlock*> &MBBs;
      // the live ranges of predicates (index = predicate number)
      std::vector<LiveRange> LRs;
      // each predicate has one definition location (index = predicate number)
      std::vector<int> DefLocs; // Pred -> loc
      // set of unused locations, managed during the
      std::set<unsigned> FreeLocs;
      // various attributes regarding locations
      unsigned NumLocs, // Total number of locations in this SPScope
               CumLocs, // Cumulative number of locations synthesized up the
                        //   tree: NumLocs + max. CumLocs over the children
               Offset,  // S0 does not need to be spilled around this scope,
                        //   this is the offset to the available registers
               SpillOffset,   // Starting offset for this scope's spill locations
               LoopCntOffset; // Loop counter stack location offset

      // UseLoc - Record to hold predicate use information for a MBB
      // - loc:   which location to use (a register)
      // - spill: where to spill loc first (spill location)
      // - load:  where to load loc before using it (spill location)
      struct UseLoc {
        int loc, load, spill;
        UseLoc(void) : loc(-1), load(-1), spill(-1) {}
      };
      // Map of MBB -> UseLoc, for an SPScope
      std::map<const MachineBasicBlock*, UseLoc> UseLocs;

      // Comparator for predicates, furthest next use;
      // used in assignLocations()
      struct FurthestNextUseComparator {
        RAInfo &RI;
        int pos;
        bool operator()(int a, int b) {
          return RI.LRs[a].hasNextUseBefore(pos, RI.LRs[b]);
        }
        FurthestNextUseComparator(RAInfo &ri, int p) : RI(ri), pos(p) {}
      };

      // createLiveRanges - Helper function to initially create the live ranges
      // for all predicates used in this SPScope
      void createLiveRanges(void);

      // assignLocations - Performs a linear scan allocation over the MBBs
      // of the SPScope to assign locations
      void assignLocations(void);

      // get an available location
      int getLoc(void) {
        if (!FreeLocs.empty()) {
          std::set<unsigned>::iterator it = FreeLocs.begin();
          FreeLocs.erase(it);
          return *it;
        }
        // create a new location
        return NumLocs++;
      }

      // make a location available again
      void freeLoc(unsigned loc) {
        assert(!FreeLocs.count(loc));
        FreeLocs.insert(loc);
      }
      // returns true if there is a register (color) available atm.
      bool hasFreePhys(void) {
        return (!FreeLocs.empty() && (*FreeLocs.begin() < NumColors) )
          || (NumLocs < NumColors);
      }

    public:
      explicit RAInfo(SPScope *S, unsigned numcolors) :
        Scope(S), NumColors(numcolors), MBBs(S->getBlocks()),
        LRs(S->getNumPredicates(), LiveRange(S->getBlocks().size()+1)),
        DefLocs(S->getNumPredicates(),-1),
        NumLocs(0), CumLocs(0), Offset(0), SpillOffset(0), LoopCntOffset(0) {
          createLiveRanges();
          assignLocations();
        }

      // getCumLocs - Get the number of cumulative locations
      unsigned getCumLocs(void) const { return CumLocs; }

      // setCumLocs - Set the number of cumulative locations, given the
      // maximum CumLocs of the children.
      // Naturally called in a post-order traversal.
      void setCumLocs(unsigned maxFromChildren) {
        CumLocs = NumLocs + maxFromChildren;
      }

      // setLoopCntOffset - Set the offset to access the stack location
      // for the loop counter.
      // Traversal order is not important for this function, depth-first
      // makes sense though.
      void setLoopCntOffset(unsigned num) {
        assert(Scope->hasLoopBound());
        LoopCntOffset = num;
      }

      // getLoopCntOffset - Get the offset to access the stack location
      // for the loop counter.
      unsigned getLoopCntOffset(void) const {
        return LoopCntOffset;
      }

      // computePhysOffset - Compute the offset into the available colors,
      // given the parent RAInfo. Naturally called in a pre-order traversal.
      void computePhysOffset(const RAInfo &ParentRI) {
        // check if we must spill the PRegs
        // Parent.num + S.cum <= size  --> no spill!
        if ( ParentRI.NumLocs + CumLocs <= NumColors ) {
          // compute offset
          Offset = ParentRI.NumLocs + ParentRI.Offset;
        }
      }

      // assignSpillOffset - Stores the offset provided as spill offset,
      // and updates the offset by adding the number of spilled predicates.
      // Traversal order is not important for this function.
      void assignSpillOffset(unsigned &spillOffset) {
        // assign the spill offset, increment
        if (NumLocs > NumColors) {
          this->SpillOffset = spillOffset;
          spillOffset += NumLocs - NumColors;
        }
      }

      // needsScopeSpill - Returns true if S0 must be spilled/restored
      // upon entry/exit of this SPScope.
      bool needsScopeSpill(void) const {
        return (Offset == 0);
      }

      // isFirstDef - Returns true if the given MBB contains the first
      // definition of the given predicate.
      // This has to be false for a header predicate, as the first
      // definition is before the loop is entered
      bool isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const {
        // header predicate
        if (pred == 0) return false;

        for(unsigned i=0; i<MBBs.size(); i++) {
          if (MBBs[i] == MBB) {
            return !LRs[pred].hasDefBefore(i);
          }
        }
        return false;
      }

      // hasSpillLoad - Returns true if the use location of the given MBB
      // requires the predicate register to be first spilled and/or loaded
      // to/from a spill location.
      bool hasSpillLoad(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          const UseLoc &ul = UseLocs.at(MBB);
          return (ul.spill!=-1) || (ul.load!=-1);
        }
        return false;
      }

      /// getUseLoc - Get the use location, which is a register location,
      /// for the given MBB, if any. Returns -1 otherwise.
      int getUseLoc(const MachineBasicBlock *MBB) const {
        if (UseLocs.count(MBB)) {
          int loc = UseLocs.at(MBB).loc + Offset;
          assert( loc < (int)NumColors );
          return loc;
        }
        return -1;
      }

      /// getLoadLoc - Get the load location, which is a spill location,
      /// for the given MBB. Returns -1 if the predicate does not need to be
      /// loaded from a spill slot.
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

      /// getSpillLoc - Get the spill location, i.e. the location where the use
      /// register has to be spilled first, for the given MBB.
      /// Returns -1 if it does not need to be spilled.
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
      /// location is a physical register.
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

      // Dump this RAInfo to dbgs().
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
//  PatmosSPReduce methods
///////////////////////////////////////////////////////////////////////////////


void PatmosSPReduce::doReduceFunction(MachineFunction &MF) {


  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  RAInfos.clear();
  ReturnInfoInsts.clear();

  //DEBUG( dbgs() << "BEFORE Single-Path Reduce\n"; MF.dump() );

  // Get the unused predicate registers
  DEBUG( dbgs() << "Available PRegs:" );
  for (TargetRegisterClass::iterator I=Patmos::PRegsRegClass.begin(),
      E=Patmos::PRegsRegClass.end(); I!=E; ++I ) {
    if (RegInfo.reg_empty(*I) && *I!=Patmos::P0) {
      AvailPredRegs.push_back(*I);
      DEBUG( dbgs() << " " << TRI->getName(*I) );
    } else {
      UnavailPredRegs.push_back(*I);
    }
  }
  DEBUG( dbgs() << "\n" );

  GuardsReg = Patmos::R26;
  // Get a temporary predicate register, which must not be used for allocation
  PRTmp     = AvailPredRegs.back();
  AvailPredRegs.pop_back();

  computeRegAlloc(PSPI->getRootScope());


  // before inserting code, we need to obtain additional instructions that are
  // spared from predication (i.e. need to execute unconditionally)
  // -> instructions that store/restore return information
  // NB: we execute the whole frame setup unconditionally!
  //collectReturnInfoInsts(MF);

  // Okay, now actually insert code, handling scopes in no particular order
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
        I!=E; ++I) {
    SPScope *S = *I;

    DEBUG( dbgs() << "Insert code in [MBB#" << S->getHeader()->getNumber()
                  << "]\n");

    // Predicate the instructions of blocks in S, also inserting spill/load
    // of predicates not in registers.
    applyPredicates(S, MF);

    // Insert predicate definitions.
    insertPredDefinitions(S, MF);
  }

  // After all scopes are handled, perform some global fixups

  // Fixup instructions that define their own guard
  moveDefUseGuardInstsToEnd();

  // Fixup kill flag of condition predicate registers
  fixupKillFlagOfCondRegs();


  //DEBUG(MF.viewCFGOnly());

  // Following walk of the SPScope tree linearizes the CFG structure,
  // inserting MBBs as required (preheader, spill/restore, loop counts, ...)
  DEBUG( dbgs() << "Linearize MBBs\n" );
  LinearizeWalker LW(*this, MF);
  PSPI->walkRoot(LW);

  // Following function merges MBBs in the linearized CFG in order to
  // simplify it
  mergeMBBs(MF);

  // Finally, we assign numbers in ascending order to MBBs again.
  MF.RenumberBlocks();

  //DEBUG(MF.viewCFGOnly());
  //DEBUG( dbgs() << "AFTER Single-Path Reduce\n"; MF.dump() );
}


void PatmosSPReduce::computeRegAlloc(SPScope *root) {
  DEBUG( dbgs() << "RegAlloc\n" );


  // perform reg-allocation in post-order to compute cumulative location
  // numbers in one go
  for (po_iterator<PatmosSinglePathInfo*> I = po_begin(PSPI), E = po_end(PSPI);
      I!=E; ++I) {
    SPScope *S = *I;
    // create RAInfo for SPScope
    RAInfo &RI = createRAInfo(S);

    // Because this is a post-order traversal, we have already visited
    // all children. Synthesize the cumulative number of locations
    unsigned maxChildPreds = 0;
    for(SPScope::child_iterator CI = S->child_begin(), CE = S->child_end();
        CI != CE; ++CI) {
      SPScope *CN = *CI;
      maxChildPreds = std::max(RAInfos.at(CN).getCumLocs(), maxChildPreds);
    }
    RI.setCumLocs(maxChildPreds);

  } // end of PO traversal for RegAlloc


  // Visit all scopes in depth-first order to compute offsets:
  // - Offset is inherited during traversal
  // - SpillOffset is assigned increased depth-first, from left to right
  // - LoopCntOffset as well
  unsigned spillLocCnt   = 0,
           loopCntOffset = 0;
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
        I!=E; ++I) {
    SPScope *S = *I;

    RAInfo &RI = RAInfos.at(S);

    // compute offset for physically available registers
    if (!S->isTopLevel()) {
      RI.computePhysOffset( RAInfos.at(S->getParent()) );
      if (!RI.needsScopeSpill()) NoSpillScopes++; // STATISTIC
    }
    // assign and update spillLocCnt
    RI.assignSpillOffset(spillLocCnt);

    // assign a loop counter offset for the stack location
    if (S->hasLoopBound()) {
      RI.setLoopCntOffset(loopCntOffset++);
    }

    DEBUG( RI.dump() );
  } // end df

  PredSpillLocs += spillLocCnt; // STATISTIC

}



RAInfo& PatmosSPReduce::createRAInfo(SPScope *S) {
  assert(!RAInfos.count(S) && "Already having RAInfo for Scope!");
  // The number of colors for allocation is AvailPredRegs.size()
  RAInfos.insert( std::make_pair(S, RAInfo(S,
                                           AvailPredRegs.size()
                                           )) );
  return RAInfos.at(S);
}


void PatmosSPReduce::getEdgeCondition(const SPScope::Edge &E,
    SmallVectorImpl<MachineOperand> &Cond) {

  MachineBasicBlock *SrcMBB = const_cast<MachineBasicBlock*>(E.first),
                    *DstMBB = const_cast<MachineBasicBlock*>(E.second);

  // get the branch condition
  MachineBasicBlock *TBB = NULL, *FBB = NULL;
  if (TII->AnalyzeBranch(*SrcMBB, TBB, FBB, Cond) || Cond.empty()) {
    report_fatal_error("AnalyzeBranch for SP-Transformation failed; "
        "could not determine branch condition");
  }

  if (TBB != DstMBB) {
    TII->ReverseBranchCondition(Cond);
  }

  if (Cond[0].isKill()) {
    Cond[0].setIsKill(false);
    // remember MBBs which have their final branch condition killed
    if (!KilledCondRegs.count(SrcMBB)) {
      KilledCondRegs.insert(std::make_pair(SrcMBB, Cond[0]));
    }
  }
}


void PatmosSPReduce::insertPredDefinitions(SPScope *S, MachineFunction &MF) {
  DEBUG( dbgs() << " Insert Predicate Definitions\n" );

  // For each MBB, check defs
  for (SPScope::iterator I=S->begin(), E=S->end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;
    const SPScope::PredDefInfo *DI = S->getDefInfo(MBB);

    // procede to next if this does not define any predicates
    if (!DI) continue;

    DEBUG( dbgs() << " - MBB#" << MBB->getNumber() << "\n" );

    // for each definition edge: insert
    for (SPScope::PredDefInfo::iterator di = DI->begin(), de = DI->end();
        di != de; ++di) {
      // Scope, (local) Node, predicate number, edge
      insertDefEdge(S, *MBB, di->first, di->second);
    }

  } // end for each MBB
}





void PatmosSPReduce::insertDefEdge(SPScope *S, MachineBasicBlock &Node,
                                   unsigned pred, const SPScope::Edge e) {

  // if Node is not a subheader, then it must be the source of the edge
  assert(S->isSubHeader(&Node) || (e.first == &Node));

  // the MBB we need to insert the defining instruction is the edge source
  MachineBasicBlock *SrcMBB = const_cast<MachineBasicBlock*>(e.first);

  RAInfo &R = RAInfos.at(S); // local scope of definitions
  // inner scope
  RAInfo &RI = !S->isSubHeader(&Node) ? R
                                      : RAInfos.at(PSPI->getScopeFor(SrcMBB));


  SmallVector<MachineOperand, 2> Cond;
  getEdgeCondition(e, Cond);

  // get the guard register for the source block
  unsigned use_preg = getUsePReg(RI, SrcMBB, true);

  unsigned loc;

  // Get the location for predicate r. The function returns true
  // if it is a register, otherwise false. The resulting location is
  // stored in loc.
  if (R.getDefLoc(loc, pred)) {
    if (!S->isSubHeader(&Node) || (!RI.needsScopeSpill())) {
      // TODO proper condition to avoid writing to the stack slot
      // -> the chain of scopes from outer to inner should not contain any
      // spilling requirements (RAInfo.needsScopeSpill)

      // FIXME assumes direct parent-child relationship, if nested
      assert(!S->isSubHeader(&Node) || (RI.Scope->getParent() == S));

      // The definition location of the predicate is a physical register.
      insertDefToRegLoc(
          *SrcMBB, loc, use_preg, Cond,
          R.Scope->getNumDefEdges(pred) > 1, // isMultiDef
          R.isFirstDef(&Node, pred)           // isFirstDef
          );
    } else {
      // assert(there exists an inner R s.t. R.needsScopeSpill());
      // somewhere on the path from outer to inner scope, S0 is spilled

      // FIXME assumes direct parent-child relationship
      assert(RI.Scope->getParent() == S);
      unsigned slot = RI.Scope->getDepth()-1;

      // set a bit in the appropriate S0 spill slot
      insertDefToS0SpillSlot(
        *SrcMBB, slot, loc, use_preg, Cond,
        R.isFirstDef(&Node, pred)
        );
    }
  } else {
    insertDefToStackLoc(
        *SrcMBB, loc, use_preg, Cond,
        R.isFirstDef(&Node, pred)
        );
  }
}

void PatmosSPReduce::
insertDefToRegLoc(MachineBasicBlock &MBB, unsigned regloc, unsigned guard,
                  const SmallVectorImpl<MachineOperand> &Cond,
                  bool isMultiDef, bool isFirstDef) {

  // insert the predicate definitions before any branch at the MBB end
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL(MI->getDebugLoc());

  MachineInstr *DefMI;
  if (isMultiDef) {
    // if this is the first definition, we unconditionally need to
    // initialize it to false
    // we can skip this, if use_preg=true anyway
    if (isFirstDef && (guard != Patmos::P0)) {
      // the PCLR instruction must not be predicated
      AddDefaultPred(BuildMI(MBB, MI, DL,
            TII->get(Patmos::PCLR), AvailPredRegs[regloc]));
      InsertedInstrs++; // STATISTIC
    }
    DefMI = BuildMI(MBB, MI, DL,
        TII->get(Patmos::PMOV), AvailPredRegs[regloc])
      .addReg(guard).addImm(0) // guard operand
      .addOperand(Cond[0]).addOperand(Cond[1]); // condition
    InsertedInstrs++; // STATISTIC
  } else {
    // the PAND instruction must not be predicated
    DefMI = AddDefaultPred(BuildMI(MBB, MI, DL,
          TII->get(Patmos::PAND), AvailPredRegs[regloc]))
      .addReg(guard).addImm(0) // current guard as source
      .addOperand(Cond[0]).addOperand(Cond[1]); // condition
    InsertedInstrs++; // STATISTIC
  }

  // remember this instruction if it has to be the last one
  if (guard == AvailPredRegs[regloc]) {
    DefUseGuardInsts.push_back(DefMI);
  }
}

void PatmosSPReduce::
insertDefToStackLoc(MachineBasicBlock &MBB, unsigned stloc, unsigned guard,
                    const SmallVectorImpl<MachineOperand> &Cond,
                    bool isFirstDef) {

  // insert the predicate definitions before any branch at the MBB end
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL(MI->getDebugLoc());

  // The definition location of the predicate is a spill location.
  int fi = PMFI->getSinglePathExcessSpillFI(stloc / 32);
  unsigned tmpReg = GuardsReg;
  unsigned bitpos = stloc % 32;

  // load from stack slot
  MachineInstr *loadMI = AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::LWC), tmpReg))
    .addFrameIndex(fi).addImm(0); // address
  TRI->eliminateFrameIndex(loadMI, 0, 3);

#ifdef USE_BCOPY
  // clear bit on first definition (unconditionally)
  if (isFirstDef) {
    // bcopy R, bitpos, !P0
    AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::BCOPY), tmpReg))
      .addReg(tmpReg)
      .addImm(bitpos)
      .addReg(Patmos::P0).addImm(-1);
    InsertedInstrs++; // STATISTIC
  }
  // (guard) bcopy R, bitpos, Cond
  BuildMI(MBB, MI, DL, TII->get(Patmos::BCOPY), tmpReg)
    .addReg(guard).addImm(0) // guard
    .addReg(tmpReg)
    .addImm(bitpos)
    .addOperand(Cond[0]).addOperand(Cond[1]); // condition
#else
  // clear bit on first definition (unconditionally)
  uint32_t or_bitmask = 1 << bitpos;
  if (isFirstDef) {
    // R &= ~(1 << loc)
    AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::ANDl), tmpReg))
      .addReg(tmpReg).addImm( ~or_bitmask );
    InsertedInstrs++; // STATISTIC
  }
  // compute combined predicate (guard && condition)
  AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::PAND), PRTmp))
    .addReg(guard).addImm(0) // guard
    .addOperand(Cond[0]).addOperand(Cond[1]); // condition
  // set bit
  // if (guard && cond) R |= (1 << loc)
  unsigned or_opcode = (isUInt<12>(or_bitmask)) ? Patmos::ORi : Patmos::ORl;
  BuildMI(MBB, MI, DL, TII->get(or_opcode), tmpReg)
    .addReg(PRTmp).addImm(0) // if (guard && cond) == true
    .addReg(tmpReg)
    .addImm(or_bitmask);
#endif
  // store back to stack slot
  MachineInstr *storeMI = AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::SWC)))
    .addFrameIndex(fi).addImm(0) // address
    .addReg(tmpReg);
  TRI->eliminateFrameIndex(storeMI, 0, 2);
  InsertedInstrs += 4; // STATISTIC
}


void PatmosSPReduce::
insertDefToS0SpillSlot(MachineBasicBlock &MBB, unsigned slot, unsigned regloc,
                       unsigned guard,
                       const SmallVectorImpl<MachineOperand> &Cond,
                       bool isFirstDef) {

  // insert the predicate definitions before any branch at the MBB end
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL(MI->getDebugLoc());

  int fi = PMFI->getSinglePathS0SpillFI(slot);
  unsigned tmpReg = GuardsReg;
  int bitpos = TRI->getS0Index(AvailPredRegs[regloc]);
  assert(bitpos > 0);

  // load from stack slot
  MachineInstr *loadMI = AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::LBC), tmpReg))
    .addFrameIndex(fi).addImm(0); // address
  TRI->eliminateFrameIndex(loadMI, 0, 3);
  InsertedInstrs++; // STATISTIC

#ifdef USE_BCOPY
  // clear bit on first definition (unconditionally)
  if (isFirstDef) {
    // bcopy R, bitpos, !P0
    AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::BCOPY), tmpReg))
      .addReg(tmpReg)
      .addImm(bitpos)
      .addReg(Patmos::P0).addImm(-1);
    InsertedInstrs++; // STATISTIC
  }
  // (guard) bcopy R, bitpos, Cond
  BuildMI(MBB, MI, DL, TII->get(Patmos::BCOPY), tmpReg)
    .addReg(guard).addImm(0) // guard
    .addReg(tmpReg)
    .addImm(bitpos)
    .addOperand(Cond[0]).addOperand(Cond[1]); // condition
  InsertedInstrs++; // STATISTIC
#else
  // clear bit on first definition (unconditionally)
  uint32_t or_bitmask = 1 << bitpos;
  if (isFirstDef) {
    // R &= ~(1 << loc)
    AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::ANDl), tmpReg))
      .addReg(tmpReg).addImm( ~or_bitmask );
    InsertedInstrs++; // STATISTIC
  }
  // compute combined predicate (guard && condition)
  AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::PAND), PRTmp))
    .addReg(guard).addImm(0) // guard
    .addOperand(Cond[0]).addOperand(Cond[1]); // condition
  InsertedInstrs++; // STATISTIC
  // set bit
  // if (guard && cond) R |= (1 << loc)
  unsigned or_opcode = (isUInt<12>(or_bitmask)) ? Patmos::ORi : Patmos::ORl;
  assert(or_opcode == Patmos::ORi); // for S0 indexing
  BuildMI(MBB, MI, DL, TII->get(or_opcode), tmpReg)
    .addReg(PRTmp).addImm(0) // if (guard && cond) == true
    .addReg(tmpReg)
    .addImm(or_bitmask);
  InsertedInstrs++; // STATISTIC
#endif
  // store back to stack slot
  MachineInstr *storeMI = AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::SBC)))
    .addFrameIndex(fi).addImm(0) // address
    .addReg(tmpReg);
  TRI->eliminateFrameIndex(storeMI, 0, 2);
  InsertedInstrs++; // STATISTIC
}


void PatmosSPReduce::moveDefUseGuardInstsToEnd(void) {
  DEBUG( dbgs() << " Moving DefUse instrs to MBB end\n" );
  // Move definitions of the currently in use predicate to the end of their MBB
  for (unsigned i = 0; i < DefUseGuardInsts.size(); i++) {
    MachineInstr *DefUseMI = DefUseGuardInsts[i];
    // get containing MBB
    MachineBasicBlock *MBB = DefUseMI->getParent();
    // the first branch at the end of MBB
    MachineBasicBlock::iterator MI = MBB->getFirstTerminator();
    // if it is not the last instruction, make it the last
    if (static_cast<MachineInstr*>(prior(MI)) != DefUseMI) {
      MBB->splice(MI, MBB, DefUseMI);

      DEBUG( dbgs() << "   in MBB#" << MBB->getNumber() << ": ";
             DefUseMI->dump() );
    }
  }
  DefUseGuardInsts.clear();
}


void PatmosSPReduce::fixupKillFlagOfCondRegs(void) {
  DEBUG( dbgs() << " Fixing up kill flags of conditions\n" );

  for (std::map<MachineBasicBlock *, MachineOperand>::iterator
        I = KilledCondRegs.begin(), E = KilledCondRegs.end(); I != E; ++I) {

    MachineBasicBlock *MBB = (*I).first;
    MachineOperand CondReg = (*I).second;

    MachineBasicBlock::iterator firstTI = MBB->getFirstTerminator();

    // restore kill flag at the last use
    // To this end, we search the instruction in which it was last used.
    for (MachineBasicBlock::iterator lastMI = prior(firstTI),
        firstMI = MBB->begin();
        lastMI != firstMI; --lastMI) {
      MachineOperand *MO;
      if ((MO = lastMI->findRegisterUseOperand(CondReg.getReg())) != NULL) {
        MO->setIsKill(true);
        DEBUG( dbgs() << "   in MBB#" << MBB->getNumber() << ": ";
               lastMI->dump() );
        break;
      }
    } // end of search

  } // end for all elements in KilledCondRegs
  KilledCondRegs.clear();
}


void PatmosSPReduce::applyPredicates(SPScope *S, MachineFunction &MF) {
  DEBUG( dbgs() << " Applying predicates to MBBs\n" );

  const RAInfo &R = RAInfos.at(S);

  // for each MBB
  for (SPScope::iterator I=S->begin(), E=S->end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;

    // Subheaders are treated in "their" SPScope.
    // The necessary glue code (copy predicates etc) is done in the linearize
    // walk.
    if (S->isSubHeader(MBB))
      continue;

    unsigned use_preg = getUsePReg(R, MBB, true);


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
      if (TII->isStackControl(MI)) {
          DEBUG_TRACE( dbgs() << "    skip stack control: " << *MI );
          continue;
      }
      if (MI->getFlag(MachineInstr::FrameSetup)) {
          continue;
          DEBUG_TRACE(dbgs() << "    skip frame setup: " << *MI);
      }
      if (ReturnInfoInsts.count(MI)) {
          DEBUG_TRACE(dbgs() << "    skip return info (re-)storing: " << *MI);
          continue;
      }

      if (MI->isCall()) {
          DEBUG_TRACE( dbgs() << "    call: " << *MI );
          assert(!TII->isPredicated(MI) && "call predicated");
          DebugLoc DL = MI->getDebugLoc();
          // copy actual preg to temporary preg
          AddDefaultPred(BuildMI(*MBB, MI, DL,
                TII->get(Patmos::PMOV), PRTmp))
            .addReg(use_preg).addImm(0);

          // store/restore caller saved R9 (gets dirty during frame setup)
          int fi = PMFI->getSinglePathCallSpillFI();
          // store to stack slot
          MachineInstr *storeMI = AddDefaultPred(BuildMI(*MBB, MI, DL,
                TII->get(Patmos::SWC)))
            .addFrameIndex(fi).addImm(0) // address
            .addReg(Patmos::R9);
          TRI->eliminateFrameIndex(storeMI, 0, 2);
          // restore from stack slot (after the call MI)
          MachineInstr *loadMI = AddDefaultPred(BuildMI(*MBB, llvm::next(MI), DL,
                TII->get(Patmos::LWC), Patmos::R9))
            .addFrameIndex(fi).addImm(0); // address
          TRI->eliminateFrameIndex(loadMI, 0, 3);
          ++MI; // skip the load operation
          InsertedInstrs += 3; // STATISTIC
          continue;
      }

      if (MI->isPredicable() && use_preg != Patmos::P0) {
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
          if (!(PO1.getReg() == use_preg && PO2.getImm() == 0)) {
            // build a new predicate := use_preg & old pred
            AddDefaultPred(BuildMI(*MBB, MI, MI->getDebugLoc(),
                                TII->get(Patmos::PAND), PRTmp))
                  .addReg(use_preg).addImm(0)
                  .addOperand(PO1).addOperand(PO2);
            PO1.setReg(PRTmp);
            PO2.setImm(0);
            InsertedInstrs++; // STATISTIC
          }
        }
      }
    } // for each instruction in MBB


    // insert spill and load instructions for the guard register
    if (R.hasSpillLoad(MBB)) {
      insertUseSpillLoad(R, MBB);
    }

    // if this is a reachable function, we need to get the
    // top-level predicate from the caller
    if (S->isTopLevel() && !S->isRootTopLevel() && S->isHeader(MBB)) {
      AddDefaultPred(BuildMI(*MBB, &MBB->front(),
            MBB->front().getDebugLoc(),
            TII->get(Patmos::PMOV), use_preg))
        .addReg(PRTmp).addImm(0);
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
      int fi = PMFI->getSinglePathExcessSpillFI(spill/32);
      // load from stack slot
      MachineInstr *loadMI = AddDefaultPred(BuildMI(*MBB, firstMI, DL,
            TII->get(Patmos::LWC), GuardsReg))
        .addFrameIndex(fi).addImm(0); // address
      TRI->eliminateFrameIndex(loadMI, 0, 3);
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
      TRI->eliminateFrameIndex(storeMI, 0, 2);
      InsertedInstrs += 4; // STATISTIC
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
  int fi = PMFI->getSinglePathExcessSpillFI(loc/32);
  // load from stack slot
  MachineInstr *loadMI = AddDefaultPred(BuildMI(*MBB, MI, DL,
        TII->get(Patmos::LWC), GuardsReg))
    .addFrameIndex(fi).addImm(0); // address
  TRI->eliminateFrameIndex(loadMI, 0, 3);
  // test bit
  // BTESTI $Guards, loc
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::BTESTI),
        target_preg)).addReg(GuardsReg).addImm(loc%32);
  InsertedInstrs += 3; // STATISTIC
}


void PatmosSPReduce::mergeMBBs(MachineFunction &MF) {
  DEBUG( dbgs() << "Merge MBBs\n" );

  // first, obtain the sequence of MBBs in DF order (as copy!)
  // NB: have to use the version below, as some version of libcxx will not
  // compile it (similar to
  //    http://lists.cs.uiuc.edu/pipermail/cfe-commits/Week-of-Mon-20130325/076850.html)
  //std::vector<MachineBasicBlock*> order(df_begin(&MF.front()),
  //                                      df_end(  &MF.front()));
  std::vector<MachineBasicBlock*> order;
  for (df_iterator<MachineBasicBlock *> I = df_begin(&MF.front()),
       E = df_end(&MF.front()); I != E; ++I) {
      order.push_back(*I);
  }


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


void PatmosSPReduce::collectReturnInfoInsts(MachineFunction &MF) {
  DEBUG( dbgs() << "Collect return info insts\n" );

  SmallSet<unsigned, 4> SpecialRegs;
  SpecialRegs.insert(Patmos::SRB);
  SpecialRegs.insert(Patmos::SRO);
  SpecialRegs.insert(Patmos::S0);

  for (MachineFunction::iterator MBB = MF.begin(), MBBe = MF.end();
      MBB != MBBe; ++MBB) {
    for (MachineBasicBlock::iterator MI = MBB->begin(), MIe = MBB->end();
        MI != MIe; ++MI) {

      if (!MI->getFlag(MachineInstr::FrameSetup)) continue;

      if (MI->getOpcode() == Patmos::MFS &&
          SpecialRegs.count(MI->getOperand(3).getReg())) {
        // store return info in prologue (reads SRB/SRO)
        ReturnInfoInsts.insert(MI);
        DEBUG(dbgs() << "   in MBB#" << MBB->getNumber() << ": "; MI->dump());
        // get reg it defines
        unsigned reg = MI->getOperand(0).getReg();
        // search down for first use of reg (store to stack slot)
        MachineBasicBlock::iterator UMI = MI;
        bool found = false;
        while (++UMI != MIe && !found) {
          // if UMI uses reg
          for (unsigned i = 0; i < UMI->getNumOperands(); i++) {
            const MachineOperand &MO = UMI->getOperand(i);
            if ( MO.isReg() && MO.getReg() == reg) {

              assert(UMI->getFlag(MachineInstr::FrameSetup));
              ReturnInfoInsts.insert(UMI);
              DEBUG(dbgs() << "         #" << MBB->getNumber() << ": ";
                  UMI->dump());
              found = true;
              break;
            }
          }
        } // end inner loop
        continue;
      }
      if (MI->getOpcode() == Patmos::MTS &&
          SpecialRegs.count(MI->getOperand(0).getReg())) {
        // restore return info in epilogue (writes SRB/SRO)
        ReturnInfoInsts.insert(MI);
        DEBUG(dbgs() << "   in MBB#" << MBB->getNumber() << ": "; MI->dump());
        // get reg it uses
        unsigned reg = MI->getOperand(3).getReg();
        // search up for def of reg (load from stack slot)
        MachineBasicBlock::iterator DMI = prior(MI);
        bool found = false;
        while (!found) {
          // if DMI defines reg
          if (DMI->definesRegister(reg)) {
            assert(DMI->getFlag(MachineInstr::FrameSetup));
            ReturnInfoInsts.insert(DMI);
            DEBUG(dbgs() << "         #" << MBB->getNumber() << ": ";
                DMI->dump());
            found = true;
            break;
          }
          if (DMI == MBB->begin()) break;
          --DMI;
        } // end inner loop
        continue;
      }
    }

  }

}



bool PatmosSPReduce::hasLiveOutPReg(const SPScope *S) const {
  const std::vector<const MachineBasicBlock *> SuccMBBs = S->getSuccMBBs();
  for (unsigned j=0; j<SuccMBBs.size(); j++) {
    for (unsigned i=0; i<UnavailPredRegs.size(); i++) {
      if (SuccMBBs[j]->isLiveIn(UnavailPredRegs[i])) {
        return true;
      }
    }
  }
  return false;
}





///////////////////////////////////////////////////////////////////////////////
//  RAInfo methods
///////////////////////////////////////////////////////////////////////////////

void RAInfo::createLiveRanges(void) {
  // create live range infomation for each predicate
  DEBUG(dbgs() << " Create live-ranges for [MBB#"
               << Scope->getHeader()->getNumber() << "]\n");


  for (unsigned i=0, e=MBBs.size(); i<e; i++) {
    MachineBasicBlock *MBB = MBBs[i];
    // insert use
    LRs[Scope->getPredUse(MBB)].addUse(i);
    // insert defs
    const SPScope::PredDefInfo *DI = Scope->getDefInfo(MBB);
    if (DI) {
      for (SPScope::PredDefInfo::iterator pi = DI->begin(), pe = DI->end();
          pi != pe; ++pi) {
        LRs[pi->first].addDef(i);
      }
    }
  }
  // add a use for header predicate
  if (!Scope->isTopLevel()) {
    LRs[0].addUse(MBBs.size());
  }
}


void RAInfo::assignLocations(void) {
  DEBUG(dbgs() << " Assign locations for [MBB#"
               << Scope->getHeader()->getNumber() << "]\n");

  // vector to keep track of locations during the scan
  std::vector<int> curLocs(Scope->getNumPredicates(),-1);

  for (unsigned i=0, e=MBBs.size(); i<e; i++) {
    MachineBasicBlock *MBB = MBBs[i];

    DEBUG( dbgs() << "  MBB#" << MBB->getNumber() << ": " );

    // (1) handle use
    unsigned usePred = Scope->getPredUse(MBB);
    // for the top-level entry of a single-path root,
    // we don't need to assign a location, as we will use p0
    if (!(usePred==0 && Scope->isRootTopLevel())) {
      UseLoc UL;

      int &curUseLoc = curLocs[usePred];
      DEBUG( dbgs() << "use " << usePred << " in loc " << curUseLoc << ", ");

      assert(MBB == Scope->getHeader() || i>0);

      if (MBB != Scope->getHeader()) {
        // each use must be preceded by a location assignment
        assert(curUseLoc >= 0);
        // if previous location was not a register, we have to allocate
        // a register and/or possibly spill
        if ( curUseLoc >= (int)NumColors ) {
          if (hasFreePhys()) {
            UL.load = curUseLoc;
            UL.loc = getLoc(); // gets a register
            // reassign
            curUseLoc = UL.loc;
          } else {
            // spill and reassign
            // order predicates wrt furthest next use
            std::vector<unsigned> order;
            for(unsigned j=0; j<LRs.size(); j++) {
              // consider all physical registers (< NumColors) in use (!= -1)
              if (curLocs[j] != -1 && curLocs[j] < (int)NumColors) {
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

      DEBUG( dbgs() << "new " << curUseLoc << ". ");

      // (2) retire locations
      if (LRs[usePred].lastUse(i)) {
        freeLoc( curUseLoc );
        curUseLoc = -1;
        DEBUG(dbgs() << "retire. ");
      }

      // store info
      UseLocs[MBB] = UL;
    }

    // (3) handle definitions in this basic block.
    //     if we need to get new locations for predicates (loc==-1),
    //     assign new ones in nearest-next-use order
    const SPScope::PredDefInfo *DI = Scope->getDefInfo(MBB);
    if (DI) {
      std::vector<unsigned> order;
      for (SPScope::PredDefInfo::iterator pi = DI->begin(), pe = DI->end();
          pi != pe; ++pi) {
        int r = pi->first;
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
        assert(curLocs[pred] == DefLocs[pred]);
        DEBUG( dbgs() << "def " << pred << " in loc "
                      << DefLocs[pred] << ", ");
      }
    }

    DEBUG(dbgs() << "\n");
  } // end of forall MBB

  // What is the location of the header predicate after handling all blocks?
  // We store this location, as it is where the next iteration has to get it
  // from (if different from its use location)
  if (!Scope->isTopLevel()) {
    UseLoc &ul = UseLocs[Scope->getHeader()];
    if (ul.loc != curLocs[0]) {
      ul.load = curLocs[0];
      // FIXME need to handle this in the back-branch block
      // case distinction for stack/reg loc?
      assert(0);
    }
  }
}


void RAInfo::dump() const {
  dbgs() << "[MBB#"     << Scope->getHeader()->getNumber()
         <<  "] depth=" << Scope->getDepth() << "\n";

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
  // (update statistic counter)
  RemovedBranchInstrs += Pass.TII->RemoveBranch(*MBB);

  if (LastMBB) {
    // add to the last MBB as successor
    LastMBB->addSuccessor(MBB);
    // move in the code layout
    MBB->moveAfter(LastMBB);
  }
  // keep track of tail
  LastMBB = MBB;
}


void LinearizeWalker::enterSubscope(SPScope *S) {

  // We don't create a preheader for entry.
  if (S->isTopLevel()) return;

  // insert loop preheader to spill predicates / load loop bound
  MachineBasicBlock *PrehdrMBB = MF.CreateMachineBasicBlock();
  MF.push_back(PrehdrMBB);


  const RAInfo &RI = Pass.RAInfos.at(S),
               &RP = Pass.RAInfos.at(S->getParent());

  DebugLoc DL;
  if (RI.needsScopeSpill()) {
    // we create a SBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = Pass.PMFI->getSinglePathS0SpillFI(S->getDepth()-1);
    unsigned tmpReg = Pass.GuardsReg;
    Pass.TII->copyPhysReg(*PrehdrMBB, PrehdrMBB->end(), DL,
        tmpReg, Patmos::S0, false);
    MachineInstr *storeMI =
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SBC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg);

    Pass.TRI->eliminateFrameIndex(storeMI, 0, 2);
    InsertedInstrs += 2; // STATISTIC
  }

  // copy the header predicate for the subloop
  const MachineBasicBlock *HeaderMBB = S->getHeader();
  unsigned outer_preg = Pass.getUsePReg(RP, HeaderMBB, true),
           inner_preg = Pass.getUsePReg(RI, HeaderMBB, true);
  assert(inner_preg != Patmos::P0);
  // TODO handle load from stack slot? (once enabled in regalloc)
  if (outer_preg != inner_preg) {
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
          Pass.TII->get(Patmos::PMOV), inner_preg))
      .addReg( outer_preg ).addImm(0);
    InsertedInstrs++; // STATISTIC
  }

  // Initialize the loop bound and store it to the stack slot
  if (S->hasLoopBound()) {
    unsigned tmpReg = Pass.GuardsReg;
    int loop = S->getLoopBound();
    // Create an instruction to load the loop bound
    // TODO try to find an unused register
    // FIXME load or copy the actual loop bound
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
          Pass.TII->get( (isUInt<12>(loop)) ? Patmos::LIi : Patmos::LIl),
          tmpReg))
      .addImm(loop); // the loop bound

    int fi = Pass.PMFI->getSinglePathLoopCntFI(RI.getLoopCntOffset());
    MachineInstr *storeMI =
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SWC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg, RegState::Kill);
    Pass.TRI->eliminateFrameIndex(storeMI, 0, 2);
    InsertedInstrs += 2; // STATISTIC
  }

  // append the preheader
  nextMBB(PrehdrMBB);
}


void LinearizeWalker::exitSubscope(SPScope *S) {

  MachineBasicBlock *HeaderMBB = S->getHeader();
  DEBUG_TRACE( dbgs() << "ScopeRange [MBB#" <<  HeaderMBB->getNumber()
                <<  ", MBB#" <<  LastMBB->getNumber() << "]\n" );

  if (S->isTopLevel()) return;

  const RAInfo &RI = Pass.RAInfos.at(S);
  DebugLoc DL;

  // insert backwards branch to header at the last block
  MachineBasicBlock *BranchMBB = MF.CreateMachineBasicBlock();
  MF.push_back(BranchMBB);
  // weave in before inserting the branch (otherwise it'll be removed again)
  nextMBB(BranchMBB);

  // now we can fill the MBB with instructions:
  // load the branch predicate
  unsigned branch_preg = Patmos::NoRegister;
  if (S->hasLoopBound()) {
    // load the loop counter, decrement it by one, and if it is not (yet)
    // zero, we enter the loop again.
    // TODO is the loop counter in a register?!
    int fi = Pass.PMFI->getSinglePathLoopCntFI(RI.getLoopCntOffset());
    unsigned tmpReg = Pass.GuardsReg;
    MachineInstr *loadMI =
      AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::LWC), tmpReg))
      .addFrameIndex(fi).addImm(0); // address
    Pass.TRI->eliminateFrameIndex(loadMI, 0, 3);

    // decrement
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::SUBi), tmpReg))
      .addReg(tmpReg).addImm(1);
    // compare with 0, PRTmp as predicate register
    branch_preg = Pass.PRTmp;
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::CMPNEQ), branch_preg))
      .addReg(Patmos::R0).addReg(tmpReg);
    // store back
    MachineInstr *storeMI =
      AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::SWC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg, RegState::Kill);
    Pass.TRI->eliminateFrameIndex(storeMI, 0, 2);
    InsertedInstrs += 4; // STATISTIC
  } else {
    // get the header predicate
    branch_preg = Pass.getUsePReg(RI, HeaderMBB);
    // get from stack slot, if necessary
    int loadloc = RI.getLoadLoc(HeaderMBB);
    if (loadloc != -1) {
      Pass.insertPredicateLoad(BranchMBB, BranchMBB->end(),
          loadloc, branch_preg);
    }
    // TODO copy between pregs?
  }
  // insert branch to header
  assert(branch_preg != Patmos::NoRegister);
  BuildMI(*BranchMBB, BranchMBB->end(), DL, Pass.TII->get(Patmos::BR))
    .addReg(branch_preg).addImm(0)
    .addMBB(HeaderMBB);
  BranchMBB->addSuccessor(HeaderMBB);
  InsertedInstrs++; // STATISTIC


  assert(!Pass.hasLiveOutPReg(S) &&
         "Unimplemented: handling of live-out PRegs of loops");


  // create a post-loop MBB to restore the spill predicates, if necessary
  if (RI.needsScopeSpill()) {
    MachineBasicBlock *PostMBB = MF.CreateMachineBasicBlock();
    MF.push_back(PostMBB);
    // we create a LBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = Pass.PMFI->getSinglePathS0SpillFI(S->getDepth()-1);
    unsigned tmpReg = Pass.GuardsReg;
    MachineInstr *loadMI =
      AddDefaultPred(BuildMI(*PostMBB, PostMBB->end(), DL,
            Pass.TII->get(Patmos::LBC), tmpReg))
      .addFrameIndex(fi).addImm(0); // address

    Pass.TRI->eliminateFrameIndex(loadMI, 0, 3);
    // assign to S0
    Pass.TII->copyPhysReg(*PostMBB, PostMBB->end(), DL,
                          Patmos::S0, tmpReg, true);
    nextMBB(PostMBB);
    InsertedInstrs += 2; // STATISTIC
  }
}

///////////////////////////////////////////////////////////////////////////////

