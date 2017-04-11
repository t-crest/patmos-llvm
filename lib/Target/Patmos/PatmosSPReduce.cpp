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
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#define USE_BCOPY
#define NOSPILL_OPTIMIZATION
//#define BOUND_UNDEREST_PROTECTION

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
#include <queue>
#include <algorithm>
#include <sstream>
#include <iostream>


using namespace llvm;

STATISTIC( RemovedBranchInstrs, "Number of branch instructions removed");
STATISTIC( InsertedInstrs,      "Number of instructions inserted");
STATISTIC( LoopCounters,        "Number of loop counters introduced");
STATISTIC( ElimLdStCnt,         "Number of eliminated redundant loads/stores");

STATISTIC( PredSpillLocs, "Number of required spill bits for predicates");
STATISTIC( NoSpillScopes,
                  "Number of SPScopes (loops) where S0 spill can be omitted");
STATISTIC( SPNumPredicates, "Number of predicates for single-path code");

// anonymous namespace
namespace {

  class LinearizeWalker;
  class RAInfo;
  class RedundantLdStEliminator;

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

    /// insertStackLocInitializations - Insert predicate initializations
    /// for predicates located on the stack.
    void insertStackLocInitializations(SPScope *S);

    /// insertPredDefinitions - Insert predicate register definitions
    /// to MBBs of the given SPScope.
    void insertPredDefinitions(SPScope *S);

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
    void insertDefToStackLoc(MachineBasicBlock &MBB, unsigned stloc,
                             unsigned guard,
                             const SmallVectorImpl<MachineOperand> &Cond);

    /// insertDefToS0SpillSlot - insert a predicate definition to a S0 spill
    /// slot
    /// @param MBB the machine basic block at which end the definition
    ///            should be placed
    /// @param slot the slot number (depth)
    /// @param regloc the reg location (index)
    /// @param guard the guard of MBB
    /// @param Cond the condition which should be assigned to the predicate
    void insertDefToS0SpillSlot(MachineBasicBlock &MBB, unsigned slot,
                    unsigned regloc, unsigned guard,
                    const SmallVectorImpl<MachineOperand> &Cond);

    /// insertDefToRegLoc - insert a predicate definition to a predicate
    /// which is located in a physical register
    /// @param MBB the machine basic block at which end the definition
    ///            should be placed
    /// @param regloc the reg location (index)
    /// @param guard the guard of MBB
    /// @param Cond the condition which should be assigned to the predicate
    /// @param isMultiDef    true if the predicate has multiple definitions
    /// @param isFirstDef    true if the definition is the first definition
    ///                      in the local scope
    /// @param isExitEdgeDef true if the definition is on an exit edge of a
    ///                      subloop
    void insertDefToRegLoc(MachineBasicBlock &MBB, unsigned regloc,
                           unsigned guard,
                           const SmallVectorImpl<MachineOperand> &Cond,
                           bool isMultiDef, bool isFirstDef,
                           bool isExitEdgeDef);

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

    /// getStackLocPair - Return frame index and bit position within,
    /// given by a stack location
    void getStackLocPair(int &fi, unsigned &bitpos,
                         const unsigned stloc) const;

    /// mergeMBBs - Merge the linear sequence of MBBs as possible
    void mergeMBBs(MachineFunction &MF);

    /// collectReturnInfoInsts - Collect instructions that store/restore
    /// return information in ReturnInfoInsts
    void collectReturnInfoInsts(MachineFunction &MF);

    /// eliminateFrameIndices - Batch call TRI->eliminateFrameIndex() on the
    /// collected stack store and load indices
    void eliminateFrameIndices(MachineFunction &MF);

    /// getLoopLiveOutPRegs - Collect unavailable PRegs that must be preserved
    /// in S0 during predicate allocation SPScope on exiting the SPScope
    /// because it lives in into a loop successor
    void getLoopLiveOutPRegs(const SPScope *S,
                             std::vector<unsigned> &pregs) const;

    /// Map to hold RA infos for each SPScope
    std::map<const SPScope*, RAInfo> RAInfos;

    // Predicate registers un-/used in the function,
    // which are un-/available for allocation here
    std::vector<unsigned> AvailPredRegs;
    std::vector<unsigned> UnavailPredRegs;

    unsigned GuardsReg; // RReg to hold all predicates
    unsigned PRTmp;     // temporary PReg

    // At each doReduce on a function, an instance of the
    // RedundantLdStEliminator is created
    RedundantLdStEliminator *GuardsLdStElim;

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
    BitVector uses, defs;
    void addUse(long pos) { uses.set(pos); }
    void addDef(long pos) { defs.set(pos); }
  public:
    LiveRange(unsigned long length)
      : uses(length), defs(length) {}
    bool isUse(long pos) const { return uses.test(pos); }
    bool isDef(long pos) const { return defs.test(pos); }
    bool lastUse(long pos) const {
      // test whether shifting out up to this use will result in an empty
      // bitvector
      // return (uses >> (pos+1LL)) == 0;
      for (unsigned i = pos+1; i < uses.size(); i++) {
        if (uses.test(i)) return false;
      }
      return true;
    }
    bool hasDefBefore(long pos) const {
      // 00000100000 pos
      // 00000011111 before
      // -> any common?
      //return (defs & ((1LL << pos)-1LL)) != 0;
      unsigned i = pos;
      while (i-- > 0) {
        if (defs.test(i)) return true;
      }
      return false;
    }
    bool pastFirstUse(long pos) const {
      // check if there is any use before (and including) pos
      //return (uses & ((1LL << (pos+1LL))-1LL)) != 0;
      for (unsigned i = 0; i <= pos; i++) {
        if (uses.test(i)) return true;
      }
      return false;
    }
    bool hasNextUseBefore(long pos, const LiveRange &other) const {
      assert(uses.size() == other.uses.size());
      // this   ....10000|...
      // other ......1000|...   -> no
      //                ^pos
      for (unsigned i = pos; i < uses.size(); i++) {
        if (other.uses.test(i)) break;
        if (uses.test(i)) return true;
      }
      return false;
    }
    std::string str(void) const {
      std::stringbuf buf;
      char kind[] = { '-', 'u', 'd', 'x' };
      for (unsigned long i = 0; i < uses.size(); i++) {
        int x = 0;
        if (uses.test(i)) x += 1;
        if (defs.test(i)) x += 2;
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
               Offset,  // If S0 does not need to be spilled around this scope,
                        //   this is the offset to the available registers
               SpillOffset; // Starting offset for this scope's spill locations

      bool NeedsScopeSpill;

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
        NumLocs(0), CumLocs(0), Offset(0), SpillOffset(0),
        NeedsScopeSpill(true) {
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

      // computePhysOffset - Compute the offset into the available colors,
      // given the parent RAInfo. Naturally called in a pre-order traversal.
      void computePhysOffset(const RAInfo &ParentRI) {
        // check if we must spill the PRegs
        // Parent.num + S.cum <= size  --> no spill!
        if ( ParentRI.NumLocs + CumLocs <= NumColors ) {
          // compute offset
          Offset = ParentRI.NumLocs + ParentRI.Offset;
          NeedsScopeSpill = false;
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
        return NeedsScopeSpill;
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

///////////////////////////////////////////////////////////////////////////////

  /// RedundantLdStEliminator - Class that implements the removal
  /// of redundant loads and stores (to a tracked register), which are
  /// inserted in the course of the transformation.
  /// This includes predicate spill code and loop counters.
  class RedundantLdStEliminator {
    public:
      explicit RedundantLdStEliminator(MachineFunction &mf,
          const PatmosRegisterInfo *tri, unsigned int tgtreg,
          const PatmosMachineFunctionInfo &PMFI)
        : MF(mf), TRI(tri), TgtReg(tgtreg), NumFIs(PMFI.getSinglePathFICnt()),
          OffsetFIs(PMFI.getSinglePathLoopCntFI(0)) {}

      void addRemovableInst(MachineInstr *MI) {
        Removables.insert(MI);
      }


      unsigned int process(void) {
        DEBUG( dbgs() << "Eliminate redundant loads/stores to " <<
            TRI->getName(TgtReg) << "\n" );

        unsigned int count = 0;
        // create the container with the bitvectors for each basic block
        // for the data-flow analyses
        for (MachineFunction::iterator MBB = MF.begin(), MBBe = MF.end();
            MBB != MBBe; ++MBB) {
          BlockInfos.insert(std::make_pair(MBB, Blockinfo(NumFIs)));
        }

        DEBUG(dbgs() << "Removing redundant loads:\n");
        findRedundantLoads();
        count += remove();

        // Having redundant loads eliminated enables simpler removal
        // of redundant stores
        DEBUG(dbgs() << "Removing redundant stores:\n");
        // FIXME the analysis is erroneous.
        //findRedundantStores();
        count += remove();

        return count;
      }

      unsigned int remove(void) {
        unsigned int cnt = Removables.size();
        for (std::set<MachineInstr *>::iterator I = Removables.begin(),
            E = Removables.end(); I != E; ++I) {
          DEBUG(dbgs() << "  " << **I);
          (*I)->eraseFromParent();
        }
        Removables.clear();
        return cnt;
      }

    private:
      MachineFunction &MF;
      const PatmosRegisterInfo *TRI;
      const unsigned int TgtReg;
      const unsigned int NumFIs;
      const unsigned int OffsetFIs;

      std::set<MachineInstr *> Removables;

      struct Blockinfo {
        // required for elimination of redundant loads
        BitVector LiveFIExit, LiveFIEntry;
        // required for elimination of redundant stores
        BitVector SubseqStoresEntry, SubseqStoresExit;
        BitVector FutureLoadsEntry, FutureLoadsExit;

        Blockinfo(unsigned int size)
          : LiveFIExit(size), LiveFIEntry(size),
            SubseqStoresEntry(size), SubseqStoresExit(size),
            FutureLoadsEntry(size), FutureLoadsExit(size) {}
      };

      std::map<const MachineBasicBlock *, Blockinfo> BlockInfos;

      inline unsigned int normalizeFI(int fi) const {
        unsigned norm = fi - OffsetFIs;
        assert((fi >= 0 && norm < NumFIs) && "FI out of bounds");
        return norm;
      }

      inline int denormalizeFI(unsigned int fi) const {
        assert(fi < NumFIs && "FI out of bounds");
        return fi + OffsetFIs;
      }

      void printFISet(const BitVector &BV, raw_ostream &os) const {
        for (int i = BV.find_first(); i != -1; i = BV.find_next(i)) {
          os << denormalizeFI(i) << " ";
        }
      }

      bool isUncondLoad(const MachineInstr *MI, int &fi) const {
        if ((MI->getOpcode() == Patmos::LBC || MI->getOpcode() == Patmos::LWC)
            && MI->getOperand(0).getReg() == TgtReg
            && (MI->getOperand(1).getReg() == Patmos::NoRegister ||
              MI->getOperand(1).getReg() == Patmos::P0)
            && MI->getOperand(2).getImm() == 0
            && MI->getOperand(3).isFI()) {
          fi = MI->getOperand(3).getIndex();
          return true;
        }
        return false;
      }

      bool isUncondStore(const MachineInstr *MI, int &fi) const {
        if ((MI->getOpcode() == Patmos::SBC || MI->getOpcode() == Patmos::SWC)
            && MI->getOperand(4).getReg() == TgtReg
            && (MI->getOperand(0).getReg() == Patmos::NoRegister ||
              MI->getOperand(0).getReg() == Patmos::P0)
            && MI->getOperand(1).getImm() == 0
            && MI->getOperand(2).isFI()) {
          fi = MI->getOperand(2).getIndex();
          return true;
        }
        return false;
      }


      void findRedundantLoads(void) {
        // forward DF problem
        std::map<MachineInstr *, BitVector> collected_loads;
        // operate in reverse-postorder
        ReversePostOrderTraversal<MachineFunction *> RPOT(&MF);
        bool changed;
        do {
          changed = false;
          for (ReversePostOrderTraversal<MachineFunction *>::rpo_iterator
              RI = RPOT.begin(),  RE = RPOT.end(); RI != RE; ++RI) {
            MachineBasicBlock *MBB = *RI;
            Blockinfo &BI = this->BlockInfos.at(MBB);

            BitVector livein = BitVector(NumFIs, true);
            // join from predecessors
            if (MBB->pred_size() > 0) {
              for (MachineBasicBlock::pred_iterator PI = MBB->pred_begin();
                  PI != MBB->pred_end(); ++PI) {
                 livein &= this->BlockInfos.at(*PI).LiveFIExit;
              }
            } else {
              livein.reset();
            }
            if (BI.LiveFIEntry != livein) {
              BI.LiveFIEntry = livein;
              changed = true;
            }

            // transfer
            BitVector livefi(livein);
            for (MachineBasicBlock::iterator MI = MBB->begin(),
                MIe = MBB->end(); MI != MIe; ++MI) {
              // check for unconditional load to TgtReg
              int fi;
              if (isUncondLoad(&*MI, fi)) {
                // remember load with livefi at entry
                collected_loads[&*MI] = livefi;
                // update
                livefi.reset();
                livefi.set(normalizeFI(fi));
              }
            }
            // was an update?
            if (BI.LiveFIExit != livefi) {
              BI.LiveFIExit = livefi;
              changed = true;
            }
          }
        } while (changed);

        // now inspect the livefi at entry of each load. if it is equal to
        // the fi of the load, the load is redundant and we can remove it.
        for (std::map<MachineInstr *, BitVector>::iterator
            I = collected_loads.begin(), E = collected_loads.end();
            I != E; ++I) {
          int fi;
          (void) isUncondLoad(I->first, fi);
          if (I->second.test(normalizeFI(fi))) {
            Removables.insert(I->first);
          }
        }
      }

      void findRedundantStores(void) {
        // backward DF problems
        std::map<MachineInstr *,
                 std::pair<BitVector, BitVector> > collected_stores;
        std::queue<MachineBasicBlock *> worklist;

        // fill worklist initially in dfs postorder
        for (po_iterator<MachineBasicBlock *> POI = po_begin(&MF.front()),
            POIe = po_end(&MF.front());  POI != POIe; ++POI) {
          worklist.push(*POI);
        }

        // iterate.
        while (!worklist.empty()) {
          // pop first element
          MachineBasicBlock *MBB = worklist.front();
          worklist.pop();
          Blockinfo &BI = this->BlockInfos.at(MBB);

          BitVector subseqstores(NumFIs, true);
          BitVector futureloads(NumFIs);
          if (MBB->succ_size() > 0) {
            for (MachineBasicBlock::succ_iterator SI = MBB->succ_begin();
                SI != MBB->succ_end(); ++SI) {
              Blockinfo &BISucc = this->BlockInfos.at(*SI);
              futureloads  |= BISucc.FutureLoadsEntry;
              subseqstores &= BISucc.SubseqStoresEntry;
            }
          } else {
            subseqstores.reset();
          }
          BI.FutureLoadsExit = futureloads;
          BI.SubseqStoresExit = subseqstores;

          // transfer
          for (MachineBasicBlock::reverse_iterator MI = MBB->rbegin(),
              MIe = MBB->rend(); MI != MIe; ++MI) {
            int fi;
            if (isUncondLoad(&*MI, fi)) {
              int nfi = normalizeFI(fi);
              futureloads.set(nfi);
              if (!subseqstores.test(nfi)) subseqstores.reset();
              continue;
            }
            if (isUncondStore(&*MI, fi)) {
              // remember store-inst with futureloads/subseq-st at exit
              collected_stores[&*MI] = std::make_pair(futureloads,
                                                      subseqstores);
              // update
              subseqstores.reset();
              subseqstores.set(normalizeFI(fi));
              continue;
            }
          }

          // was an update?
          if (BI.FutureLoadsEntry  != futureloads ||
              BI.SubseqStoresEntry != subseqstores) {
            BI.FutureLoadsEntry  = futureloads;
            BI.SubseqStoresEntry = subseqstores;
            // add predecessors to worklist
            for (MachineBasicBlock::pred_iterator PI=MBB->pred_begin();
                PI!=MBB->pred_end(); ++PI) {
              worklist.push(*PI);
            }
          }
        }

        // Now iterate through the collected store instructions.
        // If the fi of a store is covered by a subsequent store, or the
        // fi is never loaded again in the future, the store can be removed.
        for (std::map<MachineInstr *,
                      std::pair<BitVector, BitVector> >::iterator
             I = collected_stores.begin(), E = collected_stores.end();
             I != E; ++I) {
          int fi, nfi;
          (void) isUncondStore(I->first, fi);
          nfi = normalizeFI(fi);
          BitVector futureloads  = I->second.first;
          BitVector subseqstores = I->second.second;
          if (subseqstores.test(nfi) || !futureloads.test(nfi)) {
            Removables.insert(I->first);
          }
        }
      }

  };





///////////////////////////////////////////////////////////////////////////////
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

  DEBUG( dbgs() << "BEFORE Single-Path Reduce\n"; MF.dump() );

  MachineRegisterInfo &RegInfo = MF.getRegInfo();

  AvailPredRegs.clear();
  UnavailPredRegs.clear();
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

  // Guard the instructions (no particular order necessary)
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
        I != E; ++I) {
    applyPredicates(*I, MF);
  }
  // Insert predicate definitions (no particular order necessary)
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
        I != E; ++I) {
    insertPredDefinitions(*I);
    insertStackLocInitializations(*I);
  }

  // After all scopes are handled, perform some global fixups

  // Fixup instructions that define their own guard
  moveDefUseGuardInstsToEnd();

  // Fixup kill flag of condition predicate registers
  fixupKillFlagOfCondRegs();

  //DEBUG(MF.viewCFGOnly());

  // we create an instance of the eliminator here, such that we can
  // insert dummy instructions for analysis and mark them as 'to be removed'
  // with the eliminator
  GuardsLdStElim = new RedundantLdStEliminator(MF, TRI, GuardsReg, *PMFI);

  // Following walk of the SPScope tree linearizes the CFG structure,
  // inserting MBBs as required (preheader, spill/restore, loop counts, ...)
  DEBUG( dbgs() << "Linearize MBBs\n" );
  LinearizeWalker LW(*this, MF);
  PSPI->walkRoot(LW);

  // Following function merges MBBs in the linearized CFG in order to
  // simplify it
  mergeMBBs(MF);

  // Perform the elimination of LD/ST on the large basic blocks
  ElimLdStCnt += GuardsLdStElim->process();
  delete GuardsLdStElim;


  // Remove frame index operands from inserted loads and stores to stack
  eliminateFrameIndices(MF);

  // Finally, we assign numbers in ascending order to MBBs again.
  MF.RenumberBlocks();

  //DEBUG(MF.viewCFGOnly());
  DEBUG( dbgs() << "AFTER Single-Path Reduce\n"; MF.dump() );
}


void PatmosSPReduce::computeRegAlloc(SPScope *root) {
  DEBUG( dbgs() << "RegAlloc\n" );

  RAInfos.clear();

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
  unsigned spillLocCnt   = 0;
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
        I!=E; ++I) {
    SPScope *S = *I;

    RAInfo &RI = RAInfos.at(S);

    // compute offset for physically available registers
    if (!S->isTopLevel()) {
#ifdef NOSPILL_OPTIMIZATION
      RI.computePhysOffset( RAInfos.at(S->getParent()) );
#endif
      if (!RI.needsScopeSpill()) NoSpillScopes++; // STATISTIC
    }
    // assign and update spillLocCnt
    RI.assignSpillOffset(spillLocCnt);

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

void PatmosSPReduce::insertStackLocInitializations(SPScope *S) {
  DEBUG( dbgs() << " Insert StackLoc Initializations in [MBB#"
                << S->getHeader()->getNumber() << "]\n");

  // register allocation information
  RAInfo &R = RAInfos.at(S);

  // Create the masks
  std::map<int, uint32_t> masks;
  DEBUG(dbgs() << "  - Stack Loc: " );
  // 0 is the header predicate, which we never need to clear
  for (unsigned pred = 1; pred < S->getNumPredicates(); pred++) {
    unsigned stloc;
    if (!R.getDefLoc(stloc, pred)) {
      // pred is defined in a stack location
      int fi; unsigned bitpos;
      getStackLocPair(fi, bitpos, stloc);
      DEBUG(dbgs() << "p" << pred << " " << stloc
          << " ("  << fi  << "/" << bitpos << "); ");
      if (!masks.count(fi)) {
        masks[fi] = 0;
      }
      masks[fi] |= (1 << bitpos);
    }
  }
  DEBUG(dbgs() << "\n");

  // Clear stack locations according to masks, at the beginning of the header
  MachineBasicBlock *MBB = S->getHeader();
  MachineBasicBlock::iterator MI = MBB->begin();
  if (S->isTopLevel()) {
    // skip frame setup
    while (MI->getFlag(MachineInstr::FrameSetup)) ++MI;
  }

  DEBUG(dbgs() << "  - Masks:\n" );
  DebugLoc DL;
  for (std::map<int, uint32_t>::iterator I = masks.begin(), E = masks.end();
      I != E; ++I) {
    int fi = I->first;
    uint32_t mask = I->second;
    DEBUG(dbgs() << "    fi " << fi  << " mask " << mask << "\n");
    // load from stack slot
    AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::LWC), GuardsReg))
      .addFrameIndex(fi).addImm(0); // address
    // insert AND instruction to clear predicates according to mask
    AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::ANDl),
          GuardsReg))
      .addReg(GuardsReg)
      .addImm(~mask);
    // store to stack slot
    AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::SWC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(GuardsReg, RegState::Kill);
    InsertedInstrs += 3; // STATISTIC
  }
}



void PatmosSPReduce::insertPredDefinitions(SPScope *S) {
  DEBUG( dbgs() << " Insert Predicate Definitions in [MBB#"
                << S->getHeader()->getNumber() << "]\n");

  // Visit the MBBs (in topological order).
  for (SPScope::iterator I=S->begin(), E=S->end(); I!=E; ++I) {
    MachineBasicBlock *MBB = *I;
    const SPScope::PredDefInfo *DI = S->getDefInfo(MBB);

    // procede to next if this does not define any predicates
    if (!DI) continue;

    DEBUG(dbgs() << " - MBB#" << MBB->getNumber() << ": ");

    // for each definition edge: insert
    for (SPScope::PredDefInfo::iterator di = DI->begin(), de = DI->end();
        di != de; ++di) {
      DEBUG(dbgs() << di->first << " ");
      // Scope, (local) Node, predicate number, edge
      insertDefEdge(S, *MBB, di->first, di->second);
    }
    DEBUG(dbgs() << "\n");

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
          R.isFirstDef(&Node, pred),         // isFirstDef
          S->isSubHeader(&Node)              // isExitEdgeDef
          );
    } else {
      // assert(there exists an inner R s.t. R.needsScopeSpill());
      // somewhere on the path from outer to inner scope, S0 is spilled

      // FIXME assumes direct parent-child relationship
      assert(RI.Scope->getParent() == S);
      unsigned slot = RI.Scope->getDepth()-1;

      // set a bit in the appropriate S0 spill slot
      insertDefToS0SpillSlot(*SrcMBB, slot, loc, use_preg, Cond);
    }
  } else {
    insertDefToStackLoc(*SrcMBB, loc, use_preg, Cond);
  }
}

void PatmosSPReduce::
insertDefToRegLoc(MachineBasicBlock &MBB, unsigned regloc, unsigned guard,
                  const SmallVectorImpl<MachineOperand> &Cond,
                  bool isMultiDef, bool isFirstDef, bool isExitEdgeDef) {

  // insert the predicate definitions before any branch at the MBB end
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL(MI->getDebugLoc());

  MachineInstr *DefMI;
  if (isExitEdgeDef || (isMultiDef && !isFirstDef)) {
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
                    const SmallVectorImpl<MachineOperand> &Cond) {

  // insert the predicate definitions before any branch at the MBB end
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL(MI->getDebugLoc());

  // The definition location of the predicate is a spill location.
  int fi; unsigned bitpos;
  getStackLocPair(fi, bitpos, stloc);
  unsigned tmpReg = GuardsReg;

  // load from stack slot
  AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::LWC), tmpReg))
    .addFrameIndex(fi).addImm(0); // address

#ifdef USE_BCOPY
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
  InsertedInstrs += 2; // STATISTIC
#endif
  // store back to stack slot
  AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::SWC)))
    .addFrameIndex(fi).addImm(0) // address
    .addReg(tmpReg, RegState::Kill);
  InsertedInstrs += 2; // STATISTIC
}


void PatmosSPReduce::
insertDefToS0SpillSlot(MachineBasicBlock &MBB, unsigned slot, unsigned regloc,
                       unsigned guard,
                       const SmallVectorImpl<MachineOperand> &Cond) {

  // insert the predicate definitions before any branch at the MBB end
  MachineBasicBlock::iterator MI = MBB.getFirstTerminator();
  DebugLoc DL(MI->getDebugLoc());

  int fi = PMFI->getSinglePathS0SpillFI(slot);
  unsigned tmpReg = GuardsReg;
  int bitpos = TRI->getS0Index(AvailPredRegs[regloc]);
  assert(bitpos > 0);

  // load from stack slot
  AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::LBC), tmpReg))
    .addFrameIndex(fi).addImm(0); // address

#ifdef USE_BCOPY
  // (guard) bcopy R, bitpos, Cond
  BuildMI(MBB, MI, DL, TII->get(Patmos::BCOPY), tmpReg)
    .addReg(guard).addImm(0) // guard
    .addReg(tmpReg)
    .addImm(bitpos)
    .addOperand(Cond[0]).addOperand(Cond[1]); // condition
  InsertedInstrs++; // STATISTIC
#else
  uint32_t or_bitmask = 1 << bitpos;
  // compute combined predicate (guard && condition)
  AddDefaultPred(BuildMI(MBB, MI, DL,
        TII->get(Patmos::PAND), PRTmp))
    .addReg(guard).addImm(0) // guard
    .addOperand(Cond[0]).addOperand(Cond[1]); // condition
  // set bit
  // if (guard && cond) R |= (1 << loc)
  assert(isUInt<12>(or_bitmask);
  BuildMI(MBB, MI, DL, TII->get(Patmos::ORi), tmpReg)
    .addReg(PRTmp).addImm(0) // if (guard && cond) == true
    .addReg(tmpReg)
    .addImm(or_bitmask);
  InsertedInstrs += 2; // STATISTIC
#endif
  // store back to stack slot
  AddDefaultPred(BuildMI(MBB, MI, DL, TII->get(Patmos::SBC)))
    .addFrameIndex(fi).addImm(0) // address
    .addReg(tmpReg, RegState::Kill);
  InsertedInstrs += 2; // STATISTIC
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
  DEBUG( dbgs() << " Applying predicates in [MBB#"
                << S->getHeader()->getNumber() << "]\n");

  const RAInfo &R = RAInfos.at(S);

  // Predicate the instructions of blocks in S, also inserting spill/load
  // of predicates not in registers.

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
          AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::SWC)))
            .addFrameIndex(fi).addImm(0) // address
            .addReg(Patmos::R9, RegState::Kill);
          // restore from stack slot (after the call MI)
          AddDefaultPred(BuildMI(*MBB, llvm::next(MI), DL,
                TII->get(Patmos::LWC), Patmos::R9))
            .addFrameIndex(fi).addImm(0); // address
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
    if (!S->isHeader(MBB) && R.hasSpillLoad(MBB)) {
      insertUseSpillLoad(R, MBB);
    }

    // if this is a reachable function, we need to get the
    // top-level predicate from the caller
    if (S->isTopLevel() && !S->isRootTopLevel() && S->isHeader(MBB)) {
      // skip unconditionally executed frame setup
      MachineBasicBlock::iterator MI = MBB->begin();
      while (MI->getFlag(MachineInstr::FrameSetup)) ++MI;
      AddDefaultPred(BuildMI(*MBB, MI, MI->getDebugLoc(),
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


void PatmosSPReduce::getStackLocPair(int &fi, unsigned &bitpos,
                                     const unsigned stloc) const {
  fi = PMFI->getSinglePathExcessSpillFI(stloc / 32);
  bitpos = stloc % 32;
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
      int fi; unsigned bitpos;
      getStackLocPair(fi, bitpos, spill);
      // load from stack slot
      AddDefaultPred(BuildMI(*MBB, firstMI, DL,
            TII->get(Patmos::LWC), GuardsReg))
        .addFrameIndex(fi).addImm(0); // address
      // set/clear bit
#ifdef USE_BCOPY
      // (guard) bcopy R, (spill%32), use_preg
      AddDefaultPred(BuildMI(*MBB, firstMI, DL,
            TII->get(Patmos::BCOPY), GuardsReg))
        .addReg(GuardsReg)
        .addImm(bitpos)
        .addReg(use_preg).addImm(0); // condition
      InsertedInstrs++; // STATISTIC
#else
      // if (guard) R |= (1 << spill)
      uint32_t or_bitmask = 1 << bitpos;
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
      InsertedInstrs += 2; // STATISTIC
#endif
      // store back to stack slot
      AddDefaultPred(BuildMI(*MBB, firstMI, DL, TII->get(Patmos::SWC)))
        .addFrameIndex(fi).addImm(0) // address
        .addReg(GuardsReg, RegState::Kill);
      InsertedInstrs += 2; // STATISTIC (load/store)
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
  int fi; unsigned bitpos;
  getStackLocPair(fi, bitpos, loc);
  // load from stack slot
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::LWC), GuardsReg))
    .addFrameIndex(fi).addImm(0); // address
  // test bit
  // BTESTI $Guards, loc
  AddDefaultPred(BuildMI(*MBB, MI, DL, TII->get(Patmos::BTESTI), target_preg))
    .addReg(GuardsReg, RegState::Kill).addImm(bitpos);
  InsertedInstrs += 2; // STATISTIC
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

  ReturnInfoInsts.clear();

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


void PatmosSPReduce::eliminateFrameIndices(MachineFunction &MF) {

  for (MachineFunction::iterator MBB = MF.begin(), MBBe = MF.end();
      MBB != MBBe; ++MBB) {
    for (MachineBasicBlock::iterator MI = MBB->begin(), MIe = MBB->end();
        MI != MIe; ++MI) {
      if (MI->mayStore() && MI->getOperand(2).isFI()) {
        TRI->eliminateFrameIndex(MI, 0, 2);
      }
      if (MI->mayLoad() && MI->getOperand(3).isFI()) {
        TRI->eliminateFrameIndex(MI, 0, 3);
      }
    }
  }
}


void PatmosSPReduce::getLoopLiveOutPRegs(const SPScope *S,
                                         std::vector<unsigned> &pregs) const {

  const std::vector<const MachineBasicBlock *> SuccMBBs = S->getSuccMBBs();

  pregs.clear();
  for (unsigned j=0; j<SuccMBBs.size(); j++) {
    for (unsigned i=0; i<UnavailPredRegs.size(); i++) {
      if (SuccMBBs[j]->isLiveIn(UnavailPredRegs[i])) {
        DEBUG(dbgs() << "LiveIn: " << TRI->getName(UnavailPredRegs[i])
            << " into MBB#" << SuccMBBs[j]->getNumber() << "\n");
        pregs.push_back(UnavailPredRegs[i]);
      }
    }
  }
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

  SPNumPredicates += Scope->getNumPredicates(); // STATISTIC

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
            // reassign, but
            // DO NOT free stack locations again, i.e. not freeLoc(curUseLoc);
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
            // Do not free the stack location, as it would require
            // re-initialization with 0. I assume, batch initialization with
            // masks in the header are cheaper than an additional instruction
            // before the first definition (provided there are more than one
            // stack locations for predicates).
            //freeLoc(curUseLoc); // <-- we don't do this!
            UL.load  = curUseLoc;
            UL.loc   = curLocs[furthestPred];
            // differentiate between already used and not yet used
            if (LRs[furthestPred].pastFirstUse(i)) {
              UL.spill = stackLoc;
            } else {
              // if it has not been used, we change the initial
              // definition location
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
        DEBUG(dbgs() << "retire. ");
        freeLoc( curUseLoc );
        curUseLoc = -1;
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
  // Code for loading the predicate is placed before the back-branch,
  // generated in LinearizeWalker::exitSubscope().
  if (!Scope->isTopLevel()) {
    UseLoc &ul = UseLocs[Scope->getHeader()];
    if (ul.loc != curLocs[0]) {
      ul.load = curLocs[0];
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

  const SPScope *SParent = S->getParent();

  const MachineBasicBlock *HeaderMBB = S->getHeader();

  const RAInfo &RI = Pass.RAInfos.at(S),
               &RP = Pass.RAInfos.at(SParent);

  DebugLoc DL;

  if (RI.needsScopeSpill()) {
    // load the predicate registers to GuardsReg, and store them to the
    // allocated stack slot for this scope depth.
    int fi = Pass.PMFI->getSinglePathS0SpillFI(S->getDepth() - 1);
    Pass.TII->copyPhysReg(*PrehdrMBB, PrehdrMBB->end(), DL,
        Pass.GuardsReg, Patmos::S0, false);
    // we insert a dummy load for the RedundantLdStEliminator
    MachineInstr *Dummy = AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(),
          DL, Pass.TII->get(Patmos::LBC), Pass.GuardsReg))
          .addFrameIndex(fi).addImm(0); // address
    Pass.GuardsLdStElim->addRemovableInst(Dummy);
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SBC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(Pass.GuardsReg, RegState::Kill);
    InsertedInstrs += 3; // STATISTIC
  }

  // copy/load the header predicate for the subloop
  int loadloc = RP.getLoadLoc(HeaderMBB);
  unsigned inner_preg = Pass.getUsePReg(RI, HeaderMBB, true);
  if (loadloc != -1) {
    Pass.insertPredicateLoad(PrehdrMBB, PrehdrMBB->end(),
        loadloc, inner_preg);
    InsertedInstrs++; // STATISTIC
  } else {
    unsigned outer_preg = Pass.getUsePReg(RP, HeaderMBB, true);
    assert(inner_preg != Patmos::P0);
    if (outer_preg != inner_preg) {
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::PMOV), inner_preg))
        .addReg( outer_preg ).addImm(0);
      InsertedInstrs++; // STATISTIC
    }
  }

  // Insert initialization code in the preheader for predicates that are
  // defined in a register location by exit-edges of the entered subscope.
  // The subscope is represented by the header.
  // NB: We need to do this after copying the header predicate, as the
  //     predicate might retire, be reused for exit edge defs and the
  //     initialization code would clear the header predicate before ever
  //     used in the subloop
  const SPScope::PredDefInfo *DI = SParent->getDefInfo(HeaderMBB);
  if (DI) {
    DEBUG(dbgs() << "  (reg loc first defined in subscope: ");

    std::vector<unsigned> defregs;
    // for each definition edge
    for (SPScope::PredDefInfo::iterator di = DI->begin(), de = DI->end();
        di != de; ++di) {
      unsigned loc, pred = di->first;
      if (RP.getDefLoc(loc, pred)) {
        // pred is defined in a register; we only need initialisation code if
        // it is defined in the subscope as first definition
        if (RP.isFirstDef(HeaderMBB, pred)) {
          unsigned reg = Pass.AvailPredRegs[loc];
          defregs.push_back(reg);
          DEBUG(dbgs() << Pass.TRI->getName(reg) << ", ");
        }
      }
    }
    DEBUG(dbgs() << ")\n");

    // insert the instructions
    if (RI.needsScopeSpill()) {
      uint32_t mask = 0;
      for (unsigned i = 0; i < defregs.size(); i++) {
        mask |= (1 << Pass.TRI->getS0Index(defregs[i]));
      }
      assert(isUInt<12>(mask));

      int fi = Pass.PMFI->getSinglePathS0SpillFI(S->getDepth() - 1);
      // load from stack slot
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::LBC), Pass.GuardsReg))
        .addFrameIndex(fi).addImm(0); // address
      // mask out the initialization bits
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
          Pass.TII->get(Patmos::ANDl), Pass.GuardsReg))
      .addReg(Pass.GuardsReg)
      .addImm(~mask);
      // store back to stack slot
      AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SBC)))
        .addFrameIndex(fi).addImm(0) // address
        .addReg(Pass.GuardsReg, RegState::Kill);
      InsertedInstrs += 3; // STATISTIC
    } else {
      for (unsigned i = 0; i < defregs.size(); i++) {
        AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
              Pass.TII->get(Patmos::PCLR), defregs[i]));
        InsertedInstrs++; // STATISTIC
      }
    }
  } // end if (DI)


  // Initialize the loop bound and store it to the stack slot
  if (S->hasLoopBound()) {
    unsigned tmpReg = Pass.GuardsReg;
    uint32_t loop = S->getLoopBound();
    // Create an instruction to load the loop bound
    // TODO try to find an unused register
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
          Pass.TII->get( (isUInt<12>(loop)) ? Patmos::LIi : Patmos::LIl),
          tmpReg))
      .addImm(loop); // the loop bound

    int fi = Pass.PMFI->getSinglePathLoopCntFI(S->getDepth()-1);
    // we insert a dummy load for the RedundantLdStEliminator
    MachineInstr *Dummy = AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(),
          DL, Pass.TII->get(Patmos::LWC), Pass.GuardsReg))
          .addFrameIndex(fi).addImm(0); // address
    Pass.GuardsLdStElim->addRemovableInst(Dummy);
    // store the initialized loop bound to its stack slot
    AddDefaultPred(BuildMI(*PrehdrMBB, PrehdrMBB->end(), DL,
            Pass.TII->get(Patmos::SWC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg, RegState::Kill);
    InsertedInstrs += 2; // STATISTIC
    LoopCounters++; // STATISTIC
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
  //
  // load the header predicate, if necessary
  unsigned header_preg = Pass.getUsePReg(RI, HeaderMBB);
  int loadloc = RI.getLoadLoc(HeaderMBB);
  if (loadloc != -1) {
    Pass.insertPredicateLoad(BranchMBB, BranchMBB->end(),
        loadloc, header_preg);
  }
  // TODO copy between pregs?


  // load the branch predicate
  unsigned branch_preg = Patmos::NoRegister;
  if (S->hasLoopBound()) {
    // load the loop counter, decrement it by one, and if it is not (yet)
    // zero, we enter the loop again.
    // TODO is the loop counter in a register?!
    int fi = Pass.PMFI->getSinglePathLoopCntFI(S->getDepth() - 1);
    unsigned tmpReg = Pass.GuardsReg;
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::LWC), tmpReg))
      .addFrameIndex(fi).addImm(0); // address

    // decrement
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::SUBi), tmpReg))
      .addReg(tmpReg).addImm(1);
    // compare with 0, PRTmp as predicate register
    branch_preg = Pass.PRTmp;
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::CMPLT), branch_preg))
      .addReg(Patmos::R0).addReg(tmpReg);
    // store back
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
            Pass.TII->get(Patmos::SWC)))
      .addFrameIndex(fi).addImm(0) // address
      .addReg(tmpReg, RegState::Kill);
    InsertedInstrs += 4; // STATISTIC
  } else {
    // no explicit loop bound: branch on header predicate
    branch_preg = header_preg;
  }
  // insert branch to header
  assert(branch_preg != Patmos::NoRegister);

#ifdef BOUND_UNDEREST_PROTECTION
  if (branch_preg != header_preg) {
    AddDefaultPred(BuildMI(*BranchMBB, BranchMBB->end(), DL,
          Pass.TII->get(Patmos::POR), branch_preg))
      .addReg(branch_preg).addImm(0)
      .addReg(header_preg).addImm(0);
    InsertedInstrs++; // STATISTIC
  }
#endif

  // branch condition: not(<= zero)
  BuildMI(*BranchMBB, BranchMBB->end(), DL, Pass.TII->get(Patmos::BR))
    .addReg(branch_preg).addImm(0)
    .addMBB(HeaderMBB);
  BranchMBB->addSuccessor(HeaderMBB);
  InsertedInstrs++; // STATISTIC

  // create a post-loop MBB to restore the spill predicates, if necessary
  if (RI.needsScopeSpill()) {
    MachineBasicBlock *PostMBB = MF.CreateMachineBasicBlock();
    MF.push_back(PostMBB);
    // we create a LBC instruction here; TRI->eliminateFrameIndex() will
    // convert it to a stack cache access, if the stack cache is enabled.
    int fi = Pass.PMFI->getSinglePathS0SpillFI(S->getDepth()-1);
    unsigned tmpReg = Pass.GuardsReg;
    AddDefaultPred(BuildMI(*PostMBB, PostMBB->end(), DL,
            Pass.TII->get(Patmos::LBC), tmpReg))
      .addFrameIndex(fi).addImm(0); // address

    // if there are any PRegs to be preserved, do it now
    std::vector<unsigned> liveouts;
    Pass.getLoopLiveOutPRegs(S, liveouts);
    for(unsigned i = 0; i < liveouts.size(); i++) {
      AddDefaultPred(BuildMI(*PostMBB, PostMBB->end(), DL,
            Pass.TII->get(Patmos::BCOPY), tmpReg))
        .addReg(tmpReg)
        .addImm(Pass.TRI->getS0Index(liveouts[i]))
        .addReg(liveouts[i]).addImm(0);
      InsertedInstrs++; // STATISTIC
    }

    // assign to S0
    Pass.TII->copyPhysReg(*PostMBB, PostMBB->end(), DL,
                          Patmos::S0, tmpReg, true);
    nextMBB(PostMBB);
    InsertedInstrs += 2; // STATISTIC
  }
}

///////////////////////////////////////////////////////////////////////////////

