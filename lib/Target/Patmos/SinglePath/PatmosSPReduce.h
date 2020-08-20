//===---- PatmosSPReduce.h - Reduce the CFG for Single-Path code ----------===//
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

#ifndef TARGET_PATMOS_SINGLEPATH_PATMOSSPREDUCE_H_
#define TARGET_PATMOS_SINGLEPATH_PATMOSSPREDUCE_H_

#include "PatmosSPBundling.h"
#include "RAInfo.h"

namespace llvm {

  class LinearizeWalker;
  class RedundantLdStEliminator;

  class PatmosSPReduce : public MachineFunctionPass {
  private:

    friend class LinearizeWalker;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;
    const PatmosRegisterInfo *TRI;

    // The pointer to the PatmosMachinFunctionInfo is set upon running on a
    // particular function. It contains information about stack slots for
    // predicate spilling and loop bounds.
    const PatmosMachineFunctionInfo *PMFI;

    /// doReduceFunction - Reduce a given MachineFunction
    void doReduceFunction(MachineFunction &MF);

    /// createRAInfo - Helper function to create a new RAInfo for an SPScope
    /// and insert it in the RAInfos map of the pass.
    /// Returns a reference to the newly created RAInfo.
    RAInfo &createRAInfo(SPScope *S);

    /// getEdgeCondition - Get the predicate operand corresponding
    /// to a edge (predicate operand is true -> edge is taken)
    /// Side effect: branch conditions where the register operand
    /// contained a kill flag are stored in KilledCondRegs.
    SmallVector<MachineOperand, 2> getEdgeCondition(
        const PredicatedBlock* sourceBlock,
        PredicatedBlock::Definition def);

    /// insertStackLocInitializations - Insert predicate initializations
    /// for predicates located on the stack.
    void insertStackLocInitializations(SPScope *S);

    /// insertPredDefinitions - Insert predicate register definitions
    /// to MBBs of the given SPScope.
    void insertPredDefinitions(SPScope *S);

    /// insertDefEdge - insert instructions for definition of a predicate
    /// by a definition edge.
    /// @param S          local scope
    /// @param block      the block whos definition to insert
    /// @param predType   type of the predicate, i.e., physical predicate
    ///                     or stack location
    /// @param predLoc    either physical predicate register or stack location
    ///                     to be defined. (depends on 'predType')
    /// @param guardLoc   physical predicate register guarding the definition
    /// @param cond       condition predicate register and its flag
    /// @param first_def  whether this definition is the first defintion of the 
    ///                     physical predicate register. If false, the register
    ///                     already contains a another definition of the abstract 
    ///                     predicate from a different control flow path.
    ///                     If 'predType' isn't a register, this boolean is undefined.
    void insertDefEdge(SPScope *S, const PredicatedBlock *block,
        RAInfo::LocType predType, unsigned predLoc, unsigned guardLoc, 
        SmallVector<MachineOperand, 2> cond, bool first_def);

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
    /// @param predReg the physical predicate register
    /// @param guard the guard of MBB
    /// @param Cond the condition which should be assigned to the predicate
    void insertDefToS0SpillSlot(MachineBasicBlock &MBB, unsigned slot,
                    unsigned predReg, unsigned guard,
                    const SmallVectorImpl<MachineOperand> &Cond);

    /// insertDefToRegLoc - insert a predicate definition to a  a physical register
    /// @param MBB       the machine basic block at which end the definition
    ///                    should be placed
    /// @param predReg   the predicate register to define
    /// @param guard     the guard of MBB
    /// @param Cond      the condition which should be assigned to the predicate
    /// @param usePmov   whether to define the register using a guarded PMOV instructions
	///                    if false, instead uses a non-guarded PAND 
	///                    (the 'guard' is used as an operand instead)
    void insertDefToRegLoc(MachineBasicBlock &MBB, unsigned predReg,
                           unsigned guard,
                           const SmallVectorImpl<MachineOperand> &Cond,
                           bool usePmov);

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
    void insertUseSpillLoad(const RAInfo &R, PredicatedBlock *block);

    /// insertPredicateLoad - Insert code to load from a spill stack slot to
    /// a predicate register.
    void insertPredicateLoad(MachineBasicBlock *MBB,
                             MachineBasicBlock::iterator MI,
                             int loc, unsigned target_preg);

    /// Returns which registers are used for each predicate use by the given block.
    std::map<unsigned, unsigned> getPredicateRegisters(const RAInfo &R, const PredicatedBlock *MBB);

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

    // Branches that set the kill flag on condition operands are remembered,
    // as the branches themselves are removed. The last use of these
    // conditions before the branch will be set the kill flag
    std::map<MachineBasicBlock *, MachineOperand> KilledCondRegs;

    // To preserve the call hierarchy (calls are unconditional in single-path
    // code) instructions that store/restore return information (s7+s8)
    // need to be excluded from predication
    std::set<const MachineInstr *> ReturnInfoInsts;

  public:
    /// Pass ID
    static char ID;

    SPScope *RootScope;

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
      AU.addRequired<PatmosSPBundling>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    /// runOnMachineFunction - Run the SP converter on the given function.
    virtual bool runOnMachineFunction(MachineFunction &MF) {
      RootScope = getAnalysis<PatmosSPBundling>().getRootScope();
      PMFI = MF.getInfo<PatmosMachineFunctionInfo>();
      bool changed = false;
      // only convert function if marked
      if ( MF.getInfo<PatmosMachineFunctionInfo>()->isSinglePath()) {
        DEBUG( dbgs() << "[Single-Path] Reducing "
                      << MF.getFunction()->getName() << "\n" );
        doReduceFunction(MF);
        changed |= true;
      }
      return changed;
    }
  };

}

#endif /* TARGET_PATMOS_SINGLEPATH_PATMOSSPREDUCE_H_ */
