//===----- PatmosPostRAScheduler.h - list scheduler -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// PatmosPostRASchedStrategy implements the Post-RA VLIW scheduler for Patmos.
//
//===----------------------------------------------------------------------===//
#ifndef PATMOSSCHEDSTRATEGY_H
#define PATMOSSCHEDSTRATEGY_H

#include "PatmosPostRAScheduler.h"
#include "llvm/CodeGen/LatencyPriorityQueue.h"

namespace llvm {

  /// Order nodes by the ILP metric. Copied from MachineScheduler.
  struct ILPOrder {
    const SchedDFSResult *DFSResult;
    const BitVector *ScheduledTrees;
    bool MaximizeILP;

    ILPOrder(bool MaxILP): DFSResult(0), ScheduledTrees(0), MaximizeILP(MaxILP) {}

    /// \brief Apply a less-than relation on node priority.
    ///
    /// (Return true if A comes after B in the Q.)
    bool operator()(const SUnit *A, const SUnit *B) const;
  };

  class  PatmosTargetMachine;
  struct PatmosRegisterInfo;

  class PatmosPostRASchedStrategy : public PostRASchedStrategy  {
  private:
    const PatmosTargetMachine &PTM;
    const PatmosInstrInfo &PII;
    const PatmosRegisterInfo &PRI;

    /// Should we create bundles?
    bool EnableBundles;

    /// The current DAG that we are scheduling
    ScheduleDAGPostRA *DAG;

    /// Copied from the ILPSchedStrategy from MachineScheduler

    static const unsigned SubtreeLimit = 16;
    ILPOrder Cmp;
    std::vector<SUnit*> ReadyQ;

    /// AvailableQueue - The priority queue to use for the available SUnits.
    ///
    //LatencyPriorityQueue AvailableQueue;

    /// PendingQueue - This contains all of the instructions whose operands have
    /// been issued, but their results are not ready yet (due to the latency of
    /// the operation).  Once the operands becomes available, the instruction is
    /// added to the AvailableQueue.
    //std::vector<SUnit*> PendingQueue;

    /// The branch instruction in this region, or NULL.
    SUnit *CFL;

    /// Keep track of the delay slot cycles of the branch delay
    unsigned int DelaySlot;

    /// Already scheduled cycles to the end of the region.
    unsigned int CurrCycle;

    /// Set to true if the next instruction must not be bundled.
    bool EndBundle;

  public:
    PatmosPostRASchedStrategy(const PatmosTargetMachine &PTM);
    virtual ~PatmosPostRASchedStrategy() {}

    /// isSchedulingBoundary - Test if the given instruction should be
    /// considered a scheduling boundary.
    virtual bool isSchedulingBoundary(const MachineInstr *MI,
                                                const MachineBasicBlock *MBB,
                                                const MachineFunction &MF);

    /// canHandleTerminators - Return true if this strategy schedules terminator
    /// instructions properly.
    virtual bool canHandleTerminators() { return true; }

    /// Postprocess the scheduling DAG before initializing the scheduler.
    virtual void postprocessDAG(ScheduleDAGPostRA *DAG);

    /// Initialize the strategy after building the DAG for a new region.
    virtual void initialize(ScheduleDAGPostRA *DAG);

    virtual void finalize(ScheduleDAGPostRA *DAG);

    /// Notify this strategy that all roots have been released (including those
    /// that depend on EntrySU or ExitSU).
    virtual void registerRoots();

    virtual bool pickNode(SUnit *&SU, bool &IsTopNode, bool &IsBundled);

    /// \brief Scheduler callback to notify that a new subtree is scheduled.
    virtual void scheduleTree(unsigned SubtreeID);

    /// Notify PostRASchedStrategy that ScheduleDAGPostRA has scheduled an
    /// instruction and updated scheduled/remaining flags in the DAG nodes.
    virtual void schedNode(SUnit *SU, bool IsTopNode, bool IsBundled);

    /// Notify PostRASchedStrategy that a NOOP has been scheduled.
    virtual void schedNoop(bool IsTopNode);

    /// When all predecessor dependencies have been resolved, free this node for
    /// top-down scheduling.
    virtual void releaseTopNode(SUnit *SU);

    /// When all successor dependencies have been resolved, free this node for
    /// bottom-up scheduling.
    virtual void releaseBottomNode(SUnit *SU);

  private:

    /// Remove dependencies to a call or return due to implicit uses of the
    /// return values, arguments or caller saved registers.
    /// Does not remove dependencies to return info registers.
    void removeImplicitCFLDeps(SUnit &Ret);

    /// Adapt latencies to the exit node.
    void updateExitLatencies(SUnit &ExitSU);

    /// Remove barrier and memory deps between instructions that access
    /// different memory types and cannot alias.
    void removeTypedMemBarriers();

    /// Remove all dependencies between instructions with mutually exclusive
    /// predicates.
    void removeExclusivePredDeps();

    /// Check if the operand of an instruction is actually used by
    /// the instruction or if it is just return info, arguments or caller saved
    /// registers.
    bool isExplicitCFLOperand(MachineInstr *MI, MachineOperand &MO);

    /// Get the latency to the exit node of a node.
    unsigned computeExitLatency(SUnit &SU);
  };

}

#endif
