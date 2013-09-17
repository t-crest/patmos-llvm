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

    /// \brief Apply a greater-than relation on node priority.
    ///
    /// (Return true if A has higher priority than B)
    bool operator()(const SUnit *A, const SUnit *B) const;
  };

  /// This class manages a list of pending and available instructions and
  /// allows to pick the best instruction or bundle currently available.
  class PatmosLatencyQueue {
  private:
    const PatmosInstrInfo &PII;

    /// Max number of slots to fill when selecting a bundle.
    unsigned IssueWidth;

    ILPOrder Cmp;

    /// PendingQueue - This contains all of the instructions whose operands have
    /// been issued, but their results are not ready yet (due to the latency of
    /// the operation).  Once the operands becomes available, the instruction is
    /// added to the AvailableQueue.
    std::vector<SUnit*> PendingQueue;

    /// AvailableQueue - The priority queue to use for the available SUnits.
    std::vector<SUnit*> AvailableQueue;

  public:
    PatmosLatencyQueue(const PatmosTargetMachine &PTM)
    : PII(*PTM.getInstrInfo()), Cmp(true)
    {
      const PatmosSubtarget &PST = *PTM.getSubtargetImpl();

      IssueWidth = PST.enableBundling(PTM.getOptLevel()) ?
                   PST.getSchedModel()->IssueWidth : 1;
    }

    unsigned getIssueWidth() const { return IssueWidth; }

    void setIssueWidth(unsigned width) { IssueWidth = width; }

    void setDFSResult(ScheduleDAGPostRA *DAG);

    void clear();

    void initialize();

    /// Select a bundle for the current cycle. The selected instructions are
    /// put into bundle in the correct issue order. If no instruction can be
    /// issued, false is returned.
    bool selectBundle(std::vector<SUnit*> &Bundle);

    /// Go back one cycle and update availability queue. If no more
    /// instructions need to be scheduled, return false.
    bool recedeCycle(unsigned CurrCycle);

    /// Notify the queue that this instruction has now been scheduled.
    void scheduled(SUnit *SU, unsigned CurrCycle);

    /// Notify the queue that a new subtree is now getting scheduled.
    void scheduledTree(unsigned SubtreeID);

    /// put an instruction into the pending queue when all its successors have
    /// been scheduled.
    void makePending(SUnit *SU);

    void dump();

  protected:
    bool canIssueInSlot(SUnit *SU, unsigned Slot);

    /// Try to add an instruction to the bundle, return true if succeeded.
    /// \param Width the current width of the bundle, will be updated.
    bool addToBundle(std::vector<SUnit *> &Bundle, SUnit *SU, unsigned &Width);
  };

  class  PatmosTargetMachine;
  struct PatmosRegisterInfo;

  class PatmosPostRASchedStrategy : public PostRASchedStrategy  {
  private:
    /// Copied from the ILPSchedStrategy from MachineScheduler
    static const unsigned SubtreeLimit = 16;

    const PatmosTargetMachine &PTM;
    const PatmosInstrInfo &PII;
    const PatmosRegisterInfo &PRI;

    /// The current DAG that we are scheduling
    ScheduleDAGPostRA *DAG;

    /// The queue of pending and available instructions.
    PatmosLatencyQueue ReadyQ;

    /// Already scheduled cycles to the end of the region.
    unsigned int CurrCycle;

    /// The current bundle that we are emitting
    std::vector<SUnit*> CurrBundle;

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

    /// Notify PostRASchedStrategy that ScheduleDAGPostRA has rescheduled an
    /// instruction.
    virtual void reschedNode(SUnit *SU, bool IsTopNode, bool IsBundled);

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
