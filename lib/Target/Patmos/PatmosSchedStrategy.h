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
// Here is an overview of the scheduling infrastructure
//
// - ScheduleDAGVLIW: Schedules SDNodes during lowering, enabled by setting
//   SchedulePreference to VLIW in PatmosISelLowering. Uses the HazardRecognizer
//   but not the DFAPacketizer to schedule for VLIW bundles. Deprecated and
//   is disabled by default in favour of MachineScheduler/PostRAScheduler.
//
// - MachineScheduler: Scheduler framework pass that uses a ScheduleDAGInstr to
//   schedule pre-RA, does not work post-RA (for now). This pass does not fill
//   delay slots. It might only create temporary bundles for RA (?).
//
//   ScheduleDAGInstr Implementations
//   - ScheduleDAGMI: Default generic scheduler implementing register tracking
//     using LiveIntervals analysis, not scheduling for VLIW. Uses the hazard
//     recognizer but not the DFAPacketizer.
//     Uses a MaschineSchedStrategy to pick nodes and set scheduling direction.
//
//     - PatmosVLIWSchedStrategy: Implements the MachineSchedStrategy for the
//       generic ScheduleDAGMI pre-RA scheduler. TBD. Should order code so that
//       the RegAlloc does not introduce pseudo dependencies, i.e. be bundling
//       aware. Do bottom-up scheduling
//
//     - ConvergingSchedStrategy, ..: generic LLVM scheduling strategies.
//
//   - PatmosVLIWScheduleDAGMI: Generic pre-RA VLIW scheduler implementing
//     converging bottom up/top-down scheduling or similar. TBD.
//     Scheduling strategy should either be the same interface as for the
//     PostRA scheduler, the ScheduleDAGMI, or at least share a common base.
//     TODO this should share code with the PostRAScheduler, interface for
//     VLIW SchedStrategy remains to be defined.
//
// - [ Register Allocation, is bundle-aware? ]
//
// - PostRASchedulerList: Default scheduler, uses the Post-RA HazardRecognizer,
//   but not the DFAPacketizer. Does not schedule for VLIW or delay slots, but
//   emits NOOPs. Top Down scheduler similar to MachineScheduler, but not
//   configurable. Disabled by default for Patmos, not used, might be removed
//   in LLVM 3.4+ releases.
//
// - PatmosPostRAScheduler: Generic post-RA scheduler pass, replaces the
//   PostRAScheduler, works post-RA. Uses ScheduleDAGPostRA and
//   PatmosPostRASchedStrategy to schedule, create bundles and fill delay slots.
//   Post-RA version of the MachineScheduler pass.
//   TODO use a scheduler registry similar to MachineScheduler to select
//   schedulers at runtime
//   TODO PatmosPostRAScehduler and ScheduleDAGPostRA should be moved into the
//   generic framework, maybe merged with MachineScheduler if dependency on
//   pre-RA are removed (?)
//
//   - ScheduleDAGPostRA: Generic post-RA scheduler, supports VLIW and delay
//     slots, uses AntiDepBreaking and uses a PostRASchedStrategy to pick nodes
//     and select schedule direction.
//     Different from ScheduleDAGMI as it creates final bundles and delay slots.
//
//     - PatmosPostRASchedStrategy: Post RA Scheduling strategy for Patmos.
//       Fills delay slots, and creates bundles if bundling is enabled.
//
// - PatmosDelaySlotFiller: Fallback delay slot filler, only
//   executed if PatmosPostRAScheduler is not used. Performs No bundling, should
//   be kept simple.

// - PatmosPacketizer, PatmosBundleSanitizer: Used the DFAPacketizer and
//   HazardRecognizer to create bundles. BundleSanitizer to fix order of
//   instructions in bundles. Removed in favour of new PostRAScheduler.
//
//===----------------------------------------------------------------------===//
#ifndef PATMOSSCHEDSTRATEGY_H
#define PATMOSSCHEDSTRATEGY_H

#include "PatmosPostRAScheduler.h"
#include "llvm/CodeGen/MachineScheduler.h"
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
    : PII(*PTM.getInstrInfo()), Cmp(false)
    {
      const PatmosSubtarget &PST = *PTM.getSubtargetImpl();

      IssueWidth = PST.enableBundling(PTM.getOptLevel()) ?
                   PST.getSchedModel()->IssueWidth : 1;
    }

    unsigned getIssueWidth() const { return IssueWidth; }

    void setIssueWidth(unsigned width) { IssueWidth = width; }

    void setDFSResult(ScheduleDAGPostRA *DAG);

    void clear();

    bool empty();

    void initialize();

    /// Select a bundle for the current cycle. The selected instructions are
    /// put into bundle in the correct issue order. If no instruction can be
    /// issued, false is returned.
    bool selectBundle(std::vector<SUnit*> &Bundle);

    /// Go back one cycle and update availability queue.
    void recedeCycle(unsigned CurrCycle);

    /// Notify the queue that this instruction has now been scheduled.
    void scheduled(SUnit *SU, unsigned CurrCycle);

    /// Notify the queue that a new subtree is now getting scheduled.
    void scheduledTree(unsigned SubtreeID);

    /// put an instruction into the pending queue when all its successors have
    /// been scheduled.
    void makePending(SUnit *SU);

#ifndef NDEBUG
    void dump();
#endif

  protected:
    bool canIssueInSlot(SUnit *SU, unsigned Slot);

    /// Try to add an instruction to the bundle, return true if succeeded.
    /// \param Width the current width of the bundle, will be updated.
    bool addToBundle(std::vector<SUnit *> &Bundle, SUnit *SU, unsigned &Width);
  };

  class  PatmosTargetMachine;
  struct PatmosRegisterInfo;


  // TODO implement, share code with PostRASchedStrategy if possible.
  class PatmosVLIWSchedStrategy : public MachineSchedStrategy {

    //const TargetSchedModel *SchedModel;
    //const TargetRegisterInfo *TRI;

  public:

    PatmosVLIWSchedStrategy() {}

    virtual void initialize(ScheduleDAGMI *dag) {}

    virtual SUnit *pickNode(bool &IsTopNode) { return 0; }

    virtual void schedNode(SUnit *SU, bool IsTopNode) {}

    virtual void releaseTopNode(SUnit *SU) {}

    virtual void releaseBottomNode(SUnit *SU) {}

  };


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

    /// Are we emitting a pure pseudo instructions bundle?
    bool CurrIsPseudo;

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
