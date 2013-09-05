//===----- PatmosPostRAScheduler.h - list scheduler -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// PatmosPostRAScheduler implements a bottom-up scheduler based on
// ScheduleDAGInstr. This is similar to the PostRASchedulerList pass, but
// uses the PatmosScheduleDAG.
//
// This is also largely similar to MachineScheduler, does not use register
// pressure tracking and works post-RA on non-SSA code.
//
// Here is an overview of the scheduling infrastructure
//
// - ScheduleDAGVLIW: Schedules SDNodes during lowering, enabled by setting
//   SchedulePreference to VLIW in PatmosISelLowering. Uses the HazardRecognizer
//   but not the DFAPacketizer to schedule for VLIW bundles. Deprecated and
//   should be disabled in the future in favour of MachineScheduler.
//
// - MachineScheduler: Uses a ScheduleDAGInstr to schedule pre-RA, does not work
//   post-RA. This pass does not fill delay slots. It might only create
//   temporary bundles for RA.
//
//   ScheduleDAGInstr Implementations
//   - ScheduleDAGMI: Default scheduler implementing register tracking using
//     LiveIntervals analysis, not scheduling for VLIW. Uses the hazard
//     recognizer but not the DFAPacketizer.
//     Uses a MaschineSchedStrategy to pick nodes and set scheduling direction.
//
//   - PatmosVLIWScheduleDAGMI: Patmos Pre-RA VLIW scheduler implementing
//     bottom up scheduling. TBD.
//     TODO this should share code with the PostRAScheduler, interface for
//     VLIW SchedStrategy remains to be defined.
//
// - [ Register Allocation, is bundle-aware? ]
//
// - PostRASchedulerList: Default scheduler, uses the Post-RA HazardRecognizer,
//   but not the DFAPacketizer. Does not schedule for VLIW or delay slots, but
//   emits NOOPs. Top Down scheduler similar to MachineScheduler, but not
//   configurable.
//
// - PatmosPostRAScheduler: Replaces the PostRAScheduler, works post-RA.
//   Uses ScheduleDAGPostRA and PatmosPostRASchedStrategy to schedule, create
//   bundles and fill delay slots.
//
//   - ScheduleDAGPostRA: Generic scheduler for VLIW, works post-RA, uses
//     AntiDepBreaking and uses a PostRASchedStrategy to pick nodes and select
//     schedule direction.
//
//     - PatmosPostRASchedStrategy: Post RA Scheduling strategy for Patmos.
//       Fills delay slots, and creates bundles if bundling is enabled.
//
// - PatmosPacketizer, PatmosDelaySlotFiller, PatmosBundleSanitizer: Only
//   executed if PatmosPostRAScheduler is not used. Uses the DFAPacketizer and
//   HazardRecognizer to create bundles. BundleSanitizer fixes order of
//   instructions in bundles, does not work at the moment.
//
//===----------------------------------------------------------------------===//
#ifndef PATMOSPOSTRASCHEDULER_H
#define PATMOSPOSTRASCHEDULER_H

#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/LatencyPriorityQueue.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/ScheduleDAGInstrs.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"

namespace llvm {

  class MachineLoopInfo;
  class MachineDominatorTree;
  class TargetPassConfig;
  class AliasAnalysis;
  class RegisterClassInfo;
  class TargetRegisterClass;
  class SUnit;
  class SchedDFSResult;
  class AntiDepBreaker;
  
  /// MachineSchedContext provides enough context from the MachineScheduler pass
  /// for the target to instantiate a scheduler.
  struct PostRASchedContext {
    MachineFunction *MF;
    const MachineLoopInfo *MLI;
    const MachineDominatorTree *MDT;
    const TargetPassConfig *PassConfig;
    AliasAnalysis *AA;

    RegisterClassInfo *RegClassInfo;

    TargetSubtargetInfo::AntiDepBreakMode AntiDepMode;
    SmallVector<const TargetRegisterClass*, 4> CriticalPathRCs;

    PostRASchedContext();
    virtual ~PostRASchedContext();
  };


  class ScheduleDAGPostRA;

  class PostRASchedStrategy {
  public:
    virtual ~PostRASchedStrategy() {}

    /// isSchedulingBoundary - Test if the given instruction should be
    /// considered a scheduling boundary.
    virtual bool isSchedulingBoundary(const MachineInstr *MI,
                                                const MachineBasicBlock *MBB,
                                                const MachineFunction &MF) = 0;

    /// canHandleTerminators - Return true if this strategy schedules terminator
    /// instructions properly.
    virtual bool canHandleTerminators() { return false; }

    /// Initialize the strategy after building the DAG for a new region.
    virtual void initialize(ScheduleDAGPostRA *DAG) = 0;

    /// Finalize the strategy after scheduling the DAG for new region.
    virtual void finalize(ScheduleDAGPostRA *DAG) = 0;

    /// Notify this strategy that all roots have been released (including those
    /// that depend on EntrySU or ExitSU).
    virtual void registerRoots() {}

    /// Pick the next node to schedule. Return true if there is another node
    /// to be scheduled. If SU is null, a NOOP is inserted. If IsBundled is
    /// set to true, the instruction is bundled with the previously scheduled
    /// instruction. If IsTopNode is true, the node is scheduled at the top,
    /// otherwise at the bottom of the region.
    virtual bool pickNode(SUnit *&SU, bool &IsTopNode, bool &IsBundled) = 0;

    /// \brief Scheduler callback to notify that a new subtree is scheduled.
    virtual void scheduleTree(unsigned SubtreeID) {}

    /// Notify PostRASchedStrategy that ScheduleDAGPostRA has scheduled an
    /// instruction and updated scheduled/remaining flags in the DAG nodes.
    virtual void schedNode(SUnit *SU, bool IsTopNode, bool IsBundled) = 0;

    /// Notify PostRASchedStrategy that a NOOP has been scheduled.
    virtual void schedNoop(bool IsTopNode) {}

    /// When all predecessor dependencies have been resolved, free this node for
    /// top-down scheduling.
    virtual void releaseTopNode(SUnit *SU) = 0;

    /// When all successor dependencies have been resolved, free this node for
    /// bottom-up scheduling.
    virtual void releaseBottomNode(SUnit *SU) = 0;

  };

  /// Mutate the DAG as a postpass after normal DAG building.
  class ScheduleDAGPostRAMutation {
  public:
    virtual ~ScheduleDAGPostRAMutation() {}

    virtual void apply(ScheduleDAGPostRA *DAG) = 0;
  };

  class ScheduleDAGPostRA : public ScheduleDAGInstrs {

    typedef std::vector<ScheduleDAGPostRAMutation*> MutationList;

    /// SchedImpl - The schedule strategy to use.
    ///
    PostRASchedStrategy *SchedImpl;

    /// Information about DAG subtrees. If DFSResult is NULL, then SchedulerTrees
    /// will be empty.
    SchedDFSResult *DFSResult;
    BitVector ScheduledTrees;

    /// Topo - A topological ordering for SUnits which permits fast IsReachable
    /// and similar queries.
    ScheduleDAGTopologicalSort Topo;

    /// AntiDepBreak - Anti-dependence breaking object, or NULL if none
    AntiDepBreaker *AntiDepBreak;

    /// AA - AliasAnalysis for making memory reference queries.
    AliasAnalysis *AA;

    /// Mutations to postprocess the DAGs
    MutationList Mutations;

    /// The top of the unscheduled zone.
    MachineBasicBlock::iterator CurrentTop;

    /// The bottom of the unscheduled zone.
    MachineBasicBlock::iterator CurrentBottom;

    // Vector of instructions assigned to the current packet.
    SmallVector<MachineInstr*, 4> TopBundleMIs;
    SmallVector<MachineInstr*, 4> BottomBundleMIs;

    /// LiveRegs - true if the register is live.
    BitVector LiveRegs;

  public:
    ScheduleDAGPostRA(PostRASchedContext *C, PostRASchedStrategy *S);

    ~ScheduleDAGPostRA();

    void addMutation(ScheduleDAGPostRAMutation *M);

    /// \brief True if an edge can be added from PredSU to SuccSU without creating
    /// a cycle.
    bool canAddEdge(SUnit *SuccSU, SUnit *PredSU);

    /// \brief Add a DAG edge to the given SU with the given predecessor
    /// dependence data.
    ///
    /// \returns true if the edge may be added without creating a cycle OR if an
    /// equivalent edge already existed (false indicates failure).
    bool addEdge(SUnit *SuccSU, const SDep &PredDep);

    MachineBasicBlock::iterator top() const { return CurrentTop; }
    MachineBasicBlock::iterator bottom() const { return CurrentBottom; }

    /// isSchedulingBoundary - Test if the given instruction should be
    /// considered a scheduling boundary.
    virtual bool isSchedulingBoundary(const MachineInstr *MI,
                                                const MachineBasicBlock *MBB,
                                                const MachineFunction &MF);

    /// canHandleTerminators - Return true if this strategy schedules terminator
    /// instructions properly.
    virtual bool canHandleTerminators() { return CanHandleTerminators; }

    /// startBlock - Initialize register live-range state for scheduling in
    /// this block.
    ///
    virtual void startBlock(MachineBasicBlock *BB);

    /// Initialize the scheduler state for the next scheduling region.
    virtual void enterRegion(MachineBasicBlock *bb,
                             MachineBasicBlock::iterator begin,
                             MachineBasicBlock::iterator end,
                             unsigned endcount);

    /// Schedule - Schedule the instruction range using list scheduling.
    ///
    virtual void schedule();

    /// Change the position of an instruction within the basic block and update
    /// live ranges and region boundary iterators.
    void moveInstruction(MachineInstr *MI, MachineBasicBlock::iterator InsertPos);

    /// finishBlock - Clean up register live-range state.
    ///
    virtual void finishBlock();

    virtual void finalizeSchedule();

    /// Compute a DFSResult after DAG building is complete, and before any
    /// queue comparisons.
    void computeDFSResult();

    /// Return a non-null DFS result if the scheduling strategy initialized it.
    const SchedDFSResult *getDFSResult() const { return DFSResult; }

    BitVector &getScheduledTrees() { return ScheduledTrees; }

    void viewGraph(const Twine &Name, const Twine &Title) LLVM_OVERRIDE;
    void viewGraph() LLVM_OVERRIDE;

  protected:

    /// Apply each ScheduleDAGMutation step in order. This allows different
    /// instances of ScheduleDAGPostRA to perform custom DAG postprocessing.
    void postprocessDAG();

    /// Release ExitSU predecessors and setup scheduler queues.
    void initQueues(ArrayRef<SUnit*> TopRoots, ArrayRef<SUnit*> BotRoots);

    /// Move an instruction and update register pressure.
    void scheduleMI(SUnit *SU, bool IsTopNode, bool IsBundled);

    void finishTopBundle();

    void finishBottomBundle();

    /// Update scheduler DAG and queues after scheduling an instruction.
    void updateQueues(SUnit *SU, bool IsTopNode, bool IsBundled);

    /// Reinsert debug_values recorded in ScheduleDAGInstrs::DbgValues.
    void placeDebugValues();

    void findRootsAndBiasEdges(SmallVectorImpl<SUnit*> &TopRoots,
                               SmallVectorImpl<SUnit*> &BotRoots);

    void releaseSucc(SUnit *SU, SDep *SuccEdge);
    void releaseSuccessors(SUnit *SU);
    void releasePred(SUnit *SU, SDep *PredEdge);
    void releasePredecessors(SUnit *SU);


    // Code for Anti Dependency Breaker kill tracking

    void startBlockForKills(MachineBasicBlock *BB);

    // ToggleKillFlag - Toggle a register operand kill flag. Other
    // adjustments may be made to the instruction if necessary. Return
    // true if the operand has been deleted, false if not.
    bool toggleKillFlag(MachineInstr *MI, MachineOperand &MO);

    /// FixupKills - Fix register kill flags that have been made
    /// invalid due to scheduling
    ///
    void fixupKills(MachineBasicBlock *MBB);


    void dumpSchedule() const;
  };
}

#endif
