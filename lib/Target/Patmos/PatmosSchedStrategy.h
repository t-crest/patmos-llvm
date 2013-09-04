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

  class PatmosPostRASchedStrategy : public PostRASchedStrategy  {
  private:
    const PatmosInstrInfo &PII;

    /// Should we create bundles?
    bool EnableBundles;

    /// AvailableQueue - The priority queue to use for the available SUnits.
    ///
    LatencyPriorityQueue AvailableQueue;

    /// PendingQueue - This contains all of the instructions whose operands have
    /// been issued, but their results are not ready yet (due to the latency of
    /// the operation).  Once the operands becomes available, the instruction is
    /// added to the AvailableQueue.
    std::vector<SUnit*> PendingQueue;

  public:
    PatmosPostRASchedStrategy(const PatmosTargetMachine &PTM);
    virtual ~PatmosPostRASchedStrategy() {}

    /// isSchedulingBoundary - Test if the given instruction should be
    /// considered a scheduling boundary.
    virtual bool isSchedulingBoundary(const MachineInstr *MI,
                                                const MachineBasicBlock *MBB,
                                                const MachineFunction &MF);


    /// Initialize the strategy after building the DAG for a new region.
    virtual void initialize(ScheduleDAGPostRA *DAG);

    virtual void finalize(ScheduleDAGPostRA *DAG);

    /// Pick the next node to schedule, or return NULL. Set IsTopNode to true to
    /// schedule the node at the top of the unscheduled region. Otherwise it will
    /// be scheduled at the bottom.
    virtual SUnit *pickNode(bool &IsTopNode);

    /// Notify MachineSchedStrategy that ScheduleDAGMI has scheduled an
    /// instruction and updated scheduled/remaining flags in the DAG nodes.
    virtual void schedNode(SUnit *SU, bool IsTopNode);

    /// When all predecessor dependencies have been resolved, free this node for
    /// top-down scheduling.
    virtual void releaseTopNode(SUnit *SU);

    /// When all successor dependencies have been resolved, free this node for
    /// bottom-up scheduling.
    virtual void releaseBottomNode(SUnit *SU);

  private:
    void pickBundle();

  };

}

#endif
