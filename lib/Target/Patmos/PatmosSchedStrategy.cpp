//===----- SchedulePostRAList.cpp - list scheduler ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// PatmosPostRASchedStrategy implements the scheduling strategy for the post-RA
// scheduler. 
//
// TODO merge this somehow with the pre-RA MachineSchedStrategy?
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "post-RA-sched"
#include "PatmosSchedStrategy.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/LatencyPriorityQueue.h"
#include "llvm/CodeGen/ScheduleDFS.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"
using namespace llvm;

PatmosPostRASchedStrategy::PatmosPostRASchedStrategy(
                                            const PatmosTargetMachine &PTM)
: PII(*PTM.getInstrInfo())
{
  EnableBundles = PTM.getSubtargetImpl()->enableBundling(PTM.getOptLevel());
}

bool PatmosPostRASchedStrategy::isSchedulingBoundary(const MachineInstr *MI,
                                            const MachineBasicBlock *MBB,
                                            const MachineFunction &MF)
{
  if (MI->isDebugValue())
    return false;

  // Terminators and labels can't be scheduled around.
  if (MI->getDesc().isTerminator() || MI->isLabel())
    return true;

  // Do not schedule over inline asm (?)
  if (MI->isInlineAsm())
    return true;

  // All CFL instructions are boundaries, we only handle one CFL per region.
  return MI->isBarrier() || MI->isBranch() || MI->isCall() || MI->isReturn();
}

void PatmosPostRASchedStrategy::initialize(ScheduleDAGPostRA *DAG)
{
  AvailableQueue.initNodes(DAG->SUnits);
}

void PatmosPostRASchedStrategy::finalize(ScheduleDAGPostRA *DAG)
{
  AvailableQueue.releaseState();
}

SUnit *PatmosPostRASchedStrategy::pickNode(bool &IsTopNode)
{
  return 0;
}

void PatmosPostRASchedStrategy::schedNode(SUnit *SU, bool IsTopNode)
{

}

void PatmosPostRASchedStrategy::releaseTopNode(SUnit *SU)
{
  // Nothing to be done here, we are only scheduling bottom up. Entry nodes
  // are released nevertheless.
}

void PatmosPostRASchedStrategy::releaseBottomNode(SUnit *SU)
{
  // Standard scheduler algorithms will recompute the depth of the successor
  // here as such:
  //   SuccSU->setDepthToAtLeast(SU->getDepth() + SuccEdge->getLatency());
  //
  // However, we lazily compute node depth instead. Note that
  // ScheduleNodeTopDown has already updated the depth of this node which causes
  // all descendents to be marked dirty. Setting the successor depth explicitly
  // here would cause depth to be recomputed for all its ancestors. If the
  // successor is not yet ready (because of a transitively redundant edge) then
  // this causes depth computation to be quadratic in the size of the DAG.

  // If all the node's predecessors are scheduled, this node is ready
  // to be scheduled. Ignore the special ExitSU node.
  PendingQueue.push_back(SU);
}


