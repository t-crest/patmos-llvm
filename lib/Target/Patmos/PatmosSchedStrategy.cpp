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
#include "llvm/ADT/BitVector.h"
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

bool ILPOrder::operator()(const SUnit *A, const SUnit *B) const {
  unsigned SchedTreeA = DFSResult->getSubtreeID(A);
  unsigned SchedTreeB = DFSResult->getSubtreeID(B);
  if (SchedTreeA != SchedTreeB) {
    // Unscheduled trees have lower priority.
    if (ScheduledTrees->test(SchedTreeA) != ScheduledTrees->test(SchedTreeB))
      return ScheduledTrees->test(SchedTreeB);

    // Trees with shallower connections have have lower priority.
    if (DFSResult->getSubtreeLevel(SchedTreeA)
        != DFSResult->getSubtreeLevel(SchedTreeB)) {
      return DFSResult->getSubtreeLevel(SchedTreeA)
        < DFSResult->getSubtreeLevel(SchedTreeB);
    }
  }
  if (MaximizeILP)
    return DFSResult->getILP(A) < DFSResult->getILP(B);
  else
    return DFSResult->getILP(A) > DFSResult->getILP(B);
}


PatmosPostRASchedStrategy::PatmosPostRASchedStrategy(
                                            const PatmosTargetMachine &PTM)
: PII(*PTM.getInstrInfo()), DAG(0), Cmp(true)
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

void PatmosPostRASchedStrategy::initialize(ScheduleDAGPostRA *dag)
{
  DAG = dag;
  //AvailableQueue.initNodes(DAG->SUnits);

  // TODO remove barriers between loads/stores with different memory type
  // TODO remove any dependency between instructions with mutually exclusive
  //      predicates
  // TODO set latency of anti-dependencies to loads to -1 (?)
  // TODO GPRs are always bypassed, reduce latency of Data edges to ExitSU by 1
  // TODO RET and CALL have implicit deps on the return values and call
  //      arguments. Remove all those edges to schedule them into the delay slot
  //      if the registers are not actually used by CALL and RET

  DAG->computeDFSResult();
  Cmp.DFSResult = DAG->getDFSResult();
  Cmp.ScheduledTrees = &DAG->getScheduledTrees();
  ReadyQ.clear();
}

void PatmosPostRASchedStrategy::registerRoots()
{
  /*
  // Add all leaves to Available queue.
  for (unsigned i = 0, e = DAG->SUnits.size(); i != e; ++i) {
    // It is available if it has no predecessors.
    SUnit &SU = DAG->SUnits[i];
    if (!SU.NumPredsLeft && !SU.isAvailable) {
      AvailableQueue.push(&SU);
      SU.isAvailable = true;
    }
  }
  */

  // Restore the heap in ReadyQ with the updated DFS results.
  std::make_heap(ReadyQ.begin(), ReadyQ.end(), Cmp);
}

void PatmosPostRASchedStrategy::finalize(ScheduleDAGPostRA *DAG)
{
  //AvailableQueue.releaseState();
}

bool PatmosPostRASchedStrategy::pickNode(SUnit *&SU, bool &IsTopNode,
                                         bool &IsBundled)
{
  if (ReadyQ.empty()) return false;
  std::pop_heap(ReadyQ.begin(), ReadyQ.end(), Cmp);
  SU = ReadyQ.back();
  ReadyQ.pop_back();
  IsTopNode = false;
  DEBUG(dbgs() << "Pick node " << "SU(" << SU->NodeNum << ") "
        << " ILP: " << DAG->getDFSResult()->getILP(SU)
        << " Tree: " << DAG->getDFSResult()->getSubtreeID(SU) << " @"
        << DAG->getDFSResult()->getSubtreeLevel(
          DAG->getDFSResult()->getSubtreeID(SU)) << '\n'
        << "Scheduling " << *SU->getInstr());
  return true;
}

void PatmosPostRASchedStrategy::schedNode(SUnit *SU, bool IsTopNode,
                                          bool IsBundled)
{
  //AvailableQueue.scheduledNode(SU);
}

void PatmosPostRASchedStrategy::schedNoop(bool IsTopNode)
{

}

void PatmosPostRASchedStrategy::scheduleTree(unsigned SubtreeID) {
  std::make_heap(ReadyQ.begin(), ReadyQ.end(), Cmp);
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
  //PendingQueue.push_back(SU);

  ReadyQ.push_back(SU);
  std::push_heap(ReadyQ.begin(), ReadyQ.end(), Cmp);
}


