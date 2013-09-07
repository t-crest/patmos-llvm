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
#include "PatmosRegisterInfo.h"
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

#include <algorithm>

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
: PTM(PTM), PII(*PTM.getInstrInfo()), PRI(PII.getPatmosRegisterInfo()),
  DAG(0), Cmp(true), CFL(NULL), DelaySlot(0),
  CurrCycle(0), EndBundle(false)
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

  CFL = NULL;

  // Find the branch/call/ret instruction if available
  for (std::vector<SUnit>::reverse_iterator it = DAG->SUnits.rbegin(),
       ie = DAG->SUnits.rend(); it != ie; it++)
  {
    MachineInstr *MI = it->getInstr();
    if (!MI) continue;
    if (isPatmosCFL(MI->getOpcode(), MI->getDesc().TSFlags)) {
      CFL = &*it;
      break;
    }
  }

  PatmosSubtarget *PST = PTM.getSubtargetImpl();

  DelaySlot = CFL ? PST->getCFLDelaySlotCycles(CFL->getInstr()) : 0;
  CurrCycle = 0;
  EndBundle = false;

  // Reduce latencies to the exit node.
  updateExitLatencies(dag->ExitSU);

  if (CFL) {
    // Add an artificial dep from CFL to exit for the delay slot
    SDep DelayDep(CFL, SDep::Artificial);
    DelayDep.setLatency(DelaySlot + 1);
    DelayDep.setMinLatency(DelaySlot + 1);
    DAG->ExitSU.addPred(DelayDep);

    CFL->isScheduleLow = true;

    // RET and CALL have implicit deps on the return values and call
    // arguments. Remove all those edges to schedule them into the delay slot
    // if the registers are not actually used by CALL and RET
    if (CFL->getInstr()->isReturn() || CFL->getInstr()->isCall())
      removeImplicitCFLDeps(*CFL);
  }

  // remove barriers between loads/stores with different memory type
  removeTypedMemBarriers();

  // remove any dependency between instructions with mutually exclusive
  // predicates
  removeExclusivePredDeps();

  // TODO set latency of anti-dependencies to loads to -1 (?)

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
  MachineInstr *MI = SU->getInstr();
  if (MI->isInlineAsm() ||
      getPatmosFormat(MI->getDesc().TSFlags) == PatmosII::FrmALUl)
  {
    assert(!IsBundled && "Trying to bundle ALUl or inline asm");
    EndBundle = true;
  }

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


/// Remove dependencies to a return or call due to implicit uses of the return
/// value registers, arguments or callee saved regs. Does not remove
// dependencies to return info registers.
void PatmosPostRASchedStrategy::removeImplicitCFLDeps(SUnit &SU)
{
  SmallVector<SDep*,2> RemoveDeps;

  for (SUnit::pred_iterator it = SU.Preds.begin(), ie = SU.Preds.end();
       it != ie; it++)
  {
    if (!it->getSUnit()) continue;
    // We only handle Data, Anti and Output deps here.
    if (it->getKind() == SDep::Order) continue;

    MachineInstr *MI = SU.getInstr();

    // Check if it is actually only an implicit use, not a normal operand
    bool IsImplicit = true;
    for (unsigned i = 0; i < MI->getNumOperands(); i++) {
      MachineOperand &MO = MI->getOperand(i);

      if (!MO.isReg()) continue;

      // Check if the register is actually used or defined by the instruction,
      // either explicit or via a special register
      if (!isExplicitCFLOperand(MI, MO)) continue;

      // MO is an used/defined operand, check if it is defined or used by the
      // predecessor
      if (it->getKind() == SDep::Data) {
        // .. easy for Deps, since we know the register.
        if (MO.getReg() == it->getReg()) {
          IsImplicit = false;
          break;
        }
      } else if (MO.isDef() && (!MO.isImplicit())) {
        // for Anti and Output dependency we need to check the registers of
        // the predecessor.
        MachineInstr *PredMI = it->getSUnit()->getInstr();

        for (unsigned j = 0; j < PredMI->getNumOperands(); j++) {
          MachineOperand &PredMO = PredMI->getOperand(i);
          if (PredMO.isReg() && PredMO.getReg() == MO.getReg()) {
            IsImplicit = false;
            break;
          }
        }
      }
    }

    if (IsImplicit) {
      RemoveDeps.push_back(&*it);
    }
  }

  // Remove found implicit deps, add deps to exit node
  while (!RemoveDeps.empty()) {
    SDep *Dep = RemoveDeps.back();
    RemoveDeps.pop_back();

    SDep ExitDep(Dep->getSUnit(), SDep::Artificial);
    ExitDep.setLatency( computeExitLatency(*Dep->getSUnit()) );

    SU.removePred(*Dep);
    DAG->ExitSU.addPred(ExitDep);
  }
}

void PatmosPostRASchedStrategy::updateExitLatencies(SUnit &ExitSU)
{
  for (SUnit::pred_iterator it = ExitSU.Preds.begin(), ie = ExitSU.Preds.end();
       it != ie; it++)
  {
    if (it->getLatency() < 1) continue;
    if (!it->isArtificial()) continue;

    SUnit *PredSU = it->getSUnit();
    if (!PredSU || !PredSU->getInstr()) continue;

    // NOTE: We could also implement Subtarget.adjustSchedDependency(), but this
    // way this is more integrated with the scheduling algorithm.

    it->setLatency( computeExitLatency(*PredSU) );
  }
}

/// Remove barrier and memory deps between instructions that access
/// different memory types and cannot alias.
void PatmosPostRASchedStrategy::removeTypedMemBarriers()
{

}

/// Remove all dependencies between instructions with mutually exclusive
/// predicates.
void PatmosPostRASchedStrategy::removeExclusivePredDeps()
{

}

bool PatmosPostRASchedStrategy::isExplicitCFLOperand(MachineInstr *MI,
                                                     MachineOperand &MO)
{
  if (!MO.isImplicit()) return true;

  if (MO.getReg() == Patmos::SRB || MO.getReg() == Patmos::SRO ||
      MO.getReg() == Patmos::SXB || MO.getReg() == Patmos::SXO)
  {
    return true;
  }

  return false;
}

unsigned PatmosPostRASchedStrategy::computeExitLatency(SUnit &SU) {
  MachineInstr *PredMI = SU.getInstr();
  if (!PredMI) return 0;

  // TODO we should actually look into the following region/MBBs and check
  // if they are already scheduled and if we actually need a latency > 1 on
  // loads.
  unsigned Latency = 0;

  for (unsigned i = 0; i < PredMI->getNumOperands(); i++) {
    MachineOperand &MO = PredMI->getOperand(i);
    if (!MO.isReg() || !MO.isDef()) continue;

    // Get the default latency as the write cycle of the operand.
    unsigned OpLatency = DAG->getSchedModel()->computeOperandLatency(PredMI,
                                                      i, NULL, 0, false);

    // Patmos specific: GPRs are always bypassed and read in cycle 1, so we can
    // reduce the latency of edges to ExitSU by 1.
    if (PRI.isRReg(MO.getReg()) && OpLatency > 0) {
      OpLatency--;
    }

    Latency = std::max(Latency, OpLatency);
  }

  return Latency;
}
