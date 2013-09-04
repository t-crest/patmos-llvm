//===-- PatmosMachineScheduler.h - Custom Patmos MI scheduler.      ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Custom Patmos MI scheduler.
//
//===----------------------------------------------------------------------===//

#ifndef PATMOSMASCHINESCHEDULER_H
#define PATMOSMASCHINESCHEDULER_H

#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MachineScheduler.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/CodeGen/RegisterPressure.h"
#include "llvm/CodeGen/ResourcePriorityQueue.h"
#include "llvm/CodeGen/ScheduleDAGInstrs.h"
#include "llvm/CodeGen/ScheduleHazardRecognizer.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/PriorityQueue.h"

using namespace llvm;

namespace llvm {

// TODO share code with the PatmosPostRAScheduleDAG, use a new
// VLIWMachineSchedStrategy interface suited for VLIW scheduling (?)
class PatmosVLIWScheduler : public ScheduleDAGMI {
public:
  PatmosVLIWScheduler(MachineSchedContext *C, MachineSchedStrategy *S);
};



class PatmosVLIWSchedStrategy : public MachineSchedStrategy {

  const TargetSchedModel *SchedModel;
  const TargetRegisterInfo *TRI;

public:


  PatmosVLIWSchedStrategy():
    SchedModel(0), TRI(0) {}

  virtual void initialize(ScheduleDAGMI *dag);

  virtual SUnit *pickNode(bool &IsTopNode);

  virtual void schedNode(SUnit *SU, bool IsTopNode);

  virtual void releaseTopNode(SUnit *SU);

  virtual void releaseBottomNode(SUnit *SU);

};



} // namespace


#endif
