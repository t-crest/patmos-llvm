//===- PatmosMachineScheduler.cpp - MI Scheduler for Patmos -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// MachineScheduler schedules machine instructions after phi elimination. It
// preserves LiveIntervals so it can be invoked before register allocation.
//
// TODO as of now, this has been copied without modification from Hexagon.
//      Check for any changes required, maybe merge with hexagon code??
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "misched"

#include "PatmosMachineScheduler.h"

#include <queue>

using namespace llvm;


PatmosVLIWScheduler::PatmosVLIWScheduler(MachineSchedContext *C,
                                         MachineSchedStrategy *S)
: ScheduleDAGMI(C, S)
{
}

void PatmosVLIWSchedStrategy::initialize(ScheduleDAGMI *dag) {

}

SUnit *PatmosVLIWSchedStrategy::pickNode(bool &IsTopNode) {
  return 0;
}

void PatmosVLIWSchedStrategy::schedNode(SUnit *SU, bool IsTopNode) {

}

void PatmosVLIWSchedStrategy::releaseTopNode(SUnit *SU) {

}

void PatmosVLIWSchedStrategy::releaseBottomNode(SUnit *SU) {

}


