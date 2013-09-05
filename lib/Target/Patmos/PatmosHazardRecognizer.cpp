//===-- PatmosHazardRecognizer.cpp - Patmos postra hazard recognizer ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PatmosHazardRecognizer.h"
#include "PatmosInstrInfo.h"
#include "PatmosRegisterInfo.h"
#include "PatmosSubtarget.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/ScheduleDAG.h"
#include "llvm/Target/TargetRegisterInfo.h"

using namespace llvm;

PatmosHazardRecognizer::PatmosHazardRecognizer(PatmosTargetMachine &tm,
                       const InstrItineraryData *ItinData,
                       const ScheduleDAG *DAG, bool PostRA)
  : ScheduleHazardRecognizer()
{
}


/// getHazardType - Return the hazard type of emitting this node.  There are
/// three possible results.  Either:
///  * NoHazard: it is legal to issue this instruction on this cycle.
///  * Hazard: issuing this instruction would stall the machine.  If some
///     other instruction is available, issue it first.
///  * NoopHazard: issuing this instruction would break the program.  If
///     some other instruction can be issued, do so, otherwise issue a noop.
ScheduleHazardRecognizer::HazardType
PatmosHazardRecognizer::getHazardType(SUnit *SU, int Stalls) {

  return NoHazard;
}

/// Reset - This callback is invoked when a new block of
/// instructions is about to be schedule. The hazard state should be
/// set to an initialized state.
void PatmosHazardRecognizer::Reset() {

}

/// EmitInstruction - This callback is invoked when an instruction is
/// emitted, to advance the hazard state.
void PatmosHazardRecognizer::EmitInstruction(SUnit *SU) {

}

/// AdvanceCycle - This callback is invoked whenever the next top-down
/// instruction to be scheduled cannot issue in the current cycle, either
/// because of latency or resource conflicts.  This should increment the
/// internal state of the hazard recognizer so that previously "Hazard"
/// instructions will now not be hazards.
void PatmosHazardRecognizer::AdvanceCycle() {

}

/// RecedeCycle - This callback is invoked whenever the next bottom-up
/// instruction to be scheduled cannot issue in the current cycle, either
/// because of latency or resource conflicts.
void PatmosHazardRecognizer::RecedeCycle() {

}
