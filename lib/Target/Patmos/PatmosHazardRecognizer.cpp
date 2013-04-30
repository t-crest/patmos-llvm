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


ScheduleHazardRecognizer::HazardType
PatmosHazardRecognizer::getHazardType(SUnit *SU, int Stalls) {

  return ScoreboardHazardRecognizer::getHazardType(SU, Stalls);
}

void PatmosHazardRecognizer::Reset() {

  ScoreboardHazardRecognizer::Reset();
}

void PatmosHazardRecognizer::EmitInstruction(SUnit *SU) {

  ScoreboardHazardRecognizer::EmitInstruction(SU);
}

void PatmosHazardRecognizer::AdvanceCycle() {

  ScoreboardHazardRecognizer::AdvanceCycle();
}

void PatmosHazardRecognizer::RecedeCycle() {

  ScoreboardHazardRecognizer::RecedeCycle();
}
