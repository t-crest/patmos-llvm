//===-- PatmosHazardRecognizer.h - Patmos Hazard Recognizers ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines hazard recognizers for scheduling Patmos functions.
//
//===----------------------------------------------------------------------===//

#ifndef PATMOSHAZARDRECOGNIZER_H
#define PATMOSHAZARDRECOGNIZER_H

#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/ScheduleHazardRecognizer.h"

namespace llvm {

/// PatmosHazardRecognizer handles special constraints that are not expressed in
/// the scheduling itinerary.
class PatmosHazardRecognizer : public ScheduleHazardRecognizer {
private:
  // PatmosTargetMachine &PTM;

public:
  PatmosHazardRecognizer(PatmosTargetMachine &tm,
                         const InstrItineraryData *ItinData,
                         const ScheduleDAG *DAG, bool PostRA);

  virtual HazardType getHazardType(SUnit *SU, int Stalls);
  virtual void Reset();
  virtual void EmitInstruction(SUnit *SU);
  virtual void AdvanceCycle();
  virtual void RecedeCycle();
};

} // end namespace llvm

#endif // PATMOSHAZARDRECOGNIZER_H
