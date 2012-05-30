//===- PatmosSubtarget.cpp - Patmos Subtarget Information ---------*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Patmos specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "PatmosSubtarget.h"
#include "Patmos.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "PatmosGenSubtargetInfo.inc"

using namespace llvm;

PatmosSubtarget::PatmosSubtarget(const std::string &TT,
                                 const std::string &CPU,
                                 const std::string &FS) :
  PatmosGenSubtargetInfo(TT, CPU, FS) {
  std::string CPUName = "generic";

  // Parse features string.
  ParseSubtargetFeatures(CPUName, FS);

  //InstrItins = getInstrItineraryForCPU(CPUName);

  InstrItins.IssueWidth = 1;
}
