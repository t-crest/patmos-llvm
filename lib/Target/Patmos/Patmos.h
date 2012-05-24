//==-- Patmos.h - Top-level interface for Patmos representation --*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM Patmos backend.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_TARGET_PATMOS_H_
#define _LLVM_TARGET_PATMOS_H_

#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace PatmosCC {
  // Patmos specific condition code.
  enum CondCodes {
    COND_E  = 0,  // aka COND_Z
    COND_NE = 1,  // aka COND_NZ
    COND_HS = 2,  // aka COND_C
    COND_LO = 3,  // aka COND_NC
    COND_GE = 4,
    COND_L  = 5,

    COND_INVALID = -1
  };
}

namespace llvm {
  class PatmosTargetMachine;
  class FunctionPass;
  class formatted_raw_ostream;

  FunctionPass *createPatmosISelDag(PatmosTargetMachine &TM);

  //FunctionPass *createPatmosBranchSelectionPass();

} // end namespace llvm;

#endif // _LLVM_TARGET_PATMOS_H_
