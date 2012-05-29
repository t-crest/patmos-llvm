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


namespace llvm {
  class PatmosTargetMachine;
  class FunctionPass;
  class formatted_raw_ostream;

  FunctionPass *createPatmosISelDag(PatmosTargetMachine &TM);

  //FunctionPass *createPatmosBranchSelectionPass();

} // end namespace llvm;

#endif // _LLVM_TARGET_PATMOS_H_
