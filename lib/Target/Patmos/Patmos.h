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
  class ModulePass;
  class formatted_raw_ostream;
  class PassRegistry;
  class PatmosSinglePathInfo;

  void initializePatmosCallGraphBuilderPass(PassRegistry&);

  FunctionPass *createPatmosISelDag(PatmosTargetMachine &TM);
  FunctionPass *createPatmosSPPredicatePass(const PatmosTargetMachine &tm,
                                            PatmosSinglePathInfo &pspi);
  FunctionPass *createPatmosSPReducePass(const PatmosTargetMachine &tm,
                                         PatmosSinglePathInfo &pspi);
  FunctionPass *createPatmosDelaySlotFillerPass(TargetMachine &tm);
  FunctionPass *createPatmosFunctionSplitterPass(TargetMachine &tm);
  FunctionPass *createPatmosExportPass(std::string& filename,
                                       PatmosTargetMachine &tm);

  FunctionPass *createPatmosPreserveFunctionPass();
  ModulePass *createPatmosCallGraphBuilder();
  ModulePass *createPatmosStackCacheAnalysis(const PatmosTargetMachine &tm);
} // end namespace llvm;

#endif // _LLVM_TARGET_PATMOS_H_
