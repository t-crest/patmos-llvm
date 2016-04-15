//===-- MachineFunctionAnalysis.cpp ---------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the definitions of the MachineFunctionAnalysis members.
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineFunctionInitializer.h"
using namespace llvm;

INITIALIZE_PASS(MachineFunctionAnalysis, "machinefunctionanalysis",
                "Machine Function Analysis", false, true)

char MachineFunctionAnalysis::ID = 0;

MachineFunctionAnalysis::MachineFunctionAnalysis()
    : FunctionPass(ID), TM(nullptr), MF(nullptr), MFInitializer(nullptr) {
  initializeMachineFunctionAnalysisPass(*PassRegistry::getPassRegistry());
}

MachineFunctionAnalysis::MachineFunctionAnalysis(
    const TargetMachine *tm, MachineFunctionInitializer *MFInitializer)
    : FunctionPass(ID), TM(tm), MF(nullptr), MFInitializer(MFInitializer) {
  initializeMachineFunctionAnalysisPass(*PassRegistry::getPassRegistry());
}

MachineFunctionAnalysis::~MachineFunctionAnalysis() {
  assert(!MF && "MachineFunctionAnalysis left initialized!");
}

void MachineFunctionAnalysis::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<MachineModuleInfo>();
}

bool MachineFunctionAnalysis::doInitialization(Module &M) {
  MachineModuleInfo *MMI = getAnalysisIfAvailable<MachineModuleInfo>();
  assert(MMI && "MMI not around yet??");
  MMI->setModule(&M);
  NextFnNum = 0;
  return false;
}


bool MachineFunctionAnalysis::runOnFunction(Function &F) {
  assert(!MF && "MachineFunctionAnalysis already initialized!");

  // Check whether a MachineFunction exists for F.
  MachineModuleInfo &MMI = getAnalysis<MachineModuleInfo>();
  MF = MMI.getMachineFunction(&F);

  // If function already exists and we want to restart, force creation
  if (MF && TM) {
    MMI.removeMachineFunction(&F);
    delete MF;
    MF = 0;
  }

  // No MachineFunction available? create one ...
  if (!MF) {
    if (TM) {
      MF = new MachineFunction(&F, *TM, NextFnNum++, MMI);
    }
    else if (!TM) {
      // Note: We could retrieve the TargetMachine from MMI and pass a flag to the
      // constructor to force (re-)creation of MFs (this must be set in
      // LLVMTargetMachine, otherwise JIT recompilation breaks).
      // However, we need to make sure functions are numbered properly in that
      // case, so we need to persist the counter as well or use the number
      // of persisted functions to calculate the function-number.
      llvm_unreachable(
         "MachineFunction has not been preserved. "
         "Make sure to use MachineModulePasses instead of ModulePasses");
    }
  }
  else {
    // Once we stored the MF, keep it that way. This is a workaround for the
    // problem that sometimes this pass is created on the fly and thus not
    // found by MachineModulePass.
    PreserveMF = true;
  }

  if (MFInitializer)
    MFInitializer->initializeMachineFunction(*MF);

  return false;
}

void MachineFunctionAnalysis::releaseMemory() {
  // Check whether a MachineFunction exists for F.
  MachineModuleInfo *MMI = getAnalysisIfAvailable<MachineModuleInfo>();

  if (PreserveMF && MMI && MF) {
    // Store the MachineFunction instead of destroying it,
    // but only if MachineFunctionAnalysis has been initialized before
    // It seems that this happens sometimes when this analysis is free'd
    // but not used before.
    MMI->putMachineFunction(MF, MF->getFunction());
  }
  else if (MF) {
    if (MMI) {
      // If we have a MachineModuleInfo, cleanup there as well.
      MMI->removeMachineFunction(MF->getFunction());
    }
    // Not preserving or not MMI, delete the machine function.
    delete MF;
  }
  MF = 0;
}
