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
using namespace llvm;

char MachineFunctionAnalysis::ID = 0;

MachineFunctionAnalysis::MachineFunctionAnalysis(const TargetMachine &tm) :
  FunctionPass(ID), TM(tm), MF(0), PreserveMF(false) {
  initializeMachineModuleInfoPass(*PassRegistry::getPassRegistry());
}

MachineFunctionAnalysis::~MachineFunctionAnalysis() {
  releaseMemory();
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

  // check whether a MachineFunction exists for F.
  MachineModuleInfo &MMI = getAnalysis<MachineModuleInfo>();
  MF = MMI.getMachineFunction(&F);

  // no MachineFunction available? create one ...
  if (!MF) {
    MF = new MachineFunction(&F, TM, NextFnNum++, MMI,
                             getAnalysisIfAvailable<GCModuleInfo>());
  }

  return false;
}

void MachineFunctionAnalysis::releaseMemory() {
  // if needed preserve the MachineFunction, otherwise free its entirely
  MachineModuleInfo &MMI = getAnalysis<MachineModuleInfo>();
  if (PreserveMF && MF) {
    MMI.putMachineFunction(MF, MF->getFunction());
  }
  else if (MF) {
    MMI.removeMachineFunction(MF->getFunction());
    delete MF;
  }

  MF = 0;
}
