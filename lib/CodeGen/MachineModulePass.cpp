//===-- MachineModulePass.cpp -------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the definitions of the MachineModulePass members.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/IVUsers.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionAliasAnalysis.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/StackProtector.h"
#include "llvm/IR/Module.h"

using namespace llvm;

void MachineModulePass::preparePassManager(PMStack &PMS) {
  assert(!PMS.empty() && "No pass manager found, should not happen.");

  MachineFunctionAnalysis *MFA = (MachineFunctionAnalysis*)
    PMS.top()->findAnalysisPass(&MachineFunctionAnalysis::ID, true);
  if (MFA) {
    MFA->preserveMF();
  }
  ModulePass::preparePassManager(PMS);
}

bool MachineModulePass::runOnModule(Module &M) {
  bool Changed = runOnMachineModule(M);
  return Changed;
}

void MachineModulePass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addPreserved<MachineFunctionAnalysis>();

  // MachineModulePass preserves all LLVM IR passes, but there's no
  // high-level way to express this. Instead, just list a bunch of
  // passes explicitly. This does not include setPreservesCFG,
  // because CodeGen overloads that to mean preserving the MachineBasicBlock
  // CFG in addition to the LLVM IR CFG.
  AU.addPreserved<BasicAAWrapperPass>();
  AU.addPreserved<DominanceFrontier>();
  AU.addPreserved<DominatorTreeWrapperPass>();
  AU.addPreserved<AAResultsWrapperPass>();
  AU.addPreserved<GlobalsAAWrapperPass>();
  AU.addPreserved<IVUsers>();
  AU.addPreserved<LoopInfoWrapperPass>();
  AU.addPreserved<MemoryDependenceAnalysis>();
  AU.addPreserved<ScalarEvolutionWrapperPass>();
  AU.addPreserved<SCEVAAWrapperPass>();
  AU.addPreserved<StackProtector>();

  ModulePass::getAnalysisUsage(AU);
}
