//===-- PatmosStackCacheAnalysis.h - Analysis of the stack-cache usage. -=====//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Analysis results from the stack cache analysis.
// This is a dummy pass that holds analysis results when SCA runs. Other passes
// can require it without affecting the analysis behavior itself.
// XXX There is probably a cleaner way to handle this...
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/MachineModulePass.h"

namespace llvm {
class PatmosStackCacheAnalysisInfo : public MachineModulePass {
  bool Valid;

public:
  PatmosStackCacheAnalysisInfo(const TargetMachine &TM) : MachineModulePass(ID),
    Valid(false) {
      initializePatmosStackCacheAnalysisInfoPass(*PassRegistry::getPassRegistry());
    }

  PatmosStackCacheAnalysisInfo()
    : MachineModulePass(ID) {
    llvm_unreachable("should not be implicitly constructed");
  }

  virtual bool runOnMachineModule(const Module &M) { return false; }
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
    ModulePass::getAnalysisUsage(AU);
  }

  // the analysis info (pass) will always be available, with isValid() we can
  // tell whether the analysis was run
  void setValid() { Valid = true; }
  bool isValid() const { return Valid; }

  typedef std::map<const MachineInstr*, unsigned int> FillSpillCounts;
  FillSpillCounts Ensures;

  static char ID; // Pass identification, replacement for typeid
};
} // End llvm namespace
