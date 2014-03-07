//===-- MachineFunctionAnalysis.h - Owner of MachineFunctions ----*-C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the MachineFunctionAnalysis class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_MACHINEFUNCTIONANALYSIS_H
#define LLVM_CODEGEN_MACHINEFUNCTIONANALYSIS_H

#include "llvm/Pass.h"

namespace llvm {

class TargetMachine;
class MachineFunction;
class TargetMachine;

/// MachineFunctionAnalysis - This class is a Pass that manages a
/// MachineFunction object.
struct MachineFunctionAnalysis : public FunctionPass {
private:
  /// TM - The TargetMachine used to create new MachineFunctions.
  /// If set, running this pass will create a new MachineFunction. If not
  /// set, then the MachineFunctions will be retrieved from the
  /// MachineModuleInfo pass.
  const TargetMachine *TM;

  MachineFunction *MF;

  /// PreserveMF - Preserve the MF after this pass is released in the
  /// MachineModuleInfo and (temporarily) transfer ownership to it.
  bool PreserveMF;

  unsigned NextFnNum;
public:
  static char ID;
  MachineFunctionAnalysis();
  MachineFunctionAnalysis(const TargetMachine &tm);
  ~MachineFunctionAnalysis();

  /// preserveMF - Indicate that the MachineFunction should be preserved even
  /// after this MachineFunctionAnalysis instance has been freed.
  void preserveMF() { PreserveMF = true; }

  MachineFunction &getMF() const { return *MF; }

  const char* getPassName() const override {
    return "Machine Function Analysis";
  }

private:
  bool doInitialization(Module &M) override;
  bool runOnFunction(Function &F) override;
  void releaseMemory() override;
  void getAnalysisUsage(AnalysisUsage &AU) const override;
};

} // End llvm namespace

#endif
