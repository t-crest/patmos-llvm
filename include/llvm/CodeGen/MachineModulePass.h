//===-- MachineModulePass.h - Pass for MachineModules --------*-C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the MachineModulePass class.  MachineModulePass's are
// just ModulePass's, except they operate on machine code as part of a code
// generator.  Because they operate on machine code, not the LLVM
// representation, MachineModulePass's are not allowed to modify the LLVM
// representation.  Due to this limitation, the MachineModulePass class takes
// care of declaring that no LLVM passes are invalidated.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_MACHINE_MODULE_PASS_H
#define LLVM_CODEGEN_MACHINE_MODULE_PASS_H

#include "llvm/Pass.h"

namespace llvm {

/// MachineModulePass - This class adapts the ModulePass interface to
/// allow convenient creation of passes that operate on the MachineModule
/// representation. Instead of overriding runOnModule, subclasses
/// override runOnMachineModule.
class MachineModulePass : public ModulePass {
protected:
  explicit MachineModulePass(char &ID) : ModulePass(ID) {}

  void preparePassManager(PMStack &) override;

public:
  /// runOnMachineModule - This method must be overloaded to perform the
  /// desired machine code transformation or analysis.
  ///
  virtual bool runOnMachineModule(const Module &M) = 0;

  /// getAnalysisUsage - Subclasses that override getAnalysisUsage
  /// must call this.
  ///
  void getAnalysisUsage(AnalysisUsage &AU) const override;

private:
  virtual bool runOnModule(Module &F);
};

} // End llvm namespace

#endif
