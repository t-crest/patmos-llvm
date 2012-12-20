//===-- PatmosPreserveFunction.cpp - Preserve MachineFunctionAnalysis ========//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple pass setting the preserve flag of the MachineFunctionAnalysis.
//
//===----------------------------------------------------------------------===//


#include "Patmos.h"
#include "llvm/CodeGen/MachineFunctionAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"

using namespace llvm;

namespace llvm {
  /// Pass to set the preserve flag of the MachineFunctionAnalysis.
  /// \see MachineFunctionAnalysis
  class PatmosPreserveFunction : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;
  public:
    PatmosPreserveFunction() : MachineFunctionPass(ID) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Preserve Function";
    }

    /// runOnMachineFunction - Run the function splitter on the given function.
    bool runOnMachineFunction(MachineFunction &MF) {
      getAnalysis<MachineFunctionAnalysis>().preserveMF();
      return true;
    }
  };

  char PatmosPreserveFunction::ID = 0;
}

/// createPatmosPreserveFunctionPass - Returns a new PatmosPreserveFunction
/// \see PatmosPreserveFunction
FunctionPass *llvm::createPatmosPreserveFunctionPass() {
  return new PatmosPreserveFunction();
}
