//==- PMLImport.h - Import PML information --------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass is used to import PML infos and provide them to LLVM passes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_PMLIMPORT_H
#define LLVM_CODEGEN_PMLIMPORT_H

#include "llvm/Pass.h"

namespace llvm {

  class MachineBasicBlock;
  class MachineFunction;
  class Module;

  class PMLImport : public ImmutablePass {
    virtual void anchor();

  public:
    static char ID;

    PMLImport() : ImmutablePass(ID) {
      PassRegistry &Registry = *PassRegistry::getPassRegistry();
      initializePMLImportPass(Registry);
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
    }

    virtual void initializePass();


    /// Check if any PML infos are actually available.
    bool isAvailable() const;

    /// TODO define a sane set of 'isAvailable' functions (?)
    bool isAvailable(const MachineFunction *MF) const;


    /// TODO define various getters to access infos including context, ..

    uint64_t getLocalWCET(const MachineBasicBlock *MBB) const;

    double getCriticality(const MachineBasicBlock *MBB) const;

  };

}

#endif
