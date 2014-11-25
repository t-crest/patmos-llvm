//===-- PatmosDelaySlotFiller.cpp - Patmos delay slot filler --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass ensures the alignment of functions, subfunctions and basic blocks.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "PatmosSubtarget.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

namespace {

  class PatmosEnsureAlignment : public MachineFunctionPass {
  private:
    unsigned SubfunctionAlignment;

    unsigned BasicBlockAlignment;

    static char ID;
  public:

    PatmosEnsureAlignment(PatmosTargetMachine &tm)
      : MachineFunctionPass(ID)
    {
      const PatmosSubtarget *PST = tm.getSubtargetImpl();

      SubfunctionAlignment = PST->getSubfunctionAlignment();
      BasicBlockAlignment = PST->getBasicBlockAlignment();
    }

    virtual const char *getPassName() const {
      return "Patmos Ensure Alignment";
    }

    bool runOnMachineFunction(MachineFunction &MF) {
      const PatmosMachineFunctionInfo *PMFI =
                                       MF.getInfo<PatmosMachineFunctionInfo>();

      bool Changed = false;

      if (!MF.getAlignment()) {
        MF.ensureAlignment(SubfunctionAlignment);
        Changed = true;
      }

      // insert NOPs after other instructions, if necessary
      for (MachineFunction::iterator i = MF.begin(), ie = MF.end();
           i != ie; ++i)
      {
        unsigned align;

        if (!i->getAlignment())
        {
          if (PMFI->isMethodCacheRegionEntry(i)) {
            align = SubfunctionAlignment;
          } else {
            align = BasicBlockAlignment;
          }

          i->setAlignment(align);
          Changed = true;
        }
      }

      return Changed;
    }
  };

  char PatmosEnsureAlignment::ID = 0;
} // end of anonymous namespace

FunctionPass *llvm::createPatmosEnsureAlignmentPass(PatmosTargetMachine &tm) {
  return new PatmosEnsureAlignment(tm);
}

