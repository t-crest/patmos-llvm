//===-- PatmosRmLoopbound.cpp - Remove Loopbound pseudos ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass removes loopbound pseudos.
//
//===----------------------------------------------------------------------===//


#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineFunctionPass.h"


using namespace llvm;


// anonymous namespace
namespace {

  class PatmosRmLoopbound : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;


  public:
    PatmosRmLoopbound(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())) {
      (void) TM;
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Remove Loopbound Pseudos";
    }

    virtual bool runOnMachineFunction(MachineFunction &MF) {
      bool changed = false;
      for (MachineFunction::iterator FI = MF.begin(), FE = MF.end();
           FI != FE; ++FI) {
        for (MachineBasicBlock::iterator MI = FI->begin(), ME = FI->end();
            MI != ME;) {
          MachineInstr *M = MI;
          switch (M->getOpcode()) {
            case Patmos::PSEUDO_LOOPBOUND:
              ++MI;
              M->eraseFromParent();
              changed = true;
              continue;
            default: ;//NOP
          }
          ++MI;
        }
      }
      return changed;
    }
  };

  char PatmosRmLoopbound::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosRmLoopboundPass - Returns a new PatmosRmLoopbound
/// \see PatmosRmLoopbound
FunctionPass *llvm::createPatmosRmLoopboundPass(const PatmosTargetMachine &tm) {
  return new PatmosRmLoopbound(tm);
}


