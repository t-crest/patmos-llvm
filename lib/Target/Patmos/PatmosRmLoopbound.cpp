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

// anonymous namespace
namespace {

  class PatmosNakedReserver : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;
    const PatmosInstrInfo *TII;


  public:
    PatmosNakedReserver(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())) {
      (void) TM;
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos reserve regs in naked functions";
    }

    virtual bool runOnMachineFunction(MachineFunction &MF) {
      unsigned ReserveList[] = {
        Patmos::R3,
        Patmos::R4,
        Patmos::R5,
        Patmos::R6,
        Patmos::R7,
        Patmos::R8,
        Patmos::R9,
        Patmos::R10
      };
      bool changed = false;
      if (!MF.getFunction()->hasFnAttribute(Attribute::Naked))
        return false;

      for (MachineFunction::iterator FI = MF.begin(), FE = MF.end();
           FI != FE; ++FI) {
        MachineBasicBlock &MBB = *FI;
        for (unsigned i = 0; i < sizeof(ReserveList) / sizeof(unsigned); ++i)
          MBB.addLiveIn(ReserveList[i]);

        for (MachineBasicBlock::iterator MI = FI->begin(), ME = FI->end();
            MI != ME; ++MI) {
          if (MI->isReturn()) {
            for (unsigned i = 0; i < sizeof(ReserveList) / sizeof(unsigned);
                 ++i) {
              MI->addOperand(
                MachineOperand::CreateReg(ReserveList[i], false, true));
            }
          }
        }
      }
      changed = true;

      return changed;
    }
  };

  char PatmosNakedReserver::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosNakedReserver - Returns a new PatmosNakedReserver
/// \see PatmosNakedReserver
FunctionPass *llvm::createPatmosNakedReserver(const PatmosTargetMachine &tm) {
  return new PatmosNakedReserver(tm);
}


