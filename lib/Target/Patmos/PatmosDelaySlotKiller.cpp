//===-- PatmosDelaySlotKiller.cpp - Patmos delay slot killer --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a simple local pass that attempts to kill delay slots of
// control flow changing instructions (call, return, branches). If all
// instructions are NOPs, the instruction is replaced with a
// non-delayed control-flow instruction.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "delay-slot-killer"
#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"


using namespace llvm;

STATISTIC( KilledSlots, "Number of eliminated delay slots");

namespace {

  class PatmosDelaySlotKiller : public MachineFunctionPass {
  private:
    static char ID;
  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    PatmosTargetMachine &TM;
    const PatmosInstrInfo *TII;

    PatmosDelaySlotKiller(PatmosTargetMachine &tm)
      : MachineFunctionPass(ID), TM(tm),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())) { }

    virtual const char *getPassName() const {
      return "Patmos Delay Slot Killer";
    }

    bool runOnMachineFunction(MachineFunction &F) {
      bool Changed = false;
      if (TM.getSubtargetImpl()->getCFLType() != PatmosSubtarget::CFL_DELAYED) {
        DEBUG( dbgs() << "\n[DelaySlotKiller] "
               << F.getFunction()->getName() << "\n" );

        for (MachineFunction::iterator FI = F.begin(), FE = F.end();
             FI != FE; ++FI)
          Changed |= killDelaySlots(*FI);
      }
      return Changed;
    }

  protected:
    /// killDelaySlots - Kill delay slots for the given basic block.
    ///
    bool killDelaySlots(MachineBasicBlock &MBB);

  };

  char PatmosDelaySlotKiller::ID = 0;
} // end of anonymous namespace

/// createPatmosDelaySlotKillerPass - Returns a pass that kills delay
/// slots in Patmos MachineFunctions
///
FunctionPass *llvm::createPatmosDelaySlotKillerPass(PatmosTargetMachine &tm) {
  return new PatmosDelaySlotKiller(tm);
}


bool PatmosDelaySlotKiller::killDelaySlots(MachineBasicBlock &MBB) {
  bool Changed = false;

  DEBUG( dbgs() << "Killing slots in BB#" << MBB.getNumber()
                << " (" << MBB.getFullName() << ")\n" );

  // consider the basic block from top to bottom
  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {
    // Control-flow instructions ("proper" delay slots)
    if (I->hasDelaySlot()) {
      assert( ( I->isCall() || I->isReturn() || I->isBranch() )
              && "Unexpected instruction with delay slot.");

      MachineBasicBlock::instr_iterator MI = *I;
      if (I->isBundle()) { ++MI; }

      unsigned Opcode = MI->getOpcode();

      if (Opcode == Patmos::BR ||
          Opcode == Patmos::BRu ||
          Opcode == Patmos::BRR ||
          Opcode == Patmos::BRRu ||
          Opcode == Patmos::BRT ||
          Opcode == Patmos::BRTu ||
          Opcode == Patmos::BRCF ||
          Opcode == Patmos::BRCFu ||
          Opcode == Patmos::BRCFR ||
          Opcode == Patmos::BRCFRu ||
          Opcode == Patmos::BRCFT ||
          Opcode == Patmos::BRCFTu ||
          Opcode == Patmos::CALL ||
          Opcode == Patmos::CALLR ||
          Opcode == Patmos::RET ||
          Opcode == Patmos::XRET) {

        bool onlyNops = true;
        unsigned maxCount = TM.getSubtargetImpl()->getDelaySlotCycles(&*I);
        unsigned count = 0;
        for (MachineBasicBlock::iterator K = llvm::next(I), E = MBB.end();
             K != E && count < maxCount; ++K, ++count) {
          TII->skipPseudos(MBB, K);
          if (K->getOpcode() != Patmos::NOP) {
            onlyNops = false;
          }
        }
        if (onlyNops) {
          unsigned NewOpcode = 0;
          switch(Opcode) {
          case Patmos::BR:     NewOpcode = Patmos::BRND; break;
          case Patmos::BRu:    NewOpcode = Patmos::BRNDu; break;
          case Patmos::BRR:    NewOpcode = Patmos::BRRND; break;
          case Patmos::BRRu:   NewOpcode = Patmos::BRRNDu; break;
          case Patmos::BRT:    NewOpcode = Patmos::BRTND; break;
          case Patmos::BRTu:   NewOpcode = Patmos::BRTNDu; break;
          case Patmos::BRCF:   NewOpcode = Patmos::BRCFND; break;
          case Patmos::BRCFu:  NewOpcode = Patmos::BRCFNDu; break;
          case Patmos::BRCFR:  NewOpcode = Patmos::BRCFRND; break;
          case Patmos::BRCFRu: NewOpcode = Patmos::BRCFRNDu; break;
          case Patmos::BRCFT:  NewOpcode = Patmos::BRCFTND; break;
          case Patmos::BRCFTu: NewOpcode = Patmos::BRCFTNDu; break;
          case Patmos::CALL:   NewOpcode = Patmos::CALLND; break;
          case Patmos::CALLR:  NewOpcode = Patmos::CALLRND; break;
          case Patmos::RET:    NewOpcode = Patmos::RETND; break;
          case Patmos::XRET:   NewOpcode = Patmos::XRETND; break;
          }
          const MCInstrDesc &nonDelayed = TII->get(NewOpcode);
          MI->setDesc(nonDelayed);

          unsigned killCount = 0;
          MachineBasicBlock::iterator K = llvm::next(I);
          for (MachineBasicBlock::iterator E = MBB.end();
               K != E && killCount < count; ++K, ++killCount) {
            TII->skipPseudos(MBB, K);
            KilledSlots++;
          }
          MBB.erase(llvm::next(I), K);
        }
      }
      Changed = true; // pass result
    }
  }
  return Changed;
}
