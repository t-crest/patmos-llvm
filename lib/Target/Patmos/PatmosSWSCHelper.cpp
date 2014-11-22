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
#include "PatmosSWSCHelper.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"


using namespace llvm;

void llvm::ReserveRegsSWSC(const MachineFunction &MF, BitVector &Reserved) {
  // reserve two regs for stack-top, mem-top
  Reserved.set(Patmos::R19);
  Reserved.set(Patmos::R20);

  // mutually exlusive use of pred bit 7 avoids having to save/restore S0
  if (MF.getName().startswith("_sc_")) {
    Reserved.set(Patmos::P1);
    Reserved.set(Patmos::P2);
    Reserved.set(Patmos::P3);
    Reserved.set(Patmos::P4);
    Reserved.set(Patmos::P5);
    Reserved.set(Patmos::P6);
  } else {
    Reserved.set(Patmos::P7);
  }

  // preserve argument regs in _sc_funcs (based on function attribute)
  if (MF.getFunction()->hasFnAttribute("patmos-sc-func")) {
    Reserved.set(Patmos::R3);
    Reserved.set(Patmos::R4);
    Reserved.set(Patmos::R5);
    Reserved.set(Patmos::R6);
    Reserved.set(Patmos::R7);
    Reserved.set(Patmos::R8);
    Reserved.set(Patmos::R9);
    Reserved.set(Patmos::R10);
    DEBUG(dbgs() << "LLC-SWSC-Helper: "
      << "arg regs reserved in " << MF.getFunction()->getName() << ".\n");
  }

  // preserve return regs when we have to (ensure & free)
  if (MF.getFunction()->hasFnAttribute("patmos-preserve-ret")) {
    Reserved.set(Patmos::R1);
    Reserved.set(Patmos::R2);
    DEBUG(dbgs() << "LLC-SWSC-Helper: "
      << "ret regs reserved in " << MF.getFunction()->getName() << ".\n");
  }
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
#if 0
      // Patmos return regs
      unsigned RetReserveList[] = {
        Patmos::R1,
        Patmos::R2
      };

      // Patmos arg/scratch regs (r9, r10 have special use)
      unsigned TmpReserveList[] = {
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
      if (! (MF.getFunction()->hasFnAttribute("patmos-preserve-ret") ||
             MF.getFunction()->hasFnAttribute("patmos-sc-func")))
        return false;


      if (!MF.getFunction()->hasFnAttribute(Attribute::Naked))
        llvm_unreachable("reserving regs in non-naked function");

      // a vector to collect the regs to be reserved
      std::vector<unsigned> Reserved;

      unsigned RetSize = sizeof(RetReserveList) / sizeof(unsigned);
      unsigned TmpSize = sizeof(TmpReserveList) / sizeof(unsigned);

      // add returns regs based on function attribute
      if (MF.getFunction()->hasFnAttribute("patmos-preserve-ret"))
        Reserved.insert(Reserved.end(), &RetReserveList[0],
                        &RetReserveList[RetSize]);

      // add scratch regs based on function attribute
      if (MF.getFunction()->hasFnAttribute("patmos-sc-func"))
        Reserved.insert(Reserved.end(), &TmpReserveList[0],
                        &TmpReserveList[TmpSize]);

      dbgs() << "LLC-SWSC-Helper: " << Reserved.size()
        << " reg(s) reserved in " << MF.getFunction()->getName() << ".\n";

      // reserve by marking regs live-in and still live in all 'return's
      for (MachineFunction::iterator FI = MF.begin(), FE = MF.end();
           FI != FE; ++FI) {
        MachineBasicBlock &MBB = *FI;
        for (unsigned i = 0, e = Reserved.size(); i < e; ++i)
          MBB.addLiveIn(Reserved[i]);

        for (MachineBasicBlock::iterator MI = FI->begin(), ME = FI->end();
            MI != ME; ++MI) {
          if (MI->isReturn()) {
            for (unsigned i = 0, e = Reserved.size(); i < e; ++i) {
              MI->addOperand(
                MachineOperand::CreateReg(Reserved[i], false, true));
            }
          }
        }
      }
      changed = true;

      return changed;
#endif
    return false;
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


