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
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"


using namespace llvm;

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
             MF.getFunction()->hasFnAttribute("patmos-preserve-tmp")))
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
      if (MF.getFunction()->hasFnAttribute("patmos-preserve-tmp"))
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


