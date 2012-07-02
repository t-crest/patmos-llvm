//===-- PatmosDelaySlotFiller.cpp - Patmos delay slot filler --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a simple local pass that attempts to fill delay slots with useful
// instructions. If no instructions can be moved into the delay slot, then a
// NOP is placed.
//
// TODO: For the time being, only NOPs are inserted.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "delay-slot-filler"
#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"

using namespace llvm;

STATISTIC(FilledSlots, "Number of delay slots filled");

static cl::opt<bool> DisableDelaySlotFiller(
  "disable-patmos-delay-filler",
  cl::init(false),
  cl::desc("Disable the Patmos delay slot filler."),
  cl::Hidden);

namespace {
  class PatmosDelaySlotFiller : public MachineFunctionPass {
  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    TargetMachine &TM;
    const TargetInstrInfo *TII;

    static char ID;
    PatmosDelaySlotFiller(TargetMachine &tm)
      : MachineFunctionPass(ID), TM(tm), TII(tm.getInstrInfo()) { }

    virtual const char *getPassName() const {
      return "Patmos Delay Slot Filler";
    }

    bool runOnMachineBasicBlock(MachineBasicBlock &MBB);
    bool runOnMachineFunction(MachineFunction &F) {
      bool Changed = false;
      for (MachineFunction::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI)
        Changed |= runOnMachineBasicBlock(*FI);
      return Changed;
    }

  private:
    bool isDelayFiller(MachineBasicBlock &MBB,
                       MachineBasicBlock::iterator candidate);

    void insertCallUses(MachineBasicBlock::iterator MI,
                        SmallSet<unsigned, 32>& RegUses);

    void insertDefsUses(MachineBasicBlock::iterator MI,
                        SmallSet<unsigned, 32>& RegDefs,
                        SmallSet<unsigned, 32>& RegUses);

    bool IsRegInSet(SmallSet<unsigned, 32>& RegSet,
                    unsigned Reg);

    bool delayHasHazard(MachineBasicBlock::iterator candidate,
                        bool &sawLoad, bool &sawStore,
                        SmallSet<unsigned, 32> &RegDefs,
                        SmallSet<unsigned, 32> &RegUses);

    MachineBasicBlock::iterator
    findDelayInstr(MachineBasicBlock &MBB, MachineBasicBlock::iterator slot);

    bool needsUnimp(MachineBasicBlock::iterator I, unsigned &StructSize);

  };
  char PatmosDelaySlotFiller::ID = 0;
} // end of anonymous namespace

/// createPatmosDelaySlotFillerPass - Returns a pass that fills in delay
/// slots in Patmos MachineFunctions
///
FunctionPass *llvm::createPatmosDelaySlotFillerPass(TargetMachine &tm) {
  return new PatmosDelaySlotFiller(tm);
}


/// runOnMachineBasicBlock - Fill in delay slots for the given basic block.
/// We assume there is only one delay slot per delayed instruction.
///
bool PatmosDelaySlotFiller::runOnMachineBasicBlock(MachineBasicBlock &MBB) {
  bool Changed = false;

  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {
    unsigned opc = I->getOpcode();
    // FIXME: This should eventally be handled in the scheduler.
    if (opc==Patmos::MUL || opc==Patmos::MULU) {
      MachineBasicBlock::iterator J = I;
      AddDefaultPred(BuildMI(MBB, ++J, I->getDebugLoc(), TII->get(Patmos::NOP)))
          .addImm(3);
    } else // END_FIXME
    if (I->hasDelaySlot()) {
      MachineBasicBlock::iterator D = MBB.end();
      MachineBasicBlock::iterator J = I;

      if (!DisableDelaySlotFiller)
        D = findDelayInstr(MBB, I);

      if (D == MBB.end()) {
        //FIXME several single NOPs vs. one multicycle nop
        AddDefaultPred(BuildMI(MBB, ++J, I->getDebugLoc(), TII->get(Patmos::NOP)))
          .addImm(0);
        AddDefaultPred(BuildMI(MBB,   J, I->getDebugLoc(), TII->get(Patmos::NOP)))
          .addImm(0);
      } else {
        MBB.splice(++J, &MBB, D);
      }

      ++FilledSlots;  // update statistics
      Changed = true; // pass result
    }
  }
  return Changed;
}



MachineBasicBlock::iterator
PatmosDelaySlotFiller::findDelayInstr(MachineBasicBlock &MBB,
                       MachineBasicBlock::iterator slot)
{
  SmallSet<unsigned, 32> RegDefs;
  SmallSet<unsigned, 32> RegUses;
  bool sawLoad = false;
  bool sawStore = false;

  MachineBasicBlock::iterator I = slot;


  //FIXME this has to be removed and the implementation completed
  return MBB.end();
  llvm_unreachable("findDelayInstr() needs implementation");

  if (slot->isReturn()
      || slot->isCall()
      || slot->isBranch()
      || slot->isIndirectBranch()
     ) return MBB.end();


  insertDefsUses(slot, RegDefs, RegUses);

  bool done = false;
  while (!done) {
    done = (I == MBB.begin());
    if (!done) --I;

    // skip debug value
    if (I->isDebugValue()) continue;

    if (I->hasUnmodeledSideEffects()
        || I->isInlineAsm()
        || I->isLabel()
        || I->hasDelaySlot()
        || isDelayFiller(MBB, I))
      break;

    if (delayHasHazard(I, sawLoad, sawStore, RegDefs, RegUses)) {
      insertDefsUses(I, RegDefs, RegUses);
      continue;
    }

    return I;
  }
  return MBB.end();
}

bool PatmosDelaySlotFiller::delayHasHazard(MachineBasicBlock::iterator candidate,
                            bool &sawLoad,
                            bool &sawStore,
                            SmallSet<unsigned, 32> &RegDefs,
                            SmallSet<unsigned, 32> &RegUses)
{

  if (candidate->isImplicitDef() || candidate->isKill())
    return true;

  if (candidate->mayLoad()) {
    sawLoad = true;
    if (sawStore)
      return true;
  }

  if (candidate->mayStore()) {
    if (sawStore)
      return true;
    sawStore = true;
    if (sawLoad)
      return true;
  }

  for (unsigned i = 0, e = candidate->getNumOperands(); i!= e; ++i) {
    const MachineOperand &MO = candidate->getOperand(i);
    if (!MO.isReg())
      continue; // skip

    unsigned Reg = MO.getReg();

    if (MO.isDef()) {
      //check whether Reg is defined or used before delay slot.
      if (IsRegInSet(RegDefs, Reg) || IsRegInSet(RegUses, Reg))
        return true;
    }
    if (MO.isUse()) {
      //check whether Reg is defined before delay slot.
      if (IsRegInSet(RegDefs, Reg))
        return true;
    }
  }
  return false;
}


//Insert Defs and Uses of MI into the sets RegDefs and RegUses.
void PatmosDelaySlotFiller::insertDefsUses(MachineBasicBlock::iterator MI,
                            SmallSet<unsigned, 32>& RegDefs,
                            SmallSet<unsigned, 32>& RegUses)
{
  for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    if (!MO.isReg())
      continue;

    unsigned Reg = MO.getReg();
    if (Reg == 0)
      continue;
    if (MO.isDef())
      RegDefs.insert(Reg);
    if (MO.isUse())
      RegUses.insert(Reg);
  }
}

//returns true if the Reg or its alias is in the RegSet.
bool PatmosDelaySlotFiller::IsRegInSet(SmallSet<unsigned, 32>& RegSet, unsigned Reg)
{
  if (RegSet.count(Reg))
    return true;
  // check Aliased Registers
  for (const uint16_t *Alias = TM.getRegisterInfo()->getAliasSet(Reg);
       *Alias; ++ Alias)
    if (RegSet.count(*Alias))
      return true;

  return false;
}

// return true if the candidate is a delay filler.
bool PatmosDelaySlotFiller::isDelayFiller(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator candidate)
{
  if (candidate == MBB.begin())
    return false;
  --candidate;
  return candidate->hasDelaySlot();
}

