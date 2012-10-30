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
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"


//#define PATMOS_DELAY_SLOT_FILLER_TRACE

#ifdef PATMOS_DELAY_SLOT_FILLER_TRACE
#define DEBUG_TRACE(x) DEBUG(x)
#else
#define DEBUG_TRACE(x) /*empty*/
#endif

using namespace llvm;

STATISTIC( FilledSlots, "Number of delay slots filled");
STATISTIC( FilledNOPs,  "Number of delay slots filled with NOPs");

STATISTIC( InsertedLoadNOPs, "Number of NOPs inserted after loads");



static cl::opt<bool> DisableDelaySlotFiller(
  "mpatmos-disable-delay-filler",
  cl::init(false),
  cl::desc("Disable the Patmos delay slot filler."),
  cl::Hidden);

/// Number of delay slots for control-flow instructions
static const unsigned CTRL_DELAY_SLOTS = 2;

namespace {
  class PatmosDelaySlotFiller : public MachineFunctionPass {
  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    TargetMachine &TM;
    const PatmosInstrInfo *TII;
    const TargetRegisterInfo *TRI;

    static char ID;
    PatmosDelaySlotFiller(TargetMachine &tm)
      : MachineFunctionPass(ID), TM(tm),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        TRI(tm.getRegisterInfo()) { }

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

  protected:
    /// insertAfterLoad - Insert a NOP after a load instruction, if the
    /// successor instruction uses the loaded value.
    bool insertAfterLoad(MachineBasicBlock &MBB,
                         const MachineBasicBlock::iterator I);

    void fillSlotForCtrlFlow(MachineBasicBlock &MBB,
                    const MachineBasicBlock::iterator I,
                    SmallSet<MachineInstr*, 16> &FillerInstrs);

    void insertDefsUses(MachineInstr *MI,
                    SmallSet<unsigned, 32> &RegDefs,
                    SmallSet<unsigned, 32> &RegUses);

    bool hasDefUseDep(const MachineInstr *D, const MachineInstr *U);

  private:

    bool IsRegInSet(const SmallSet<unsigned, 32> &RegSet,
                    unsigned Reg);

    bool delayHasHazard(MachineBasicBlock::iterator candidate,
                        bool &sawLoad, bool &sawStore,
                        const SmallSet<unsigned, 32> &RegDefs,
                        const SmallSet<unsigned, 32> &RegUses);

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
  // Instructions already used to fill delay slots
  SmallSet<MachineInstr *, 16> FillerInstrs;

  // XXX call AnalyzeBranch for this MBB for a last time?

  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {
    // Control-flow instructions ("proper" delay slots)
    if (I->hasDelaySlot()) {
      assert( ( I->isCall() || I->isReturn() || I->isBranch() )
              && "Unexpected instruction with delay slot.");

      fillSlotForCtrlFlow(MBB, I, FillerInstrs);
      Changed = true; // pass result
    }
  }

  // insert NOPs after other instructions, if necessary
  // FIXME: This should eventually be handled in the scheduler.
  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {
    unsigned opc = I->getOpcode();
    if (opc==Patmos::MUL || opc==Patmos::MULU) {
      ++I;
      TII->insertNoop(MBB, I);
      TII->insertNoop(MBB, I);
      TII->insertNoop(MBB, I);
      Changed = true; // pass result
    } else if (I->mayLoad()) {
      // if the instruction is a load instruction, and the next instruction
      // reads a register defined by the load, we insert a NOP.
      Changed |= insertAfterLoad(MBB, I);
    }
  }
  return Changed;
}


void PatmosDelaySlotFiller::
fillSlotForCtrlFlow(MachineBasicBlock &MBB, const MachineBasicBlock::iterator I,
                    SmallSet<MachineInstr *, 16> &FillerInstrs) {

  SmallVector<MachineInstr *, 8> Candidates;

  DEBUG( dbgs() << "Filling slots for: " << *I );

  if (!DisableDelaySlotFiller) {
    bool sawLoad = false;
    bool sawStore = false;
    SmallSet<unsigned, 32> RegDefs;
    SmallSet<unsigned, 32> RegUses;

    // initialize sets
    insertDefsUses(I, RegDefs, RegUses);

    MachineBasicBlock::iterator J = I;
    while (J != MBB.begin()) {
      --J;
      DEBUG_TRACE( dbgs() << " -- inspect: " << *J );

      if ( J->hasDelaySlot() || FillerInstrs.count(J) ||
          Candidates.size() == CTRL_DELAY_SLOTS ||
           J->isInlineAsm() || J->isLabel() ) {
        DEBUG_TRACE( dbgs() << " -- break at: " << *J );
        break;
      }
      // skip debug value
      if (J->isDebugValue()) continue;

      // skip upon hazard
      if (delayHasHazard(J, sawLoad, sawStore, RegDefs, RegUses) ||
          TII->isStackControl(J) // skip stack control instructions
         ) {
        DEBUG_TRACE( dbgs() << " -- skip: " << *J );
        // still consider dependencies
        insertDefsUses(J, RegDefs, RegUses);
        continue;
      }
      // Found a filler
      Candidates.push_back(J);
      DEBUG_TRACE( dbgs() << " -- candidate: " << *J );
    }
  }
  // move instructions / insert NOPs
  MachineBasicBlock::iterator NI = next(I);
  for (unsigned i=0; i<CTRL_DELAY_SLOTS; i++) {
    if (i < Candidates.size()) {
      MachineInstr *FillMI = Candidates[i];
      MBB.splice(next(I), &MBB, FillMI);
      FillerInstrs.insert(FillMI);
      ++FilledSlots;  // update statistics
      DEBUG( dbgs() << " -- filler: " << *FillMI );
    } else {
      // we add the NOPs before the next instruction
      TII->insertNoop(MBB, NI);
      FillerInstrs.insert(prior(NI));
      ++FilledNOPs;  // update statistics
      DEBUG( dbgs() << " -- filler: NOP\n" );
    }
  }

}



bool PatmosDelaySlotFiller::delayHasHazard(MachineBasicBlock::iterator I,
                            bool &sawLoad, bool &sawStore,
                            const SmallSet<unsigned, 32> &RegDefs,
                            const SmallSet<unsigned, 32> &RegUses) {

  assert(!I->isKill() && !I->hasDelaySlot());

  // Loads or stores cannot be moved past a store to the delay slot
  // and stores cannot be moved past a load.
  if (I->mayLoad()) {
    sawLoad = true;
    if (sawStore) return true;
  }

  if (I->mayStore()) {
    if (sawStore) return true;
    sawStore = true;
    if (sawLoad) return true;
  }

  for (MachineInstr::mop_iterator MO = I->operands_begin();
       MO != I->operands_end(); ++MO) {
    unsigned reg;

    if (!MO->isReg() || !(reg = MO->getReg()))
      continue; // skip

    if (MO->isDef()) {
      // check whether Reg is defined or used before delay slot.
      if (IsRegInSet(RegDefs, reg) || IsRegInSet(RegUses, reg))
        return true;
    }
    if (MO->isUse()) {
      // check whether Reg is defined before delay slot.
      if (IsRegInSet(RegDefs, reg))
        return true;
    }
  }
  return false;
}


void PatmosDelaySlotFiller::insertDefsUses(MachineInstr *MI,
                                           SmallSet<unsigned, 32> &RegDefs,
                                           SmallSet<unsigned, 32> &RegUses) {

  // If MI is a call or return, just examine the explicit non-variadic operands.
  MCInstrDesc MCID = MI->getDesc();
  unsigned e = MI->isCall() || MI->isReturn() ? MCID.getNumOperands() :
                                                MI->getNumOperands();

  DEBUG_TRACE(dbgs() << " ---- regs: [");
  for (unsigned i = 0; i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    unsigned reg;
    if (!MO.isReg() || !(reg = MO.getReg()))
      continue;

    DEBUG_TRACE(dbgs() << " " << PrintReg(reg, TRI) );
    if (MO.isDef())
      RegDefs.insert(reg);
    else if (MO.isUse())
      RegUses.insert(reg);
  }
  DEBUG_TRACE(dbgs() << " ]\n");
}

//returns true if the reg or its alias is in the RegSet.
bool PatmosDelaySlotFiller::IsRegInSet(const SmallSet<unsigned, 32> &RegSet,
                                       unsigned reg)
{
  if (RegSet.count(reg))
    return true;
  // check Aliased Registers
  for (const uint16_t *Alias = TRI->getAliasSet(reg);
       *Alias; ++ Alias)
    if (RegSet.count(*Alias)) {
      DEBUG_TRACE(dbgs() << " ---- alias: " << PrintReg(*Alias, TRI) << "\n");
      return true;
    }

  return false;
}



bool PatmosDelaySlotFiller::hasDefUseDep(const MachineInstr *D,
                                         const MachineInstr *U) {
  // check all use operands of instruction U
  for (MachineInstr::const_mop_iterator MO = U->operands_begin();
      MO != U->operands_end(); ++MO) {
    // if the operand is a use and D defines it, we have a def-use dependence
    if (MO->isReg() && MO->readsReg() &&
        D->definesRegister(MO->getReg())) {
      return true;
    }
  }
  return false;
}


bool PatmosDelaySlotFiller::
insertAfterLoad(MachineBasicBlock &MBB, const MachineBasicBlock::iterator I) {

  MachineBasicBlock::iterator J = next(I);

  // "usual" case, the load is in the middle of an MBB
  if (J!=MBB.end() && hasDefUseDep(I,J)) {
    // insert after I
    TII->insertNoop(MBB, next(I));
    // stats and debug output
    ++InsertedLoadNOPs;
    DEBUG( dbgs() << "NOP inserted after load: " << *I );
    DEBUG( dbgs() << "                 before: " << *J );
    return true;
  }

  // the load is the last instruction of the block;
  // we have to check the successors
  if ( J == MBB.end() ) {
    if (MBB.succ_empty()) // no successor, nothing we need to do
      return false;

    if (MBB.succ_size() == 1) { // fallthrough case
      MachineInstr *FirstMI = (*MBB.succ_begin())->begin();
      if (hasDefUseDep(I, FirstMI)) {
          TII->insertNoop(MBB, next(I)); // insert at the end of MBB
          // stats and debug output
          ++InsertedLoadNOPs;
          DEBUG( dbgs() << "NOP inserted after load: " << *I );
          DEBUG( dbgs() << "           (end) before: " << *FirstMI );
          return true;
      }
    } else { // more than one successor
      assert(MBB.succ_size() > 1);
      // check all successors
      bool inserted = false;
      for (MachineBasicBlock::succ_iterator SMBB = MBB.succ_begin();
              SMBB!=MBB.succ_end(); ++SMBB) {
        MachineInstr *FirstMI = (*SMBB)->begin();
        if (hasDefUseDep(I, FirstMI)) {
          TII->insertNoop(**SMBB, FirstMI); // insert before first instruction
          // stats and debug output
          ++InsertedLoadNOPs;
          if (!inserted) {
            DEBUG( dbgs() << "NOP inserted after load: " << *I );
            inserted = true;
          }
          DEBUG(   dbgs() << "          (succ) before: " << *FirstMI );
        }
      }
      return inserted;
    }
  }
  return false;
}
