//===-- PatmosDelaySlotFiller.cpp - Patmos delay slot filler --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is a simple local pass that attempts to fill delay slots of control
// flow changing instructions (call, return, branches) with useful
// instructions. If no instructions can be moved into the delay slot, then a
// NOP is inserted.
//
// At the current state, only instructions from the local basic are considered
// (not the targets of branches).
//
// As a post-processing step, NOPs are inserted after loads again, where
// necessary.
//
// FIXME: Also 3 NOPs are inserted after MUL/MULU - this needs to be considered
//        in the scheduler.
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
#include "llvm/Function.h"
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

  class DelayHazardInfo;

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
      DEBUG( dbgs() << "[DelaySlotFiller] "
                    << F.getFunction()->getName() << "\n" );
      for (MachineFunction::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI)
        Changed |= runOnMachineBasicBlock(*FI);

      // insert NOPs after other instructions, if necessary
      // FIXME: This should eventually be handled in the scheduler.
      for (MachineFunction::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI)
        Changed |= insertNOPs(*FI);
      return Changed;
    }

    /// hasDefUseDep - Returns true if D defines a register thet is used in U.
    /// Used in this class to check whether a value loaded to a register is
    /// used in the next instruction.
    bool hasDefUseDep(const MachineInstr *D, const MachineInstr *U) const;

  protected:
    /// insertNOPs - insert NOPs where necessary to avoid hazards
    bool insertNOPs(MachineBasicBlock &MBB);

    /// insertAfterLoad - Insert a NOP after a load instruction, if the
    /// successor instruction uses the loaded value.
    bool insertAfterLoad(MachineBasicBlock &MBB,
                         const MachineBasicBlock::iterator I);

    /// fillSlotForCtrlFlow - Fills the delay slots of instruction I in MBB.
    /// \param FillerInstrs  A reference to instructions already used as
    ///                      fillers in the current MBB
    void fillSlotForCtrlFlow(MachineBasicBlock &MBB,
                    const MachineBasicBlock::iterator I,
                    SmallSet<MachineInstr*, 16> &FillerInstrs);
  };

  /// Information used throughout finding a delay filler for an instruction
  /// with a delay slot.
  class DelayHazardInfo {
  public:

    DelayHazardInfo(PatmosDelaySlotFiller &pdsf)
      : PDSF(pdsf), sawLoad(false), sawStore(false), sawSTC(false) { }

    void insertDefsUses(MachineInstr *MI);
    bool hasHazard(MachineBasicBlock::iterator candidate);

    unsigned getNumCandidates() const { return Candidates.size(); }
    MachineInstr *getCandidate(unsigned idx) { return Candidates[idx]; }
    void appendCandidate(MachineInstr *MI) { Candidates.push_back(MI); }

  protected:
    bool isRegInSet(const SmallSet<unsigned, 32> &RegSet,
                    unsigned reg) const;
  private:
    const PatmosDelaySlotFiller& PDSF;
    bool sawLoad;
    bool sawStore;
    bool sawSTC; // stack control instruction
    SmallSet<unsigned, 32> RegDefs;
    SmallSet<unsigned, 32> RegUses;
    SmallVector<MachineInstr *, 16> Candidates;
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
  // Instructions already used to fill delay slots,
  // this info needs to survive for the whole MBB
  SmallSet<MachineInstr *, 16> FillerInstrs;

  // XXX call AnalyzeBranch for this MBB for a last time?

  // consider the basic block from top to bottom
  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {
    // Control-flow instructions ("proper" delay slots)
    if (I->hasDelaySlot()) {
      assert( ( I->isCall() || I->isReturn() || I->isBranch() )
              && "Unexpected instruction with delay slot.");
      // following call scans the instructions backwards again
      fillSlotForCtrlFlow(MBB, I, FillerInstrs);
      Changed = true; // pass result
    }
  }
  return Changed;
}


bool PatmosDelaySlotFiller::insertNOPs(MachineBasicBlock &MBB) {
  bool Changed = false;
  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {
    unsigned opc = I->getOpcode();
    if (opc==Patmos::MUL || opc==Patmos::MULU) {
      MachineBasicBlock::iterator J = next(I);
      TII->insertNoop(MBB, J);
      TII->insertNoop(MBB, J);
      TII->insertNoop(MBB, J);
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

  DelayHazardInfo DI(*this);

  DEBUG( dbgs() << "Filling slots for: " << *I );

  if (!DisableDelaySlotFiller) {

    // initialize sets
    DI.insertDefsUses(I);

    MachineBasicBlock::iterator J = I;
    while (J != MBB.begin()) {
      --J;
      DEBUG_TRACE( dbgs() << " -- inspect: " << *J );

      // we can't / don't need to scan backward further
      if ( J->hasDelaySlot() || FillerInstrs.count(J) ||
           DI.getNumCandidates() == CTRL_DELAY_SLOTS ||
           J->isInlineAsm() || J->isLabel() ) {
        DEBUG_TRACE( dbgs() << " -- break at: " << *J );
        break;
      }
      // skip debug value
      if (J->isDebugValue()) continue;

      // skip upon hazard
      if (DI.hasHazard(J)) {
        // update dependencies
        DI.insertDefsUses(J);
        DEBUG_TRACE( dbgs() << " -- skip: " << *J );
        continue;
      }
      // Found a filler, add to candidates
      DI.appendCandidate(J);
      DEBUG_TRACE( dbgs() << " -- candidate: " << *J );
    }
  }

  // move instructions / insert NOPs
  MachineBasicBlock::iterator NI = next(I);
  for (unsigned i=0; i<CTRL_DELAY_SLOTS; i++) {
    if (i < DI.getNumCandidates()) {
      MachineInstr *FillMI = DI.getCandidate(i);
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



bool PatmosDelaySlotFiller::hasDefUseDep(const MachineInstr *D,
                                         const MachineInstr *U) const {
  // If U is a call or return, just examine the explicit non-variadic operands.
  const MCInstrDesc& MCID = U->getDesc();
  unsigned e = (U->isCall() || U->isReturn()) ? MCID.getNumOperands() :
                                                U->getNumOperands();

  for (unsigned i = 0; i != e; ++i) {
    const MachineOperand &MO = U->getOperand(i);
    // if the operand is a use and D defines it, we have a def-use dependence
    if ( MO.isReg() && MO.readsReg() && D->definesRegister(MO.getReg()) ) {
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


///////////////////////////////////////////////////////////////////////////////
// DelayHazardInfo methods
///////////////////////////////////////////////////////////////////////////////


void DelayHazardInfo::insertDefsUses(MachineInstr *MI) {

  // If MI is a call or return, just examine the explicit non-variadic operands.
  const MCInstrDesc& MCID = MI->getDesc();
  unsigned e = (MI->isCall() || MI->isReturn()) ? MCID.getNumOperands() :
                                                  MI->getNumOperands();

  DEBUG_TRACE(dbgs() << " ---- regs: [");
  for (unsigned i = 0; i != e; ++i) {
    const MachineOperand &MO = MI->getOperand(i);
    unsigned reg;
    if (!MO.isReg() || !(reg = MO.getReg()))
      continue;

    bool inserted = false;
    if (MO.isDef())
      inserted = RegDefs.insert(reg);
    else if (MO.isUse())
      inserted = RegUses.insert(reg);

    if (inserted) {
      DEBUG_TRACE(dbgs() << " " << PrintReg(reg, PDSF.TRI) );
    }
  }
  DEBUG_TRACE(dbgs() << " ]\n");
}


bool DelayHazardInfo::hasHazard(MachineBasicBlock::iterator I) {

  assert(!I->isKill() && !I->hasDelaySlot());

  const PatmosInstrInfo *TII = PDSF.TII;

  // don't move long latency/split MUL into delay slot
  if (I->getOpcode() == Patmos::MUL ||
      I->getOpcode() == Patmos::MULU)
    return true;

  // don't move loads with use immediately afterwards
  if ( I->mayLoad() && !Candidates.empty() &&
        PDSF.hasDefUseDep(I, Candidates.back()) )
      return true;

  // be very careful about stack cache access: ld/st must not
  // pass control instructions;
  // don't move stack control
  if (TII->isStackControl(I)) {
    sawSTC = true;
    return true;
  }
  if ( (I->mayLoad()||I->mayStore()) && TII->getMemType(I)==PatmosII::MEM_S
       && sawSTC )
    return true;

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

  for (MachineInstr::const_mop_iterator MO = I->operands_begin();
       MO != I->operands_end(); ++MO) {
    unsigned reg;

    if (!MO->isReg() || !(reg = MO->getReg()))
      continue; // skip

    if (MO->isDef()) {
      // check whether Reg is defined or used before delay slot.
      if (isRegInSet(RegDefs, reg) || isRegInSet(RegUses, reg))
        return true;
    }
    if (MO->isUse()) {
      // check whether Reg is defined before delay slot.
      if (isRegInSet(RegDefs, reg))
        return true;
    }
  }
  return false;
}


//returns true if the reg or its alias is in the RegSet.
bool DelayHazardInfo::isRegInSet(const SmallSet<unsigned, 32> &RegSet,
                                 unsigned reg) const {

  if (RegSet.count(reg))
    return true;
  // check Aliased Registers
  for (const uint16_t *Alias = PDSF.TRI->getAliasSet(reg);
       *Alias; ++ Alias)
    if (RegSet.count(*Alias)) {
      DEBUG_TRACE(dbgs() << " ---- alias: "
                  << PrintReg(*Alias, PDSF.TRI) << "\n");
      return true;
    }

  return false;
}

