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
// FIXME: This pass must check for bundles. It may skip moving bundles around,
//        but it must calculate the latencies correctly and insert NOPs
//        correctly.
//        This will become a fall-back pass to fill up any hazards and delay
//        slots with NOPs in case scheduling has been disabled. If scheduling
//        is enabled, it must be assumed that delay slots are already filled,
//        but the pass should still check and insert NOPs for hazards as
//        belt-and-braces-pass.
//
// FIXME: We should also check for other latencies, like mfs after STC. Use
//        the hazard detector for this!
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "delay-slot-filler"
#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
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

STATISTIC( SkippedLoadNOPs, "Number of loads not requiring a NOP");
STATISTIC( InsertedLoadNOPs, "Number of NOPs inserted after loads");

STATISTIC( SkippedMulNOPs, "Number of muls not requiring a NOP");
STATISTIC( InsertedMulNOPs, "Number of NOPs inserted after muls");


static cl::opt<bool> DisableDelaySlotFiller(
  "mpatmos-disable-delay-filler",
  cl::init(false),
  cl::desc("Disable the Patmos delay slot filler."),
  cl::Hidden);

namespace {

  class DelayHazardInfo;

  class PatmosDelaySlotFiller : public MachineFunctionPass {
  private:
    bool ForceDisableFiller;

    static char ID;
  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    PatmosTargetMachine &TM;
    const PatmosInstrInfo *TII;
    const TargetRegisterInfo *TRI;

    PatmosDelaySlotFiller(PatmosTargetMachine &tm, bool disable)
      : MachineFunctionPass(ID), ForceDisableFiller(disable), TM(tm),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        TRI(tm.getRegisterInfo()) { }

    virtual const char *getPassName() const {
      return "Patmos Delay Slot Filler";
    }

    bool runOnMachineFunction(MachineFunction &F) {
      bool Changed = false;
      DEBUG( dbgs() << "\n[DelaySlotFiller] "
                    << F.getFunction()->getName() << "\n" );

      // FIXME: check if Post-RA scheduler is enabled (by option or Subtarget),
      //        skip this loop (delay slot filling) in this case.
      for (MachineFunction::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI)
        Changed |= fillDelaySlots(*FI);

      // insert NOPs after other instructions, if necessary
      for (MachineFunction::iterator FI = F.begin(), FE = F.end();
           FI != FE; ++FI)
        Changed |= insertNOPs(*FI);
      return Changed;
    }

    /// hasDefUseDep - Returns true if D defines a register that is used in U.
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

    /// insertAfterMul - Insert a NOP after a mul instruction, if the
    /// successor instructions use the result value.
    bool insertAfterMul(MachineBasicBlock &MBB,
                         const MachineBasicBlock::iterator I);

    /// fillDelaySlots - Fill in delay slots for the given basic block.
    /// We assume there is only one delay slot per delayed instruction.
    ///
    bool fillDelaySlots(MachineBasicBlock &MBB);

    /// fillSlotForCtrlFlow - Fills the delay slots of instruction I in MBB.
    /// \param FillerInstrs  A reference to instructions already used as
    ///                      fillers in the current MBB
    void fillSlotForCtrlFlow(MachineBasicBlock &MBB,
                    const MachineBasicBlock::iterator I,
                    SmallSet<MachineInstr*, 16> &FillerInstrs);

    /// insertNOPAfter - Insert a nop after an instruction I, or split the
    /// bundle I.
    void insertNOPAfter(MachineBasicBlock &MBB,
                        const MachineBasicBlock::iterator I);
  };

  /// Information used throughout finding a delay filler for an instruction
  /// with a delay slot.
  class DelayHazardInfo {
  public:

    DelayHazardInfo(PatmosDelaySlotFiller &pdsf, const MachineInstr &I)
      : PDSF(pdsf), MI(I), sawLoad(false), sawStore(false), sawSTC(false) { }

    void insertDefsUses(MachineInstr *MI);
    bool hasHazard(MachineBasicBlock &MBB,
                   MachineBasicBlock::iterator candidate);

    unsigned getNumCandidates() const { return Candidates.size(); }
    MachineInstr *getCandidate(unsigned idx) { return Candidates[idx]; }
    void appendCandidate(MachineInstr *MI) { Candidates.push_back(MI); }

  protected:
    bool isRegInSet(const SmallSet<unsigned, 32> &RegSet,
                    unsigned reg) const;
  private:
    const PatmosDelaySlotFiller &PDSF;
    const MachineInstr &MI;
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
FunctionPass *llvm::createPatmosDelaySlotFillerPass(PatmosTargetMachine &tm,
                                                    bool ForceDisable) {
  return new PatmosDelaySlotFiller(tm, ForceDisable);
}


bool PatmosDelaySlotFiller::fillDelaySlots(MachineBasicBlock &MBB) {
  bool Changed = false;
  // Instructions already used to fill delay slots,
  // this info needs to survive for the whole MBB
  SmallSet<MachineInstr *, 16> FillerInstrs;

  DEBUG( dbgs() << "Filling slots in BB#" << MBB.getNumber()
                << " (" << MBB.getFullName() << ")\n" );
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

  DEBUG( dbgs() << "Inserting NOPs in " << MBB.getName() << "\n" );
  for (MachineBasicBlock::iterator I = MBB.begin(); I != MBB.end(); ++I) {

    if (TII->hasOpcode(I, Patmos::MUL) || TII->hasOpcode(I, Patmos::MULU)) {
      Changed |= insertAfterMul(MBB, I);
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

  DelayHazardInfo DI(*this, *I);

  DEBUG( dbgs() << "For: " << *I );

  unsigned CFLDelaySlots = TM.getSubtargetImpl()->getCFLDelaySlotCycles(I);

  if (!DisableDelaySlotFiller && !ForceDisableFiller) {

    // initialize sets
    DI.insertDefsUses(I);

    MachineBasicBlock::iterator J = I;
    while (J != MBB.begin()) {
      --J;
      DEBUG_TRACE( dbgs() << " -- inspect: " << *J );

      // we can't / don't need to scan backward further
      if ( J->hasDelaySlot() || FillerInstrs.count(J) ||
           DI.getNumCandidates() == CFLDelaySlots ||
           J->isInlineAsm() || J->isLabel() ) {
        DEBUG_TRACE( dbgs() << " -- break at: " << *J );
        break;
      }
      // skip debug value
      if (J->isDebugValue()) continue;
      if (J->isImplicitDef()) continue;

      // skip upon hazard
      if (DI.hasHazard(MBB, J)) {
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
  for (unsigned i=0; i<CFLDelaySlots; i++) {
    if (i < DI.getNumCandidates()) {
      MachineInstr *FillMI = DI.getCandidate(i);
      MBB.splice(next(I), &MBB, FillMI);
      FillerInstrs.insert(FillMI);
      ++FilledSlots;  // update statistics
      DEBUG( dbgs() << " -- filler: " << *FillMI );
    } else {
      // we add the NOPs before the next instruction
      insertNOPAfter(MBB, I);
      FillerInstrs.insert(prior(NI));
      ++FilledNOPs;  // update statistics
      DEBUG( dbgs() << " -- filler: NOP\n" );
    }
  }

}

void PatmosDelaySlotFiller::insertNOPAfter(MachineBasicBlock &MBB,
                    const MachineBasicBlock::iterator I)
{
  // We just insert a NOP. We let the PatmosBundleCleanup pass split bundles
  // to remove the NOPs where required.
  MachineBasicBlock::iterator NI = next(I);
  TII->insertNoop(MBB, NI);
}


bool PatmosDelaySlotFiller::hasDefUseDep(const MachineInstr *D,
                                         const MachineInstr *U) const {

  // TODO check for mutually exclusive predicates, using
  // PatmosInstrInfo.hasDisjointPredicates. In this case, check only if
  // one instruction defines the guard of the other instruction.

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

  MachineBasicBlock::iterator J = TII->nextNonPseudo(MBB, I);

  // "usual" case, the load is in the middle of an MBB
  if (J!=MBB.end() && hasDefUseDep(I,J)) {
    // insert after I
    TII->insertNoop(MBB, next(I));
    // stats and debug output
    ++InsertedLoadNOPs;
    DEBUG( dbgs() << "NOP inserted after load: " << *I );
    DEBUG( dbgs() << "                 before: " << *J );
    return true;
  } else if ( J != MBB.end() ) {
    // no dependency in next cycle
    ++SkippedLoadNOPs;
    return false;
  }

  // the load is the last instruction of the block;
  // we have to check the successors
  if ( J == MBB.end() ) {
    if (MBB.succ_empty()) // no successor, nothing we need to do
      return false;

    // check all successors
    bool inserted = false;
    for (MachineBasicBlock::succ_iterator SMBB = MBB.succ_begin();
            SMBB!=MBB.succ_end(); ++SMBB) {
      MachineInstr *FirstMI = (*SMBB)->begin();
      if ((*SMBB)->empty() || hasDefUseDep(I, FirstMI)) {
        TII->insertNoop(**SMBB, (*SMBB)->begin()); // insert before first instruction
        // stats and debug output
        ++InsertedLoadNOPs;
        if (!inserted) {
          DEBUG( dbgs() << "NOP inserted after load: " << *I );
          inserted = true;
        }
        DEBUG(   dbgs() << "          (succ) before: " << *FirstMI );
      } else {
        ++SkippedLoadNOPs;
      }
    }
    return inserted;
  }
  return false;
}

bool PatmosDelaySlotFiller::
insertAfterMul(MachineBasicBlock &MBB, const MachineBasicBlock::iterator I) {

  int Latency = TM.getSubtargetImpl()->getMULLatency();

  MachineBasicBlock::iterator J = TII->nextNonPseudo(MBB, I);

  while (Latency > 0) {

    // Check if this is a dependency
    const MachineInstr *MI;
    if (J == MBB.end() || (MI = TII->hasOpcode(J, Patmos::MFS)) ) {

      // TODO We do not look over BB boundaries for now
      unsigned Reg = J != MBB.end() ? MI->getOperand(3).getReg() : Patmos::SL;

      if (Reg == Patmos::SL || Reg == Patmos::SH) {
        while (Latency > 0) {
          insertNOPAfter(MBB, I);
          InsertedMulNOPs++;
          Latency--;
        }

        return true;
      }
    }

    Latency--;

    SkippedMulNOPs++;

    J = TII->nextNonPseudo(MBB, J);
  }

  return false;
}

///////////////////////////////////////////////////////////////////////////////
// DelayHazardInfo methods
///////////////////////////////////////////////////////////////////////////////


void DelayHazardInfo::insertDefsUses(MachineInstr *MI) {

  // If MI is a call or return, just examine the explicit non-variadic operands.
  const MCInstrDesc& MCID = MI->getDesc();
  unsigned e = (MI->isCall() || MI->isReturn(MachineInstr::AllInBundle))
                      ? MCID.getNumOperands() : MI->getNumOperands();

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


bool DelayHazardInfo::hasHazard(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) {

  // for calls, allow only single-issue and 32bit instructions
  if (MI.isCall() && PDSF.TII->getInstrSize(I) != 4)
    return true;

  if (I->isBundle()) {
    MachineBasicBlock::instr_iterator II = *I; ++II;

    for (; II != MBB.instr_end() && II->isInsideBundle(); ++II) {
      // skip debug value
      if (II->isDebugValue()) continue;
      if (II->isImplicitDef()) continue;

      if (hasHazard(MBB, II)) return true;
    }
    return false;
  }

  assert(!I->isKill() && !I->hasDelaySlot());

  const PatmosInstrInfo *TII = PDSF.TII;

  // don't move long latency/split MUL into delay slot
  if (I->getOpcode() == Patmos::MUL ||
      I->getOpcode() == Patmos::MULU)
    return true;

  // don't move loads to the last delay slot for return
  if (I->mayLoad() && Candidates.empty() && MI.isReturn() )
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

  // instruction has unmodeled side-effects
  // check for safe MTS/MFS (which can have side-effects in general)
  if (I->hasUnmodeledSideEffects() && !TII->isSideEffectFreeSRegAccess(I))
    return true;

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

  // Check Reg and all aliased Registers.
  for (MCRegAliasIterator AI(reg, PDSF.TRI, true);
       AI.isValid(); ++AI)
    if (RegSet.count(*AI)) {
      DEBUG_TRACE(dbgs() << " ---- alias: "
                  << PrintReg(*AI, PDSF.TRI) << "\n");
      return true;
    }
  return false;
}

