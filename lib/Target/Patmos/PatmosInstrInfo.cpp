//===- PatmosInstrInfo.cpp - Patmos Instruction Information ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Patmos implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define GET_INSTRINFO_CTOR
#include "PatmosGenInstrInfo.inc"


using namespace llvm;

PatmosInstrInfo::PatmosInstrInfo(PatmosTargetMachine &tm)
  : PatmosGenInstrInfo(Patmos::ADJCALLSTACKDOWN, Patmos::ADJCALLSTACKUP),
    RI(tm, *this), TM(tm) {}

bool PatmosInstrInfo::findCommutedOpIndices(MachineInstr *MI,
                                            unsigned &SrcOpIdx1,
                                            unsigned &SrcOpIdx2) const {
  switch (MI->getOpcode())
  {
    case Patmos::ADDr:
    case Patmos::ORr:
    case Patmos::ANDr:
    case Patmos::XORr:
    case Patmos::NORr:
      SrcOpIdx1 = 3;
      SrcOpIdx2 = 4;
      return true;
    case Patmos::MUL:
    case Patmos::MULU:
      SrcOpIdx1 = 2;
      SrcOpIdx2 = 3;
      return true;
    default:
      llvm_unreachable("Unexpected commutable machine instruction.");
  }
  return false;
}

void PatmosInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I, DebugLoc DL,
                                  unsigned DestReg, unsigned SrcReg,
                                  bool KillSrc) const {
  unsigned Opc;
  if (Patmos::RRegsRegClass.contains(DestReg, SrcReg)) {
    // General purpose register
    Opc = Patmos::MOV;
    AddDefaultPred(BuildMI(MBB, I, DL, get(Opc), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else if (Patmos::PRegsRegClass.contains(DestReg, SrcReg)) {
    // Predicate register
    Opc = Patmos::PMOV;
    AddDefaultPred(BuildMI(MBB, I, DL, get(Opc), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else if (Patmos::SRegsRegClass.contains(DestReg)) {
    assert(Patmos::RRegsRegClass.contains(SrcReg));
    AddDefaultPred(BuildMI(MBB, I, DL, get(Patmos::MTS), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else if (Patmos::SRegsRegClass.contains(SrcReg)) {
    assert(Patmos::RRegsRegClass.contains(DestReg));
    AddDefaultPred(BuildMI(MBB, I, DL, get(Patmos::MFS), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else {
    llvm_unreachable("Impossible reg-to-reg copy");
  }

}


void PatmosInstrInfo::storeRegToStackSlot( MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator MI,
                                           unsigned SrcReg, bool isKill,
                                           int FrameIndex,
                                           const TargetRegisterClass *RC,
                                           const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  if (RC == &Patmos::RRegsRegClass) {
    AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::SWC)))
      .addFrameIndex(FrameIndex).addImm(0) // address
      .addReg(SrcReg, getKillRegState(isKill)); // value to store
  }
  else if (RC == &Patmos::PRegsRegClass) {
    // As clients of this function (esp. InlineSpiller, foldMemoryOperands)
    // assume that the store instruction is the last one emitted, we cannot
    // simply emit two predicated stores.
    // We also cannot use RTR as it might be used for large FI offset
    // computation.
    // We work around this restriction by using a pseudo-inst that
    // is expanded in PatmosRegisterInfo::eliminateFrameIndex()
    BuildMI(MBB, MI, DL, get(Patmos::PSEUDO_PREG_SPILL))
      .addFrameIndex(FrameIndex).addImm(0) // address
      .addReg(SrcReg, getKillRegState(isKill)); // predicate register
  }
  else llvm_unreachable("Register class not handled!");
}

void PatmosInstrInfo::loadRegFromStackSlot( MachineBasicBlock &MBB,
                                            MachineBasicBlock::iterator MI,
                                            unsigned DestReg, int FrameIdx,
                                            const TargetRegisterClass *RC,
                                            const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  if (RC == &Patmos::RRegsRegClass) {
    AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::LWC), DestReg))
      .addFrameIndex(FrameIdx).addImm(0); // address
  }
  else if (RC == &Patmos::PRegsRegClass) {
    // Clients assume the last instruction inserted to be a load instruction.
    // Again, we work around this with a pseudo instruction that is expanded
    // during FrameIndex elimination.
    BuildMI(MBB, MI, DL, get(Patmos::PSEUDO_PREG_RELOAD), DestReg)
      .addFrameIndex(FrameIdx).addImm(0); // address
  }
  else llvm_unreachable("Register class not handled!");
}


////////////////////////////////////////////////////////////////////////////////
//
// Branch handling
//

bool PatmosInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const
{
  // Start from the bottom of the block and work up, examining the
  // terminator instructions.
  MachineBasicBlock::iterator I = MBB.end();
  bool seen_uncondbr=false, seen_condbr=false;
  int rcnt=0;

  while (I != MBB.begin()) {
    --I;
    rcnt++;
    DEBUG( dbgs() << "[AnalyzeBranch] BB#" << MBB.getNumber() << " i" << rcnt
          << ": " << *I; );

    if (I->isDebugValue())
      continue;

    // TODO skip delay slots (if this pass is supposed to eventually
    // work after delay slot filler)

    // Working from the bottom, when we see a non-terminator
    // instruction, we're done.
    if (!isUnpredicatedTerminator(I))
      break;

    // A terminator that isn't a (direct) branch can't easily be handled
    // by this analysis.
    if (!I->isBranch() || I->isIndirectBranch())
      return true;

    // Handle Unconditional branches
    if (I->isUnconditionalBranch()) {
      if (!AllowModify) {
        TBB = getBranchTarget(I);
        seen_uncondbr = true;
        continue;
      }
      // If the block has any instructions after an uncond branch, delete them.
      while (llvm::next(I) != MBB.end())
        llvm::next(I)->eraseFromParent();
      Cond.clear();
      FBB = NULL;

      // If it is a fallthrough, eliminate also the unconditional branch
      if (MBB.isLayoutSuccessor(getBranchTarget(I))) {
        TBB = NULL;
        seen_uncondbr = false;
        I->eraseFromParent();
        I = MBB.end();
        continue;
      }

      // TBB is used to indicate the unconditional destination.
      TBB = getBranchTarget(I);
      seen_uncondbr = true;
      continue;
    }

    // Handle conditional branches
    if (I->isConditionalBranch() ) {
      int i;

      //FIXME
      return true;

      // we only treat the first conditional branch in a row
      if (seen_condbr) break;
      seen_condbr = true;
      // Get branch condition
      i = I->findFirstPredOperandIdx();
      assert(i != -1 );
      Cond.push_back(I->getOperand(i));   // reg
      Cond.push_back(I->getOperand(i+1)); // flag
      // We've processed an unconditional branch before,
      // the unconditional target goes to FBB now
      if (seen_uncondbr) FBB = TBB; //FIXME
      // target of conditional branch goes to TBB
      TBB = getBranchTarget(I);
      continue;
    }

    return true;
  }

  DEBUG({
    std::string msg;
    if      ( TBB==NULL && FBB==NULL &&  Cond.empty() ) msg = "fallthrough";
    else if ( TBB!=NULL && FBB==NULL &&  Cond.empty() ) msg = "uncond";
    else if ( TBB!=NULL && FBB==NULL && !Cond.empty() ) msg = "cond+fallthrough";
    else if ( TBB!=NULL && FBB!=NULL && !Cond.empty() ) msg = "cond+uncond";
    dbgs() << "[AnalyzeBranch] BB#" << MBB.getNumber() << "= " << msg << "\n";
  });

  return false;
}

unsigned
PatmosInstrInfo::InsertBranch(MachineBasicBlock &MBB,MachineBasicBlock *TBB,
                              MachineBasicBlock *FBB,
                              const SmallVectorImpl<MachineOperand> &Cond,
                              DebugLoc DL) const {
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 2 || Cond.size() == 0) &&
         "Patmos branch conditions should have two components (reg+imm)!");

  if (Cond.empty()) {
    // Unconditional branch?
    assert(TBB && !FBB && "Unconditional branch with multiple successors!");
    AddDefaultPred(BuildMI(&MBB, DL, get(Patmos::BCu)))
      .addMBB(TBB);
    return 1;
  }

  // Conditional branch.
  unsigned Count = 0;
  BuildMI(&MBB, DL, get(Patmos::BC))
    .addReg(Cond[0].getReg()).addImm(Cond[1].getImm()) // condition as predicate
    .addMBB(TBB);
  ++Count;

  if (FBB) {
    // Two-way Conditional branch. Insert the second (unconditional) branch.
    AddDefaultPred(BuildMI(&MBB, DL, get(Patmos::BCu)))
      .addMBB(FBB);
    ++Count;
  }
  return Count;

}


bool PatmosInstrInfo::isPredicated(const MachineInstr *MI) const
{
  int i = MI->findFirstPredOperandIdx();
  if (i != -1) {
    unsigned reg  = MI->getOperand(i).getReg();
    int      flag = MI->getOperand(++i).getImm();
    return (reg != Patmos::NoRegister) && ((reg != Patmos::P0) || flag);
  }
  return false;
}

bool PatmosInstrInfo::isUnpredicatedTerminator(const MachineInstr *MI) const {
  if (!MI->isTerminator()) return false;

  // Conditional branch is a special case.
  if (MI->isBranch() && !MI->isBarrier())
    return true;

  return !isPredicated(MI);
}



unsigned PatmosInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const
{
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;
    if (!I->isBranch()) break; // Not a branch
    // Remove the branch.
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

bool PatmosInstrInfo::
ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  // invert the flag
  int64_t invflag = Cond[1].getImm();
  Cond[1].setImm( (invflag)?0:-1 );
  return false; //success
}


MachineBasicBlock *PatmosInstrInfo::
getBranchTarget(const MachineInstr *MI) const {
  assert(MI->isBranch() && "Not a branch instruction!");
  return MI->getOperand(2).getMBB();
}

void PatmosInstrInfo::
InsertNOP(MachineBasicBlock &MBB, MachineBasicBlock::iterator &I,
          DebugLoc DL, unsigned NumCycles, bool ForceSCNOP) const {
  MachineBasicBlock::iterator J = I;
  if (ForceSCNOP || NumCycles<=1) {
    for(unsigned i=0; i<NumCycles; i++)
      BuildMI(MBB, ++J, DL, get(Patmos::NOP))
        .addReg(Patmos::NoRegister).addImm(1);
  } else {
    assert(isUInt<4>(NumCycles) && "Multicycle-NOP chains not implemented");
    AddDefaultPred(BuildMI(MBB, ++J, DL, get(Patmos::MCNOP)))
      .addImm(NumCycles);
  }
}
