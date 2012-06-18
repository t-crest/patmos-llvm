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
  unsigned Opc = 0;

  if (RC == &Patmos::RRegsRegClass)
    Opc = Patmos::SWC;
  else if (RC == &Patmos::SRegsRegClass) {
    Opc = Patmos::SWC;
//     assert("Unexpected spill of special register." && false);
//     abort();
  }
  else if (RC == &Patmos::PRegsRegClass) {
    assert("Unexpected spill of special register." && false);
    abort();
  }

  assert(Opc && "Register class not handled!");
  AddDefaultPred(BuildMI(MBB, MI, DL, get(Opc)))
    .addFrameIndex(FrameIndex).addImm(0) // address
    .addReg(SrcReg, getKillRegState(isKill)); // value to store
}

void PatmosInstrInfo::loadRegFromStackSlot( MachineBasicBlock &MBB,
                                            MachineBasicBlock::iterator MI,
                                            unsigned DestReg, int FrameIdx,
                                            const TargetRegisterClass *RC,
                                            const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();
  unsigned Opc = 0;

  if (RC == &Patmos::RRegsRegClass)
    Opc = Patmos::LWC;
  else if (RC == &Patmos::SRegsRegClass) {
    Opc = Patmos::LWC;
//     assert("Unexpected spill of special register." && false);
//     abort();
  }
  else if (RC == &Patmos::PRegsRegClass) {
    assert("Unexpected spill of special register." && false);
    abort();
  }

  assert(Opc && "Register class not handled!");
  AddDefaultPred(BuildMI(MBB, MI, DL, get(Opc), DestReg))
    .addFrameIndex(FrameIdx).addImm(0); // address
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
  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;

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
        continue;
      }
      // If the block has any instructions after an uncond branch, delete them.
      while (llvm::next(I) != MBB.end())
        llvm::next(I)->eraseFromParent();
      Cond.clear();
      FBB = 0;

      // If it is a fallthrough, eliminate also the unconditional branch
      if (MBB.isLayoutSuccessor(getBranchTarget(I))) {
        TBB = 0;
        I->eraseFromParent();
        I = MBB.end();
        continue;
      }

      // TBB is used to indicate the unconditinal destination.
      TBB = getBranchTarget(I);
      continue;
    }

    // TODO Handle conditional branches
    if (!I->isConditionalBranch())
      return true; //Unknown Opcode

    return true;
  }
  return false;
}

unsigned
PatmosInstrInfo::InsertBranch(MachineBasicBlock &MBB,MachineBasicBlock *TBB,
                              MachineBasicBlock *FBB,
                              const SmallVectorImpl<MachineOperand> &Cond,
                              DebugLoc DL) const {
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "Patmos branch conditions should have one component!");

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

bool PatmosInstrInfo::ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  // invert the flag
  unsigned invflag = Cond[1].getImm();
  Cond[1].setImm(1-invflag);
  return false; //success
}


MachineBasicBlock *PatmosInstrInfo::getBranchTarget(const MachineInstr *MI) const {
  assert(MI->isBranch() && "Not a branch instruction!");
  return MI->getOperand(2).getMBB();
}
