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
  : PatmosGenInstrInfo(-1, -1),
    RI(tm, *this), TM(tm) {}

#if 0
void PatmosInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator MI,
                                    unsigned SrcReg, bool isKill, int FrameIdx,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();
  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = *MF.getFrameInfo();

  MachineMemOperand *MMO =
    MF.getMachineMemOperand(
              MachinePointerInfo(PseudoSourceValue::getFixedStack(FrameIdx)),
                            MachineMemOperand::MOStore,
                            MFI.getObjectSize(FrameIdx),
                            MFI.getObjectAlignment(FrameIdx));

  if (RC == &Patmos::GR16RegClass)
    BuildMI(MBB, MI, DL, get(Patmos::MOV16mr))
      .addFrameIndex(FrameIdx).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else if (RC == &Patmos::GR8RegClass)
    BuildMI(MBB, MI, DL, get(Patmos::MOV8mr))
      .addFrameIndex(FrameIdx).addImm(0)
      .addReg(SrcReg, getKillRegState(isKill)).addMemOperand(MMO);
  else
    llvm_unreachable("Cannot store this register to stack slot!");
}

void PatmosInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator MI,
                                           unsigned DestReg, int FrameIdx,
                                           const TargetRegisterClass *RC,
                                           const TargetRegisterInfo *TRI) const{
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();
  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = *MF.getFrameInfo();

  MachineMemOperand *MMO =
    MF.getMachineMemOperand(
              MachinePointerInfo(PseudoSourceValue::getFixedStack(FrameIdx)),
                            MachineMemOperand::MOLoad,
                            MFI.getObjectSize(FrameIdx),
                            MFI.getObjectAlignment(FrameIdx));

  if (RC == &Patmos::GR16RegClass)
    BuildMI(MBB, MI, DL, get(Patmos::MOV16rm))
      .addReg(DestReg).addFrameIndex(FrameIdx).addImm(0).addMemOperand(MMO);
  else if (RC == &Patmos::GR8RegClass)
    BuildMI(MBB, MI, DL, get(Patmos::MOV8rm))
      .addReg(DestReg).addFrameIndex(FrameIdx).addImm(0).addMemOperand(MMO);
  else
    llvm_unreachable("Cannot store this register to stack slot!");
}

void PatmosInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I, DebugLoc DL,
                                  unsigned DestReg, unsigned SrcReg,
                                  bool KillSrc) const {
  unsigned Opc;
  if (Patmos::GR16RegClass.contains(DestReg, SrcReg))
    Opc = Patmos::MOV16rr;
  else if (Patmos::GR8RegClass.contains(DestReg, SrcReg))
    Opc = Patmos::MOV8rr;
  else
    llvm_unreachable("Impossible reg-to-reg copy");

  BuildMI(MBB, I, DL, get(Opc), DestReg)
    .addReg(SrcReg, getKillRegState(KillSrc));
}

unsigned PatmosInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;

  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;
    if (I->getOpcode() != Patmos::JMP &&
        I->getOpcode() != Patmos::JCC &&
        I->getOpcode() != Patmos::Br &&
        I->getOpcode() != Patmos::Bm)
      break;
    // Remove the branch.
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }

  return Count;
}

bool PatmosInstrInfo::
ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  assert(Cond.size() == 1 && "Invalid Xbranch condition!");

  PatmosCC::CondCodes CC = static_cast<PatmosCC::CondCodes>(Cond[0].getImm());

  switch (CC) {
  default:
    assert(0 && "Invalid branch condition!");
    break;
  case PatmosCC::COND_E:
    CC = PatmosCC::COND_NE;
    break;
  case PatmosCC::COND_NE:
    CC = PatmosCC::COND_E;
    break;
  case PatmosCC::COND_L:
    CC = PatmosCC::COND_GE;
    break;
  case PatmosCC::COND_GE:
    CC = PatmosCC::COND_L;
    break;
  case PatmosCC::COND_HS:
    CC = PatmosCC::COND_LO;
    break;
  case PatmosCC::COND_LO:
    CC = PatmosCC::COND_HS;
    break;
  }

  Cond[0].setImm(CC);
  return false;
}

bool PatmosInstrInfo::isUnpredicatedTerminator(const MachineInstr *MI) const {
  const MCInstrDesc &MCID = MI->getDesc();
  if (!MCID.isTerminator()) return false;

  // Conditional branch is a special case.
  if (MCID.isBranch() && !MCID.isBarrier())
    return true;
  if (!MCID.isPredicable())
    return true;
  return !isPredicated(MI);
}

bool PatmosInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
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

    // A terminator that isn't a branch can't easily be handled
    // by this analysis.
    if (!I->getDesc().isBranch())
      return true;

    // Cannot handle indirect branches.
    if (I->getOpcode() == Patmos::Br ||
        I->getOpcode() == Patmos::Bm)
      return true;

    // Handle unconditional branches.
    if (I->getOpcode() == Patmos::JMP) {
      if (!AllowModify) {
        TBB = I->getOperand(0).getMBB();
        continue;
      }

      // If the block has any instructions after a JMP, delete them.
      while (llvm::next(I) != MBB.end())
        llvm::next(I)->eraseFromParent();
      Cond.clear();
      FBB = 0;

      // Delete the JMP if it's equivalent to a fall-through.
      if (MBB.isLayoutSuccessor(I->getOperand(0).getMBB())) {
        TBB = 0;
        I->eraseFromParent();
        I = MBB.end();
        continue;
      }

      // TBB is used to indicate the unconditinal destination.
      TBB = I->getOperand(0).getMBB();
      continue;
    }

    // Handle conditional branches.
    assert(I->getOpcode() == Patmos::JCC && "Invalid conditional branch");
    PatmosCC::CondCodes BranchCode =
      static_cast<PatmosCC::CondCodes>(I->getOperand(1).getImm());
    if (BranchCode == PatmosCC::COND_INVALID)
      return true;  // Can't handle weird stuff.

    // Working from the bottom, handle the first conditional branch.
    if (Cond.empty()) {
      FBB = TBB;
      TBB = I->getOperand(0).getMBB();
      Cond.push_back(MachineOperand::CreateImm(BranchCode));
      continue;
    }

    // Handle subsequent conditional branches. Only handle the case where all
    // conditional branches branch to the same destination.
    assert(Cond.size() == 1);
    assert(TBB);

    // Only handle the case where all conditional branches branch to
    // the same destination.
    if (TBB != I->getOperand(0).getMBB())
      return true;

    PatmosCC::CondCodes OldBranchCode = (PatmosCC::CondCodes)Cond[0].getImm();
    // If the conditions are the same, we can leave them alone.
    if (OldBranchCode == BranchCode)
      continue;

    return true;
  }

  return false;
}

unsigned
PatmosInstrInfo::InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                              MachineBasicBlock *FBB,
                              const SmallVectorImpl<MachineOperand> &Cond,
                              DebugLoc DL) const {
  // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "Patmos branch conditions have one component!");

  if (Cond.empty()) {
    // Unconditional branch?
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(Patmos::JMP)).addMBB(TBB);
    return 1;
  }

  // Conditional branch.
  unsigned Count = 0;
  BuildMI(&MBB, DL, get(Patmos::JCC)).addMBB(TBB).addImm(Cond[0].getImm());
  ++Count;

  if (FBB) {
    // Two-way Conditional branch. Insert the second branch.
    BuildMI(&MBB, DL, get(Patmos::JMP)).addMBB(FBB);
    ++Count;
  }
  return Count;
}

/// GetInstSize - Return the number of bytes of code the specified
/// instruction may be.  This returns the maximum number of bytes.
///
unsigned PatmosInstrInfo::GetInstSizeInBytes(const MachineInstr *MI) const {
  const MCInstrDesc &Desc = MI->getDesc();

  switch (Desc.TSFlags & PatmosII::SizeMask) {
  default:
    switch (Desc.getOpcode()) {
    default:
      assert(0 && "Unknown instruction size!");
    case TargetOpcode::PROLOG_LABEL:
    case TargetOpcode::EH_LABEL:
    case TargetOpcode::IMPLICIT_DEF:
    case TargetOpcode::KILL:
    case TargetOpcode::DBG_VALUE:
      return 0;
    case TargetOpcode::INLINEASM: {
      const MachineFunction *MF = MI->getParent()->getParent();
      const TargetInstrInfo &TII = *MF->getTarget().getInstrInfo();
      return TII.getInlineAsmLength(MI->getOperand(0).getSymbolName(),
                                    *MF->getTarget().getMCAsmInfo());
    }
    }
  case PatmosII::SizeSpecial:
    switch (MI->getOpcode()) {
    default:
      assert(0 && "Unknown instruction size!");
    case Patmos::SAR8r1c:
    case Patmos::SAR16r1c:
      return 4;
    }
  case PatmosII::Size2Bytes:
    return 2;
  case PatmosII::Size4Bytes:
    return 4;
  case PatmosII::Size6Bytes:
    return 6;
  }

  return 6;
}

#endif
