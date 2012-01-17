//===- PatmosRegisterInfo.cpp - Patmos Register Information ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Patmos implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-reg-info"

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosRegisterInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/Support/ErrorHandling.h"

#define GET_REGINFO_TARGET_DESC
#include "PatmosGenRegisterInfo.inc"

using namespace llvm;

// FIXME: Provide proper call frame setup / destroy opcodes.
PatmosRegisterInfo::PatmosRegisterInfo(PatmosTargetMachine &tm,
                                       const TargetInstrInfo &tii)
  : PatmosGenRegisterInfo(Patmos::PCW), TM(tm), TII(tii) {
  StackAlign = TM.getFrameLowering()->getStackAlignment();
}

const unsigned*
PatmosRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  const TargetFrameLowering *TFI = MF->getTarget().getFrameLowering();
  const Function* F = MF->getFunction();
  static const unsigned CalleeSavedRegs[] = {
    Patmos::FPW, Patmos::R5W, Patmos::R6W, Patmos::R7W,
    Patmos::R8W, Patmos::R9W, Patmos::R10W, Patmos::R11W,
    0
  };
  static const unsigned CalleeSavedRegsFP[] = {
    Patmos::R5W, Patmos::R6W, Patmos::R7W,
    Patmos::R8W, Patmos::R9W, Patmos::R10W, Patmos::R11W,
    0
  };
  static const unsigned CalleeSavedRegsIntr[] = {
    Patmos::FPW,  Patmos::R5W,  Patmos::R6W,  Patmos::R7W,
    Patmos::R8W,  Patmos::R9W,  Patmos::R10W, Patmos::R11W,
    Patmos::R12W, Patmos::R13W, Patmos::R14W, Patmos::R15W,
    0
  };
  static const unsigned CalleeSavedRegsIntrFP[] = {
    Patmos::R5W,  Patmos::R6W,  Patmos::R7W,
    Patmos::R8W,  Patmos::R9W,  Patmos::R10W, Patmos::R11W,
    Patmos::R12W, Patmos::R13W, Patmos::R14W, Patmos::R15W,
    0
  };

  if (TFI->hasFP(*MF))
    return (F->getCallingConv() == CallingConv::MSP430_INTR ? // FIXME
            CalleeSavedRegsIntrFP : CalleeSavedRegsFP);
  else
    return (F->getCallingConv() == CallingConv::MSP430_INTR ? // FIXME
            CalleeSavedRegsIntr : CalleeSavedRegs);

}

BitVector PatmosRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  // Mark 4 special registers with subregisters as reserved.
  Reserved.set(Patmos::PCB);
  Reserved.set(Patmos::SPB);
  Reserved.set(Patmos::SRB);
  Reserved.set(Patmos::CGB);
  Reserved.set(Patmos::PCW);
  Reserved.set(Patmos::SPW);
  Reserved.set(Patmos::SRW);
  Reserved.set(Patmos::CGW);

  // Mark frame pointer as reserved if needed.
  if (TFI->hasFP(MF))
    Reserved.set(Patmos::FPW);

  return Reserved;
}

const TargetRegisterClass *
PatmosRegisterInfo::getPointerRegClass(unsigned Kind) const {
  return &Patmos::GR16RegClass;
}

void PatmosRegisterInfo::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  if (!TFI->hasReservedCallFrame(MF)) {
    // If the stack pointer can be changed after prologue, turn the
    // adjcallstackup instruction into a 'sub SPW, <amt>' and the
    // adjcallstackdown instruction into 'add SPW, <amt>'
    // TODO: consider using push / pop instead of sub + store / add
    MachineInstr *Old = I;
    uint64_t Amount = Old->getOperand(0).getImm();
    if (Amount != 0) {
      // We need to keep the stack aligned properly.  To do this, we round the
      // amount of space needed for the outgoing arguments up to the next
      // alignment boundary.
      Amount = (Amount+StackAlign-1)/StackAlign*StackAlign;

      MachineInstr *New = 0;
      if (Old->getOpcode() == TII.getCallFrameSetupOpcode()) {
        New = BuildMI(MF, Old->getDebugLoc(),
                      TII.get(Patmos::SUB16ri), Patmos::SPW)
          .addReg(Patmos::SPW).addImm(Amount);
      } else {
        assert(Old->getOpcode() == TII.getCallFrameDestroyOpcode());
        // factor out the amount the callee already popped.
        uint64_t CalleeAmt = Old->getOperand(1).getImm();
        Amount -= CalleeAmt;
        if (Amount)
          New = BuildMI(MF, Old->getDebugLoc(),
                        TII.get(Patmos::ADD16ri), Patmos::SPW)
            .addReg(Patmos::SPW).addImm(Amount);
      }

      if (New) {
        // The SRW implicit def is dead.
        New->getOperand(3).setIsDead();

        // Replace the pseudo instruction with a new instruction...
        MBB.insert(I, New);
      }
    }
  } else if (I->getOpcode() == TII.getCallFrameDestroyOpcode()) {
    // If we are performing frame pointer elimination and if the callee pops
    // something off the stack pointer, add it back.
    if (uint64_t CalleeAmt = I->getOperand(1).getImm()) {
      MachineInstr *Old = I;
      MachineInstr *New =
        BuildMI(MF, Old->getDebugLoc(), TII.get(Patmos::SUB16ri),
                Patmos::SPW).addReg(Patmos::SPW).addImm(CalleeAmt);
      // The SRW implicit def is dead.
      New->getOperand(3).setIsDead();

      MBB.insert(I, New);
    }
  }

  MBB.erase(I);
}

void
PatmosRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                        int SPAdj, RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  unsigned i = 0;
  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();
  DebugLoc dl = MI.getDebugLoc();
  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() && "Instr doesn't have FrameIndex operand!");
  }

  int FrameIndex = MI.getOperand(i).getIndex();

  unsigned BasePtr = (TFI->hasFP(MF) ? Patmos::FPW : Patmos::SPW);
  int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex);

  // Skip the saved PC
  Offset += 2;

  if (!TFI->hasFP(MF))
    Offset += MF.getFrameInfo()->getStackSize();
  else
    Offset += 2; // Skip the saved FPW

  // Fold imm into offset
  Offset += MI.getOperand(i+1).getImm();

  if (MI.getOpcode() == Patmos::ADD16ri) {
    // This is actually "load effective address" of the stack slot
    // instruction. We have only two-address instructions, thus we need to
    // expand it into mov + add

    MI.setDesc(TII.get(Patmos::MOV16rr));
    MI.getOperand(i).ChangeToRegister(BasePtr, false);

    if (Offset == 0)
      return;

    // We need to materialize the offset via add instruction.
    unsigned DstReg = MI.getOperand(0).getReg();
    if (Offset < 0)
      BuildMI(MBB, llvm::next(II), dl, TII.get(Patmos::SUB16ri), DstReg)
        .addReg(DstReg).addImm(-Offset);
    else
      BuildMI(MBB, llvm::next(II), dl, TII.get(Patmos::ADD16ri), DstReg)
        .addReg(DstReg).addImm(Offset);

    return;
  }

  MI.getOperand(i).ChangeToRegister(BasePtr, false);
  MI.getOperand(i+1).ChangeToImmediate(Offset);
}

void
PatmosRegisterInfo::processFunctionBeforeFrameFinalized(MachineFunction &MF)
                                                                         const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  // Create a frame entry for the FPW register that must be saved.
  if (TFI->hasFP(MF)) {
    int FrameIdx = MF.getFrameInfo()->CreateFixedObject(2, -4, true);
    (void)FrameIdx;
    assert(FrameIdx == MF.getFrameInfo()->getObjectIndexBegin() &&
           "Slot for FPW register must be last in order to be found!");
  }
}

unsigned PatmosRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  return TFI->hasFP(MF) ? Patmos::FPW : Patmos::SPW;
}
