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
  : PatmosGenRegisterInfo(Patmos::R1), TM(tm), TII(tii) {
  StackAlign = TM.getFrameLowering()->getStackAlignment();
}

const uint16_t*
PatmosRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
  const TargetFrameLowering *TFI = MF->getTarget().getFrameLowering();
  //const Function* F = MF->getFunction();
  static const uint16_t CalleeSavedRegs[] = {
    // Special regs
    Patmos::SZ, Patmos::SB, Patmos::SO,
    // GPR
    Patmos::R21, Patmos::R22, Patmos::R23, Patmos::R24,
    Patmos::R25, Patmos::R26, Patmos::R27, Patmos::R28, Patmos::R29,
    // Predicate regs
    Patmos::P1, Patmos::P2, Patmos::P3, Patmos::P4,
    Patmos::P5, Patmos::P6, Patmos::P7,
    0
  };
  static const uint16_t CalleeSavedRegsFP[] = {
    // Special regs
    Patmos::SZ, Patmos::SB, Patmos::SO,
    // GPR
    Patmos::R21, Patmos::R22, Patmos::R23, Patmos::R24,
    Patmos::R25, Patmos::R26, Patmos::R27, Patmos::R28, Patmos::R29,
    Patmos::RFP,
    // Predicate regs
    Patmos::P1, Patmos::P2, Patmos::P3, Patmos::P4,
    Patmos::P5, Patmos::P6, Patmos::P7,
    0
  };

  return (TFI->hasFP(*MF)) ? CalleeSavedRegsFP : CalleeSavedRegs;

}

BitVector PatmosRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  Reserved.set(Patmos::R0);
  Reserved.set(Patmos::P0);

  // All the special registers are reserved
  Reserved.set(Patmos::SZ);
  Reserved.set(Patmos::SM);
  Reserved.set(Patmos::SL);
  Reserved.set(Patmos::SH);
  Reserved.set(Patmos::SB);
  Reserved.set(Patmos::SO);
  Reserved.set(Patmos::ST);
  Reserved.set(Patmos::S7);
  Reserved.set(Patmos::S8);
  Reserved.set(Patmos::S9);
  Reserved.set(Patmos::S10);
  Reserved.set(Patmos::S11);
  Reserved.set(Patmos::S12);
  Reserved.set(Patmos::S13);
  Reserved.set(Patmos::S14);
  Reserved.set(Patmos::S15);

  // stack pointer
  Reserved.set(Patmos::RSP);
  // Mark frame pointer as reserved if needed.
  if (TFI->hasFP(MF))
    Reserved.set(Patmos::RFP);

  return Reserved;
}

#if 0
const TargetRegisterClass *
PatmosRegisterInfo::getPointerRegClass(unsigned Kind) const {
  return &Patmos::GR16RegClass;
}
#endif

void PatmosRegisterInfo::
eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  // Simply discard ADJCALLSTACKDOWN, ADJCALLSTACKUP instructions.
  MBB.erase(I);
}

#if 0
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

#endif

void
PatmosRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                        int SPAdj, RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  // get some references
  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();
  //DebugLoc dl = MI.getDebugLoc();

  // find position of the FrameIndex object
  unsigned i = 0;
  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() && "Instr doesn't have FrameIndex operand!");
  }

  // expect FrameIndex to be on second position for load/store operations
  // TODO: position in other expressions than load/store!
  assert(i == 2 || i == 3);

  // get information on FrameIndex's stack slot 
  int FrameIndex = MI.getOperand(i).getIndex(); 
  unsigned BasePtr = (TFI->hasFP(MF) ? Patmos::RFP : Patmos::RSP);
  int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex);

  // adjust stack offset
  if (!TFI->hasFP(MF))
    Offset += MF.getFrameInfo()->getStackSize();
  else
    Offset += 2; // Skip the saved RFP

  // Fold imm into offset
  Offset += MI.getOperand(i+1).getImm();

  // ensure that the offset fits the instruction
  switch (MI.getOpcode())
  {
    case Patmos::LWC: case Patmos::LWM: 
    case Patmos::SWC: case Patmos::SWM:
      // 9 bit
      Offset = Offset >> 2;
      assert(isInt<7>(Offset));
      break;
    case Patmos::LHC: case Patmos::LHM:
    case Patmos::LHUC: case Patmos::LHUM:
    case Patmos::SHC: case Patmos::SHM:
      // 8 bit
      Offset = Offset >> 1;
      assert(isInt<7>(Offset));
      break;
    case Patmos::LBC: case Patmos::LBM:
    case Patmos::LBUC: case Patmos::LBUM:
    case Patmos::SBC: case Patmos::SBM:
      // 7 bit
      assert(isInt<7>(Offset));
      break;
    case Patmos::ADDi:
      // 12 bit
      assert(isUInt<12>(Offset));
      break;
    case Patmos::ADDl:
      // all should be fine
      break;
    default:
      assert("Unexpected operation with FrameIndex encountered." && false);
      abort();
  }

  // update the instruction's operands
  MI.getOperand(i).ChangeToRegister(BasePtr, false);
  MI.getOperand(i+1).ChangeToImmediate(Offset);
}

unsigned PatmosRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  return TFI->hasFP(MF) ? Patmos::RFP : Patmos::RSP;
}
