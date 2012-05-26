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
      SrcOpIdx1 = 3;
      SrcOpIdx2 = 4;
      return true;
      break;
    default:
      assert("Unexpected commutable machine instruction." && false);
      return false;
  }

  abort();
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

