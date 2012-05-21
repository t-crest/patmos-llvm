//======-- PatmosFrameLowering.cpp - Patmos Frame Information -------=========//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Patmos implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "PatmosFrameLowering.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

static unsigned AlignOffset(unsigned Offset, unsigned Align) {
  return (Offset + Align - 1) / Align * Align;
}

bool PatmosFrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();

  //return (MF.getTarget().Options.DisableFramePointerElim(MF) ||
  return (DisableFramePointerElim(MF) ||
          MF.getFrameInfo()->hasVarSizedObjects() ||
          MFI->isFrameAddressTaken());
}

#if 0
bool PatmosFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  return !MF.getFrameInfo()->hasVarSizedObjects();
}
#endif

void PatmosFrameLowering::emitPrologue(MachineFunction &MF) const {
  // get some references
  MachineBasicBlock &MBB   = MF.front();
  MachineFrameInfo *MFI    = MF.getFrameInfo();
  // TODO: track max. size of stack used for passing arguments
  // PatmosFunctionInfo *PFI  = MF.getInfo<PatmosFunctionInfo>();
  const PatmosInstrInfo &TII =
    *static_cast<const PatmosInstrInfo*>(MF.getTarget().getInstrInfo());


  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // First, compute final stack size.
  unsigned StackAlign = getStackAlignment();
  // TODO: track max. size of stack used for passing arguments
  unsigned LocalVarAreaOffset = 0; // PFI->getMaxCallFrameSize();
  unsigned StackSize = AlignOffset(LocalVarAreaOffset, StackAlign) +
    AlignOffset(MFI->getStackSize(), StackAlign);

   // Update stack size
  MFI->setStackSize(StackSize);

  // No need to allocate space on the stack.
  if (StackSize == 0 && !MFI->adjustsStack()) return;

  // adjust stack : sp -= stack size
  if (StackSize <= 0xFFF) {
    BuildMI(MBB, MBBI, dl, TII.get(Patmos::SUBi), Patmos::RSP)
      .addReg(Patmos::P0).addImm(0) // predicate: always true
      .addReg(Patmos::RSP).addImm(StackSize);
  }
  else {
    BuildMI(MBB, MBBI, dl, TII.get(Patmos::SUBl), Patmos::RSP)
      .addReg(Patmos::P0).addImm(0) // predicate: always true
      .addReg(Patmos::RSP).addImm(StackSize);
  }

  // if framepointer enabled, set it to point to the stack pointer.
  if (hasFP(MF)) {
    // Set frame pointer: FP = SP
    BuildMI(MBB, MBBI, dl, TII.get(Patmos::MOV), Patmos::RFP)
      .addReg(Patmos::P0).addImm(0) // predicate: always true
      .addReg(Patmos::RSP);
  }
}

void PatmosFrameLowering::emitEpilogue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  MachineFrameInfo *MFI            = MF.getFrameInfo();
  const PatmosInstrInfo &TII =
    *static_cast<const PatmosInstrInfo*>(MF.getTarget().getInstrInfo());
  DebugLoc dl = MBBI->getDebugLoc();

  // Get the number of bytes from FrameInfo
  unsigned StackSize = MFI->getStackSize();

  // if framepointer enabled, restore the stack pointer.
  if (hasFP(MF)) {
    // Find the first instruction that restores a callee-saved register.
    MachineBasicBlock::iterator I = MBBI;

    for (unsigned i = 0; i < MFI->getCalleeSavedInfo().size(); ++i)
      --I;

    // Restore stack pointer: SP = FP
    BuildMI(MBB, I, dl, TII.get(Patmos::MOV), Patmos::RSP)
      .addReg(Patmos::P0).addImm(0) // predicate: always true
      .addReg(Patmos::RFP);
  }

  // adjust stack  : sp += stack size
  if (StackSize) {
    if (StackSize <= 0xFFF) {
      BuildMI(MBB, MBBI, dl, TII.get(Patmos::ADDi), Patmos::RSP)
        .addReg(Patmos::P0).addImm(0) // predicate: always true
        .addReg(Patmos::RSP).addImm(StackSize);
    }
    else {
      BuildMI(MBB, MBBI, dl, TII.get(Patmos::ADDl), Patmos::RSP)
        .addReg(Patmos::P0).addImm(0) // predicate: always true
        .addReg(Patmos::RSP).addImm(StackSize);
    }
  }
}


#if 0
// FIXME: Can we eleminate these in favour of generic code?
bool
PatmosFrameLowering::spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                           MachineBasicBlock::iterator MI,
                                        const std::vector<CalleeSavedInfo> &CSI,
                                        const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return false;

  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *MF.getTarget().getInstrInfo();
  PatmosMachineFunctionInfo *MFI = MF.getInfo<PatmosMachineFunctionInfo>();
  MFI->setCalleeSavedFrameSize(CSI.size() * 2);

  for (unsigned i = CSI.size(); i != 0; --i) {
    unsigned Reg = CSI[i-1].getReg();
    // Add the callee-saved register as live-in. It's killed at the spill.
    MBB.addLiveIn(Reg);
    BuildMI(MBB, MI, DL, TII.get(Patmos::PUSH16r))
      .addReg(Reg, RegState::Kill);
  }
  return true;
}

bool
PatmosFrameLowering::restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                                 MachineBasicBlock::iterator MI,
                                        const std::vector<CalleeSavedInfo> &CSI,
                                        const TargetRegisterInfo *TRI) const {
  if (CSI.empty())
    return false;

  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  MachineFunction &MF = *MBB.getParent();
  const TargetInstrInfo &TII = *MF.getTarget().getInstrInfo();

  for (unsigned i = 0, e = CSI.size(); i != e; ++i)
    BuildMI(MBB, MI, DL, TII.get(Patmos::POP16r), CSI[i].getReg());

  return true;
}

#endif
