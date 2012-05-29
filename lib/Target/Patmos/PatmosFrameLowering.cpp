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


bool PatmosFrameLowering::hasFP(const MachineFunction &MF) const {
  const MachineFrameInfo *MFI = MF.getFrameInfo();

  return (MF.getTarget().Options.DisableFramePointerElim(MF) ||
  //return (DisableFramePointerElim(MF) ||
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
  MachineBasicBlock &MBB     = MF.front();
  MachineFrameInfo *MFI      = MF.getFrameInfo();
  const TargetInstrInfo *TII = MF.getTarget().getInstrInfo();


  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // First, get final stack size.
  unsigned maxFrameSize = MFI->getMaxCallFrameSize();
  unsigned stackSize = MFI->getStackSize() + (!hasFP(MF) ? 0 : maxFrameSize);
  MFI->setStackSize(stackSize);

  // No need to allocate space on the stack.
  if (stackSize == 0 && !MFI->adjustsStack()) return;

  // adjust stack : sp -= stack size
  if (stackSize <= 0xFFF) {
    AddDefaultPred(BuildMI(MBB, MBBI, dl, TII->get(Patmos::SUBi), Patmos::RSP))
      .addReg(Patmos::RSP).addImm(stackSize);
  }
  else {
    AddDefaultPred(BuildMI(MBB, MBBI, dl, TII->get(Patmos::SUBl), Patmos::RSP))
      .addReg(Patmos::RSP).addImm(stackSize);
  }

  // eliminate DYNALLOC instruction (aka. alloca)
  const MCInstrDesc &dynallocMCID = (maxFrameSize <= 0xFFF) ?
                                TII->get(Patmos::SUBi) : TII->get(Patmos::SUBl);

  for (MachineFunction::iterator BB(MF.begin()), E(MF.end()); BB != E; ++BB) {
    for (MachineBasicBlock::iterator MI(BB->begin()), MIE(BB->end()); MI != MIE;
         MI++) {
      // found a DYNALLOC instruction?
      if (MI->getOpcode() == Patmos::DYNALLOC) {
        // rewrite it to a sub immediate/sub immediate long
        MI->getOperand(4).setImm(maxFrameSize);
        MI->setDesc(dynallocMCID);
      }
    }
  }
}

void PatmosFrameLowering::emitEpilogue(MachineFunction &MF,
                                       MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  MachineFrameInfo *MFI            = MF.getFrameInfo();
  const TargetInstrInfo *TII       = MF.getTarget().getInstrInfo();
  DebugLoc dl                      = MBBI->getDebugLoc();

  // Get the number of bytes from FrameInfo
  unsigned stackSize = MFI->getStackSize();

  // adjust stack  : sp += stack size
  if (stackSize) {
    if (stackSize <= 0xFFF) {
      AddDefaultPred(BuildMI(MBB, MBBI, dl, TII->get(Patmos::ADDi),
                             Patmos::RSP))
        .addReg(Patmos::RSP).addImm(stackSize);
    }
    else {
      AddDefaultPred(BuildMI(MBB, MBBI, dl, TII->get(Patmos::ADDl),
                             Patmos::RSP))
        .addReg(Patmos::RSP).addImm(stackSize);
    }
  }
}

void PatmosFrameLowering::processFunctionBeforeCalleeSavedScan(
                                  MachineFunction& MF, RegScavenger* RS) const {

  MachineRegisterInfo& MRI = MF.getRegInfo();

  // Mark RFP, SB, and SO as used or unused.
  if (hasFP(MF))
    MRI.setPhysRegUsed(Patmos::RFP);

  // Mark the special registers of the method cache to be used when calls exist.
  if (MF.getFrameInfo()->hasCalls()) {
    MRI.setPhysRegUsed(Patmos::SB);
    MRI.setPhysRegUsed(Patmos::SO);
  }
  else {
    MRI.setPhysRegUnused(Patmos::SB);
    MRI.setPhysRegUnused(Patmos::SO);
  }
}

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
  PatmosMachineFunctionInfo *PMFI = MF.getInfo<PatmosMachineFunctionInfo>();

  unsigned spilledSize = 0;
  bool predicateSpilled = false;
  for (unsigned i = CSI.size(); i != 0; --i) {
    unsigned Reg = CSI[i-1].getReg();
    // Add the callee-saved register as live-in. It's killed at the spill.
    MBB.addLiveIn(Reg);

    // copy to R register first, then spill
    if (Patmos::SRegsRegClass.contains(Reg)) {
      TII.copyPhysReg(MBB, MI, DL, Patmos::R9, Reg, true);
      Reg = Patmos::R9;
    }
    else if (Patmos::PRegsRegClass.contains(Reg)) {
      if (predicateSpilled)
        // store all predicate registers at once
        continue;
      else {
        TII.copyPhysReg(MBB, MI, DL, Patmos::R9, Patmos::SZ, true);
        Reg = Patmos::R9;
        predicateSpilled = true;
      }
    }

    // spill
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(MBB, MI, Reg, true, CSI[i-1].getFrameIdx(), RC, TRI);
    prior(MI)->setFlag(MachineInstr::FrameSetup);

    // increment spilled size
    spilledSize += 4;
  }

  PMFI->setCalleeSavedFrameSize(spilledSize);

  // if framepointer enabled, set it to point to the stack pointer.
  if (hasFP(MF)) {
    // Set frame pointer: FP = SP
    AddDefaultPred(BuildMI(MBB, MI, DL, TII.get(Patmos::MOV), Patmos::RFP))
      .addReg(Patmos::RSP);
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

  // if framepointer enabled, first restore the stack pointer.
  if (hasFP(MF)) {
    // Restore stack pointer: SP = FP
    AddDefaultPred(BuildMI(MBB, MI, DL, TII.get(Patmos::MOV), Patmos::RSP))
      .addReg(Patmos::RFP);
  }

  // restore the calle saved register
  bool predicateLoaded = false;
  for (unsigned i = CSI.size(); i != 0; --i) {
    unsigned Reg = CSI[i-1].getReg();
    unsigned tmpReg = Reg;
    // Add the callee-saved register as live-in. It's killed at the spill.
    MBB.addLiveIn(Reg);

    // copy to special register after reloading
    if (Patmos::SRegsRegClass.contains(Reg))
      tmpReg = Patmos::R9;

    if (Patmos::PRegsRegClass.contains(Reg))
    {
      if (predicateLoaded)
        continue;
      else
      {
        tmpReg = Patmos::R9;
        Reg = Patmos::SZ; // load into SZ
        predicateLoaded = true;
      }
    }

    // load
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(tmpReg);
    TII.loadRegFromStackSlot(MBB, MI, tmpReg, CSI[i-1].getFrameIdx(), RC, TRI);
    prior(MI)->setFlag(MachineInstr::FrameSetup);

    // copy, if needed
    if (tmpReg != Reg)
    {
      TII.copyPhysReg(MBB, MI, DL, Reg, tmpReg, true);
    }
  }

  return true;
}
