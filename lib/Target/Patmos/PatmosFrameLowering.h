//==- PatmosFrameLowering.h - Define frame lowering for Patmos --*- C++ -*--==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_FRAMEINFO_H_
#define _PATMOS_FRAMEINFO_H_

#include "Patmos.h"
#include "PatmosSubtarget.h"
#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {
  class PatmosSubtarget;

class PatmosFrameLowering : public TargetFrameLowering {
protected:
  const PatmosSubtarget &STI;

public:
  explicit PatmosFrameLowering(const PatmosSubtarget &sti)
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 4, 0), STI(sti) {
  }

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction &MF) const;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const;

  bool hasFP(const MachineFunction &MF) const;

  /// processFunctionBeforeCalleeSavedScan - This method is called immediately
  /// before PrologEpilogInserter scans the physical registers used to determine
  /// what callee saved registers should be spilled. This method is optional.
  virtual void processFunctionBeforeCalleeSavedScan(MachineFunction &MF,
                                                 RegScavenger *RS = NULL) const;
  bool spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MI,
                                 const std::vector<CalleeSavedInfo> &CSI,
                                 const TargetRegisterInfo *TRI) const;
  bool restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                   MachineBasicBlock::iterator MI,
                                   const std::vector<CalleeSavedInfo> &CSI,
                                   const TargetRegisterInfo *TRI) const;
#if 0
  bool hasReservedCallFrame(const MachineFunction &MF) const;
#endif

};

} // End llvm namespace

#endif // _PATMOS_FRAMEINFO_H_
