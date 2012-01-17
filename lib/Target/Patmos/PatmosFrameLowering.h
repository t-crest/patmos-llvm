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
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 2, -2), STI(sti) {
  }

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction &MF) const;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const;

  bool spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MI,
                                 const std::vector<CalleeSavedInfo> &CSI,
                                 const TargetRegisterInfo *TRI) const;
  bool restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                   MachineBasicBlock::iterator MI,
                                   const std::vector<CalleeSavedInfo> &CSI,
                                   const TargetRegisterInfo *TRI) const;

  bool hasFP(const MachineFunction &MF) const;
  bool hasReservedCallFrame(const MachineFunction &MF) const;
};

} // End llvm namespace

#endif // _PATMOS_FRAMEINFO_H_
