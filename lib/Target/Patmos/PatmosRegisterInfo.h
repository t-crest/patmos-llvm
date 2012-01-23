//===- PatmosRegisterInfo.h - Patmos Register Information Impl --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Patmos implementation of the MRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_TARGET_PATMOS_REGISTERINFO_H_
#define _LLVM_TARGET_PATMOS_REGISTERINFO_H_

#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "PatmosGenRegisterInfo.inc"

namespace llvm {

class TargetInstrInfo;
class PatmosTargetMachine;

struct PatmosRegisterInfo : public PatmosGenRegisterInfo {
private:
  PatmosTargetMachine &TM;
  const TargetInstrInfo &TII;

  /// StackAlign - Default stack alignment.
  ///
  unsigned StackAlign;
public:
  PatmosRegisterInfo(PatmosTargetMachine &tm, const TargetInstrInfo &tii);

  /// Code Generation virtual methods...
  const unsigned *getCalleeSavedRegs(const MachineFunction *MF = 0) const;//XXX

  BitVector getReservedRegs(const MachineFunction &MF) const;//XXX

  const TargetRegisterClass *
  getMatchingSuperRegClass(const TargetRegisterClass *A,
                           const TargetRegisterClass *B, unsigned Idx) const {
    // No sub-classes makes this really easy.
    return A;
  }

#if 0
  const TargetRegisterClass* getPointerRegClass(unsigned Kind = 0) const;

  void eliminateCallFramePseudoInstr(MachineFunction &MF,
                                     MachineBasicBlock &MBB,
                                     MachineBasicBlock::iterator I) const;

  void processFunctionBeforeFrameFinalized(MachineFunction &MF) const;
#endif

  void eliminateFrameIndex(MachineBasicBlock::iterator II,
                           int SPAdj, RegScavenger *RS = NULL) const;//XXX

  // Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const;//XXX
};

} // end namespace llvm

#endif // _LLVM_TARGET_PATMOS_REGISTERINFO_H_
