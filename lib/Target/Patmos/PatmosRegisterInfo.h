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

  /// computeLargeFIOffset - Emit an ADDi or ADDl instruction to compute a large 
  /// FI offset.
  /// \note The offset and basePtr arguments are possibly updated!
  void computeLargeFIOffset(MachineRegisterInfo &MRI,
                            int &offset, unsigned &basePtr,
                            MachineBasicBlock::iterator II,
                            int shl) const;

  /// expandPseudoPregInstr - expand PSEUDO_PREG_SPILL or PSEUDO_PREG_RELOAD
  /// to a sequence of real machine instructions.
  void expandPseudoPregInstr(MachineBasicBlock::iterator II,
                             int offset, unsigned basePtr,
                             bool isOnStackCache) const;
public:
  PatmosRegisterInfo(PatmosTargetMachine &tm, const TargetInstrInfo &tii);

  /// get the associate patmos target machine
  PatmosTargetMachine& getTargetMachine() const { return TM; }

  /// Code Generation virtual methods...
  const uint16_t *getCalleeSavedRegs(const MachineFunction *MF = 0) const;

  BitVector getReservedRegs(const MachineFunction &MF) const;


  const TargetRegisterClass *
  getMatchingSuperRegClass(const TargetRegisterClass *A,
                           const TargetRegisterClass *B, unsigned Idx) const {
    // No sub-classes makes this really easy.
    return A;
  }

  bool isRReg(unsigned RegNo) const;
  bool isSReg(unsigned RegNo) const;
  bool isPReg(unsigned RegNo) const;

  /// hasReservedSpillSlot - Return true if target has reserved a spill slot in
  /// the stack frame of the given function for the specified register. e.g. On
  /// x86, if the frame register is required, the first fixed stack object is
  /// reserved as its spill slot. This tells PEI not to create a new stack frame
  /// object for the given register. It should be called only after
  /// processFunctionBeforeCalleeSavedScan().
  virtual bool hasReservedSpillSlot(const MachineFunction &MF, unsigned Reg,
                                    int &FrameIdx) const;

  /// requiresRegisterScavenging - returns true if the target requires (and can
  /// make use of) the register scavenger.
  virtual bool requiresRegisterScavenging(const MachineFunction &MF) const;

  /// requiresFrameIndexScavenging - returns true if the target requires post
  /// PEI scavenging of registers for materializing frame index constants.
  virtual bool requiresFrameIndexScavenging(const MachineFunction &MF) const {
    return false; //FIXME
  }

  virtual bool trackLivenessAfterRegAlloc(const MachineFunction &MF) const {
    return true;
  }

  virtual void eliminateFrameIndex(MachineBasicBlock::iterator II,
                                   int SPAdj, unsigned FIOperandNum,
		                   RegScavenger *RS = NULL) const;

  // Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const;

};

} // end namespace llvm

#endif // _LLVM_TARGET_PATMOS_REGISTERINFO_H_
