//===- PatmosInstrInfo.h - Patmos Instruction Information -------*- C++ -*-===//
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

#ifndef _LLVM_TARGET_PATMOS_INSTRINFO_H_
#define _LLVM_TARGET_PATMOS_INSTRINFO_H_

#include "llvm/Target/TargetInstrInfo.h"
#include "PatmosRegisterInfo.h"

#define GET_INSTRINFO_HEADER
#include "PatmosGenInstrInfo.inc"

namespace llvm {

class PatmosTargetMachine;

class PatmosInstrInfo : public PatmosGenInstrInfo {
  const PatmosRegisterInfo RI;
  PatmosTargetMachine &TM;
public:
  explicit PatmosInstrInfo(PatmosTargetMachine &TM);

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  ///
  virtual const TargetRegisterInfo &getRegisterInfo() const { return RI; }

  void copyPhysReg(MachineBasicBlock &MBB,
                   MachineBasicBlock::iterator I, DebugLoc DL,
                   unsigned DestReg, unsigned SrcReg,
                   bool KillSrc) const;

  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MI,
                           unsigned SrcReg, bool isKill,
                           int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            unsigned DestReg, int FrameIdx,
                            const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const;

};

}

#endif // _LLVM_TARGET_PATMOS_INSTRINFO_H_
