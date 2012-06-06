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
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "PatmosRegisterInfo.h"
#include "MCTargetDesc/PatmosMCTargetDesc.h"

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

  /// findCommutedOpIndices - If specified MI is commutable, return the two
  /// operand indices that would swap value. Return false if the instruction
  /// is not in a form which this routine understands.
  virtual bool findCommutedOpIndices(MachineInstr *MI, unsigned &SrcOpIdx1,
                                     unsigned &SrcOpIdx2) const;

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


  // Branch handling

  virtual bool AnalyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                             MachineBasicBlock *&FBB,
                             SmallVectorImpl<MachineOperand> &Cond,
                             bool AllowModify = false) const;

  virtual unsigned RemoveBranch(MachineBasicBlock &MBB) const;

  virtual unsigned InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                                MachineBasicBlock *FBB,
                                const SmallVectorImpl<MachineOperand> &Cond,
                                DebugLoc DL) const;

  virtual bool ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const;


  MachineBasicBlock *getBranchTarget(const MachineInstr *MI) const;
};

static inline
const MachineInstrBuilder &AddDefaultPred(const MachineInstrBuilder &MIB) {
  // predicate: always true
  return MIB.addReg(Patmos::NoRegister).addImm(0);
}


static inline
bool HasDefaultPred(const MachineInstr *MI) {
  return (MI->getOperand(0).getReg() == Patmos::NoRegister)
      && (MI->getOperand(1).getImm() == 0);
}

} // end namespace llvm

#endif // _LLVM_TARGET_PATMOS_INSTRINFO_H_
