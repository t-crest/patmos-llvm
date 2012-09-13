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
#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {
  class PatmosSubtarget;
  class PatmosTargetMachine;

class PatmosFrameLowering : public TargetFrameLowering {
protected:
  const PatmosTargetMachine &TM;
  const PatmosSubtarget &STC;

  /// assignFIsToStackCache - Assign some FIs to the stack cache.
  /// Currently this is only done for spill slots.
  /// @return The final size of the shadow stack.
  unsigned assignFIsToStackCache(MachineFunction &MF) const;

  /// emitSTC - Emit a stack reserve/free/ensure operation.
  /// The size of the stack frame is calculated before by assignFIsToStackCache
  /// and is retrieved via the PatmosMachineFunctionInfo.
  /// \see assignFIsToStackCache
  /// \see PatmosMachineFunctionInfo
  void emitSTC(MachineFunction &MF, MachineBasicBlock &MBB,
               MachineBasicBlock::iterator &MI, unsigned Opcode) const;

  /// patchCallSites - Emit stack ensure operations after every call.
  /// The size of the stack frame is calculated before by assignFIsToStackCache
  /// and is retrieved via the PatmosMachineFunctionInfo.
  /// \see assignFIsToStackCache
  /// \see PatmosMachineFunctionInfo
  void patchCallSites(MachineFunction &MF) const;
  public:
  explicit PatmosFrameLowering(const PatmosTargetMachine &tm);

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
