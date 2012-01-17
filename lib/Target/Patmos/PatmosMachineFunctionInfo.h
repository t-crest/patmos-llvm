//===- PatmosMachineFuctionInfo.h - Patmos machine function info -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares Patmos-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_MACHINEFUNCTIONINFO_H_
#define _PATMOS_MACHINEFUNCTIONINFO_H_

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

/// PatmosMachineFunctionInfo - This class is derived from MachineFunction and
/// contains private Patmos target-specific information for each MachineFunction.
class PatmosMachineFunctionInfo : public MachineFunctionInfo {
  /// CalleeSavedFrameSize - Size of the callee-saved register portion of the
  /// stack frame in bytes.
  unsigned CalleeSavedFrameSize;

  /// ReturnAddrIndex - FrameIndex for return slot.
  int ReturnAddrIndex;

public:
  PatmosMachineFunctionInfo() : CalleeSavedFrameSize(0) {}

  explicit PatmosMachineFunctionInfo(MachineFunction &MF)
    : CalleeSavedFrameSize(0), ReturnAddrIndex(0) {}

  unsigned getCalleeSavedFrameSize() const { return CalleeSavedFrameSize; }
  void setCalleeSavedFrameSize(unsigned bytes) { CalleeSavedFrameSize = bytes; }

  int getRAIndex() const { return ReturnAddrIndex; }
  void setRAIndex(int Index) { ReturnAddrIndex = Index; }
};

} // End llvm namespace

#endif // _PATMOS_MACHINEFUNCTIONINFO_H_
