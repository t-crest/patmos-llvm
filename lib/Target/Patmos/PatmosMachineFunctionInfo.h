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

#include <limits>

namespace llvm {

/// PatmosMachineFunctionInfo - This class is derived from MachineFunction and
/// contains private Patmos target-specific information for each 
/// MachineFunction.
class PatmosMachineFunctionInfo : public MachineFunctionInfo {
  /// StackCacheReservedSize - Size in bytes that need to be reserved on the
  /// stack cache.
  unsigned StackCacheReservedBytes;

  /// FistStackCacheOffset - Offset of the first FI assigned to the stack cache.
  int FirstStackCacheOffset;

  /// LastStackCacheOffset - Offset of the last FI assigned to the stack cache.
  int LastStackCacheOffset;

  /// VarArgsFI - FrameIndex to access parameters of variadic functions.
  int VarArgsFI;

  // do not provide any default constructor.
  PatmosMachineFunctionInfo() {}
public:
  explicit PatmosMachineFunctionInfo(MachineFunction &MF) :
    StackCacheReservedBytes(0),
    FirstStackCacheOffset(std::numeric_limits<int>::max()),
    LastStackCacheOffset(std::numeric_limits<int>::max()), VarArgsFI(0) {
  }

  /// getFirstStackCacheOffset - Return the offset of the first FI assigned to 
  /// the stack cache.
  int getFirstStackCacheOffset() const {
    return FirstStackCacheOffset;
  }

  /// setFirstStackCacheFI - Set the offset of the First FI assigned to the 
  /// stack cache.
  void setFirstStackCacheOffset(int newIndex) {
    FirstStackCacheOffset = newIndex;
  }

  /// getLastStackCacheOffset - Return the offset of the last FI assigned to the
  /// stack cache.
  int getLastStackCacheOffset() const {
    return LastStackCacheOffset;
  }

  /// setLastStackCacheFI - Set the offset of the last FI assigned to the stack
  /// cache.
  void setLastStackCacheOffset(int newIndex) {
    LastStackCacheOffset = newIndex;
  }

  /// getStackCacheReservedBytes - Get the number of bytes reserved on the
  /// stack cache.
  unsigned getStackCacheReservedBytes() const {
    return StackCacheReservedBytes;
  }

  /// setStackCacheReservedBytes - Set the number of bytes reserved on the stack
  /// cache.
  void setStackCacheReservedBytes(unsigned newSize) {
    StackCacheReservedBytes = newSize;
  }

  /// getVarArgsFI - Get the FI used to access parameters of variadic functions.
  unsigned getVarArgsFI() const {
    return VarArgsFI;
  }

  /// setVarArgsFI - Set the FI used to access parameters of variadic functions.
  void setVarArgsFI(int newFI) {
    VarArgsFI = newFI;
  }
};

} // End llvm namespace

#endif // _PATMOS_MACHINEFUNCTIONINFO_H_
