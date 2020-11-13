//===- PatmosUtil.h - Patmos CG utilities that fit nowhere else -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains utility functions and classes that fit nowhere else
// in the code generator.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_UTIL_H_
#define _PATMOS_UTIL_H_

#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/ADT/SmallString.h"

namespace llvm {


/// getMBBIRName - Get a name for the MBB that reflects its identity in the
/// LLVM IR. If there is no mapping from IR code, an <anonymous> is used.
/// For uniqueness, the function name and current MBB number are included.
/// Format: #FunctionName#BasicBlockName#MBBNumber
void getMBBIRName(const MachineBasicBlock *MBB,
                         SmallString<128> &result);

/// Extracts loop bound information from the metadata of the block
/// terminator, if available.
///
/// The first element is the minimum iteration count.
/// The second element is the maximum iteration count.
/// If a bound is not available, -1 is returned.
std::pair<int,int> getLoopBounds(const MachineBasicBlock * MBB);

} // End llvm namespace

#endif // _PATMOS_UTIL_H_
