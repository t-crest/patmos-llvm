//===- PatmosSubtarget.cpp - Patmos Subtarget Information ---------*- C++ -*-=//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Patmos specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "PatmosSubtarget.h"
#include "Patmos.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/ADT/StringExtras.h"


#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "PatmosGenSubtargetInfo.inc"

using namespace llvm;


/// StackCacheBlockSize - Block size of the stack cache in bytes (default: 4,
/// i.e., word-sized).
static cl::opt<unsigned> StackCacheBlockSize("mpatmos-stack-cache-block-size",
                           cl::init(4),
                           cl::desc("Block size of the stack cache in bytes."));

/// StackCacheSize - Total size of the stack cache in bytes (default: 4096,
/// i.e., 1K words).
static cl::opt<unsigned> StackCacheSize("mpatmos-stack-cache-size",
                           cl::init(4096),
                           cl::desc("Total size of the stack cache in bytes."));

/// MethodCacheBlockSize - Block size of the method cache in bytes.
static cl::opt<unsigned> MethodCacheBlockSize("mpatmos-method-cache-block-size",
                   cl::init(32),
                   cl::desc("Size of the instruction cache blocks in bytes "
                           "(defaults to 32)."));

/// MethodCacheSize - Total size of the method cache in bytes.
static cl::opt<unsigned> MethodCacheSize("mpatmos-method-cache-size",
                     cl::init(2048),
                     cl::desc("Total size of the instruction cache in bytes "
                              "(default 2048)"));



PatmosSubtarget::PatmosSubtarget(const std::string &TT,
                                 const std::string &CPU,
                                 const std::string &FS) :
  PatmosGenSubtargetInfo(TT, CPU, FS)
{
  std::string CPUName = CPU;
  if (CPUName.empty()) CPUName = "generic";

  // Parse features string.
  ParseSubtargetFeatures(CPUName, FS);

  InstrItins = getInstrItineraryForCPU(CPUName);

  InstrItins.IssueWidth = 1;
}

unsigned PatmosSubtarget::getStackCacheSize() const {
  return StackCacheSize;
}

unsigned PatmosSubtarget::getStackCacheBlockSize() const {
  return StackCacheBlockSize;
}

unsigned PatmosSubtarget::getMethodCacheSize() const {
  return MethodCacheSize;
}

unsigned PatmosSubtarget::getMethodCacheBlockSize() const {
  return MethodCacheBlockSize;
}
