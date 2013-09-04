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
                           "(default 32)."));

/// MethodCacheSize - Total size of the method cache in bytes.
static cl::opt<unsigned> MethodCacheSize("mpatmos-method-cache-size",
                     cl::init(1024),
                     cl::desc("Total size of the instruction cache in bytes "
                              "(default 1024)"));

static cl::opt<bool> DisableVLIW("mpatmos-disable-vliw",
	             cl::init(true),
		     cl::desc("Schedule instructions only in first slot."));

static cl::opt<bool> DisableMIPreRA("mpatmos-disable-pre-ra-misched",
                     cl::init(true),
                     cl::desc("Disable any pre-RA MI scheduler."));

static cl::opt<bool> DisablePostRA("mpatmos-disable-post-ra",
                     cl::init(true),
                     cl::desc("Disable any post-RA scheduling."));

static cl::opt<bool> DisablePatmosPostRA("mpatmos-disable-post-ra-patmos",
                     cl::init(false),
                     cl::desc("Use the standard LLVM post-RA scheduler instead "
                              "of the Patmos post-RA scheduler."));


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
}

bool PatmosSubtarget::enablePostRAScheduler(CodeGenOpt::Level OptLevel,
                                   TargetSubtargetInfo::AntiDepBreakMode& Mode,
                                   RegClassVector& CriticalPathRCs) const {
  return hasPostRAScheduler(OptLevel) && !usePatmosPostRAScheduler(OptLevel);
}

bool PatmosSubtarget::enableBundling(CodeGenOpt::Level OptLevel) const {
  return !DisableVLIW;
}

bool PatmosSubtarget::hasPostRAScheduler(CodeGenOpt::Level OptLevel) const {

  // TargetPassConfig does not add the PostRA pass for -O0!
  if (OptLevel == CodeGenOpt::None) return false;

  // TODO there are also -disable-post-ra and -post-RA-scheduler flags,
  // which override the default postRA scheduler behavior, be basically ignore
  // them for now.
  return !DisablePostRA;
}

bool PatmosSubtarget::usePreRAMIScheduler(CodeGenOpt::Level OptLevel) const {

  if (OptLevel == CodeGenOpt::None) return false;

  return !DisableMIPreRA;
}

bool PatmosSubtarget::usePatmosPostRAScheduler(CodeGenOpt::Level OptLevel) const {
  return hasPostRAScheduler(OptLevel) && !DisablePatmosPostRA;
}


bool PatmosSubtarget::canIssueInSlot(unsigned SchedClass, unsigned Slot) const {
  const InstrStage* IS = InstrItins.beginStage(SchedClass);
  unsigned FuncUnits = IS->getUnits();

  switch (Slot) {
  case 0:
    return FuncUnits & PatmosGenericItinerariesFU::FU_ALU0;
  case 1:
    return FuncUnits & PatmosGenericItinerariesFU::FU_ALU1;
  default:
    return false;
  }
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
