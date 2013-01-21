//==-- PatmosSinglePathInfo.h - Class to hold information for SP CodeGen ---==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a class to hold information and configuration of
// Single-Path Code Generation.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"


using namespace llvm;


/// SPConvList - Option to enable single-path conversion.
static cl::list<std::string> SPConvList(
  "mpatmos-spconv",
  cl::value_desc("list"),
  cl::desc("A list of functions to Single-Path convert (Patmos only)."),
  cl::CommaSeparated,
  cl::Hidden);


///////////////////////////////////////////////////////////////////////////////



PatmosSinglePathInfo::PatmosSinglePathInfo(const PatmosTargetMachine &tm) : TM(tm) {

  // get the set of functions to convert as specified on command line
  SPConvFuncs.insert( SPConvList.begin(), SPConvList.end() );
  dbgs() << "[PatmosSP] Single-Path Info created.\n";
}

#if 0
  if (!SPConvFuncs.empty()) {
    DEBUG( dbgs() << "Following functions to SPConv not found:\n" );
    for (std::set<std::string>::iterator it=SPConvFuncs.begin(); it!=SPConvFuncs.end(); ++it)
      DEBUG( dbgs() << *it << ' ');
    DEBUG( dbgs() << '\n');
  }
#endif

bool PatmosSinglePathInfo::isToConvert(MachineFunction &MF) {
  return SPConvFuncs.count(MF.getFunction()->getName()) > 0;
}
