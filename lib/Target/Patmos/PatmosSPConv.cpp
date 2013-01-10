//===-- PatmosSPConv.cpp - If-convert functions for single-path code ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass if-converts functions before register allocation.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-spconv"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <sstream>
#include <iostream>

using namespace llvm;


/// SPConvList - Option to enable single-path conversion.
static cl::list<std::string> SPConvList(
  "mpatmos-spconv",
  cl::value_desc("list"),
  cl::desc("A list of functions to Single-Path convert (Patmos only)."),
  cl::CommaSeparated,
  cl::Hidden);



// anonymous namespace
namespace {

  /// Pass to perform if-conversion for single-path code generation
  class PatmosSPConv : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    const PatmosTargetMachine &TM;
    const PatmosSubtarget &STC;

    /// Set of functions to be converted
    std::set<std::string> SPConvFuncs;

  protected:
    /// Perform the conversion on a given MachineFunction
    void doConvertFunction(MachineFunction &MF);

  public:
    /// PatmosSPConv - Initialize with PatmosTargetMachine
    PatmosSPConv(const PatmosTargetMachine &tm) :
      MachineFunctionPass(ID), TM(tm),
      STC(tm.getSubtarget<PatmosSubtarget>()) {}

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Single-Path Converter";
    }

    /// doInitialization - Initialize SPConv pass
    virtual bool doInitialization(Module &M) {
      // get the set of functions to convert as specified on command line
      SPConvFuncs.insert( SPConvList.begin(), SPConvList.end() );
      return false;
    }

    /// doFinalization - Finalize SPConv pass
    virtual bool doFinalization(Module &M) {
      if (!SPConvFuncs.empty()) {
        DEBUG( dbgs() << "Following functions to SPConv not found:\n" );
        for (std::set<std::string>::iterator it=SPConvFuncs.begin(); it!=SPConvFuncs.end(); ++it)
           DEBUG( dbgs() << *it << ' ');
        DEBUG( dbgs() << '\n');
      }
      return false;
    }

    /// runOnMachineFunction - Run the SP converter on the given function.
    virtual bool runOnMachineFunction(MachineFunction &MF) {
      // If the pass is not enabled, bail out
      if (SPConvFuncs.empty())
        return false;

      bool changed = false;
      std::string curfunc = MF.getFunction()->getName();
      // only convert function if specified on command line
      if ( SPConvFuncs.count(curfunc) ) {
        DEBUG( dbgs() << "Single-Path converting " << curfunc << "\n");
        doConvertFunction(MF);
        SPConvFuncs.erase(curfunc);
        changed |= true;
      }
      return changed;
    }
  };

  char PatmosSPConv::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosSPConvPass - Returns a new PatmosSPConv
/// \see PatmosSPConv
FunctionPass *llvm::createPatmosSPConvPass(const PatmosTargetMachine &tm) {
  return new PatmosSPConv(tm);
}


///////////////////////////////////////////////////////////////////////////////

void PatmosSPConv::doConvertFunction(MachineFunction &MF) {
  // TODO
}
