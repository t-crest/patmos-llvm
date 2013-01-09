//===-- PatmosFunctionSplitter.cpp - Split functions to fit into the cache ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a Patmos-customized PML export driver and -pass.
// 
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-export"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
//#include "llvm/CodeGen/PMLExport.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <sstream>
#include <iostream>

using namespace llvm;

// TODO should we make this a generic option of PMLExport?
static cl::opt<bool> DisableFunctionSplitter(
  "mpatmos-serialize-skip-instructions",
  cl::init(false),
  cl::desc("Only export interesting instructions, such as branches."),
  cl::Hidden);

namespace llvm {

  /*
  class PatmosExport : public PMLExport {

  public:
    PatmosExport(TargetMachine *tm) : PMLExport(tm) {}

  };


  // TODO check if we can just subtype PMLExportPass here
  class PatmosExportPass : public MachineFunctionPass {

    static char ID;

    PMLExportManager PEM;

  public:
    PatmosExportPass(std::string& filename, PatmosTargetMachine *tm)
      : MachineFunctionPass(ID), PEM(filename, tm)
    {
      PEM.addDefaultExporter();
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<MachineLoopInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    virtual bool doInitialization(Module &M) {
      PEM.initialize();
      return false;
    }

    virtual bool doFinalization(Module &M) {
      PEM.finalize();
      return false;
    }

    virtual const char *getPassName() const { return "Patmos YAML/PML Export"; }

    /// Serialize machine function, bitcode functions and relation
    /// graph for generic architecture
    virtual bool runOnMachineFunction(MachineFunction &MF)
    {
      MachineLoopInfo *LI = &getAnalysis<MachineLoopInfo>();
      yaml::Doc<yaml::GenericArchitecture> YDoc("pml-0.1");
      PEM.serialize(MF, LI, YDoc);
      PEM.writeDoc(YDoc);
      return false;
    }
  };

  char PatmosExportPass::ID = 0;

  /// createPatmosExportPass - Returns a new PatmosExportPass
  /// \see PatmosExportPass
  FunctionPass *createPatmosExportPass(std::string& filename,
                                       PatmosTargetMachine &tm)
  {
    return new PatmosExportPass(filename, &tm);
  }
  */

} // end namespace llvm

