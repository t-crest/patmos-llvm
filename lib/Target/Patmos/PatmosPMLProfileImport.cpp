//===-- PatmosPMLProfileImport.cpp - Import PML analysis results. -===========//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Import PML info to PatmosMachineFunctionInfo and set branch probabilities. 
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-pml-import"

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineBranchProbabilityInfo.h"
#include "llvm/CodeGen/PML.h"
#include "llvm/CodeGen/PMLImport.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <set>
#include <cmath>

using namespace llvm;


/// EnableViewSCAGraph - Option to enable the rendering of the Spill Cost 
/// Analysis graph.
static cl::opt<bool> UseCritEdgeWeight(
  "mpatmos-use-crit-edge-weight",
  cl::init(false),
  cl::desc("Use criticalities for edge weights instead of frequencies."),
  cl::Hidden);

namespace llvm {
  /// Count the number of SENS instructions removed.
  STATISTIC(RemovedSENS, "SENS instructions removed (zero fills).");


  class PatmosPMLProfileImport : public MachineFunctionPass {
  private:

    /// Target machine info
    const PatmosTargetMachine &TM;

  public:
    static char ID; // Pass identification, replacement for typeid

    PatmosPMLProfileImport(const PatmosTargetMachine &TM)
    : MachineFunctionPass(ID), TM(TM)
    {
    }

    virtual const char *getPassName() const {
      return "Patmos PML Profile Import Pass";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<PMLMachineFunctionImport>();
      AU.addRequired<MachineBranchProbabilityInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    virtual bool runOnMachineFunction(MachineFunction &MF);

  };

  char PatmosPMLProfileImport::ID = 0;
}


/// createPatmosStackCacheAnalysis - Returns a new PatmosStackCacheAnalysis.
FunctionPass *llvm::createPatmosPMLProfileImport(const PatmosTargetMachine &tm)
{
  return new PatmosPMLProfileImport(tm);
}

bool PatmosPMLProfileImport::runOnMachineFunction(MachineFunction &MF)
{
  PMLMachineFunctionImport &PI = getAnalysis<PMLMachineFunctionImport>();

  if (!PI.isAvailable()) return false;

  PI.loadCriticalityMap();

  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();
  PatmosAnalysisInfo &PAI = PMFI.getAnalysisInfo();

  MachineBranchProbabilityInfo &MBPI =
                                getAnalysis<MachineBranchProbabilityInfo>();

  for (MachineFunction::iterator it = MF.begin(), ie = MF.end(); it != ie; it++)
  {
    MachineBasicBlock *MBB = &*it;

    double Crit = PI.getCriticalty(MBB);
    PAI.setCriticality(MBB, Crit);

    uint64_t Freq = PI.getWCETFrequency(MBB);
    PAI.setFrequency(MBB, Freq);

    /// Set edge probabilities based on frequency or criticality of edges
    for (MachineBasicBlock::succ_iterator succ = MBB->succ_begin(),
         se = MBB->succ_end(); succ != se; succ++)
    {
      MachineBasicBlock *ToMBB = *succ;

      uint32_t Weight;
      if (UseCritEdgeWeight) {
        Weight = round(PI.getCriticalty(MBB, ToMBB, 1.0) * 10000.0);
      } else {
        Weight = PI.getWCETFrequency(MBB, ToMBB, 0);
      }

      MBPI.setEdgeWeight(MBB, succ, Weight);
    }
  }

  return false;
}
