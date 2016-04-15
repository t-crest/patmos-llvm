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
#include "llvm/PML.h"
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
  cl::init(true),
  cl::desc("Use criticalities for edge weights instead of frequencies."),
  cl::Hidden);

namespace {

  class PatmosPMLProfileImport : public MachineFunctionPass {
  private:

  public:
    static char ID; // Pass identification, replacement for typeid

    PatmosPMLProfileImport() : MachineFunctionPass(ID) {
      initializePatmosPMLProfileImportPass(*PassRegistry::getPassRegistry());
    }

    virtual const char *getPassName() const {
      return "Patmos PML Profile Import Pass";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<PMLMachineFunctionImport>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    virtual bool runOnMachineFunction(MachineFunction &MF);

  };

  char PatmosPMLProfileImport::ID = 0;
}

INITIALIZE_PASS_BEGIN(PatmosPMLProfileImport, "patmos-pml-import",
                      "Patmos PML Profile Import", false, false)
INITIALIZE_PASS_DEPENDENCY(PMLMachineFunctionImport)
INITIALIZE_PASS_END(PatmosPMLProfileImport, "patmos-pml-import",
                    "Patmos PML Profile Import", false, false)

FunctionPass *llvm::createPatmosPMLProfileImport() {
  return new PatmosPMLProfileImport();
}

bool PatmosPMLProfileImport::runOnMachineFunction(MachineFunction &MF)
{
  PMLMachineFunctionImport &PI = getAnalysis<PMLMachineFunctionImport>();

  if (!PI.isAvailable()) return false;

  PI.loadCriticalityMap();

  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();
  PatmosAnalysisInfo &PAI = PMFI.getAnalysisInfo();

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

      BranchProbability Prob;
      if (UseCritEdgeWeight) {
        uint32_t num = round(PI.getCriticalty(MBB, ToMBB, 1.0) * 10000);
        Prob = BranchProbability::getBranchProbability(num, 10000);
      } else {
        Prob = BranchProbability::getRaw(PI.getWCETFrequency(MBB, ToMBB, 0));
      }

      MBB->setSuccProbability(succ, Prob);
    }

    MBB->normalizeSuccProbs();
  }

  return false;
}
