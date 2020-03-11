/*
 * PatmosSPBundling.h
 *
 *  Created on: 20 Mar 2019
 *      Author: Emad
 */

#ifndef TARGET_PATMOS_SINGLEPATH_PATMOSSPBUNDLING_H_
#define TARGET_PATMOS_SINGLEPATH_PATMOSSPBUNDLING_H_

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSinglePathInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/IR/Metadata.h"
#include "llvm/CodeGen/MachinePostDominators.h"

#include "PatmosSinglePathInfo.h"

#define DEBUG_TYPE "patmos-singlepath"

namespace llvm {

class PatmosSPBundling : public MachineFunctionPass {

private:

  const PatmosTargetMachine &TM;
  const PatmosSubtarget &STC;
  const PatmosInstrInfo *TII;
  const PatmosRegisterInfo *TRI;

  // The pointer to the PatmosMachinFunctionInfo is set upon running on a
  // particular function. It contains information about stack slots for
  // predicate spilling and loop bounds.
  const PatmosMachineFunctionInfo *PMFI;

  PatmosSinglePathInfo *PSPI;

  MachinePostDominatorTree *PostDom;

  /// doBundlingFunction - Bundle a given MachineFunction
  void doBundlingFunction(SPScope* root);

public:
  static char ID;
  PatmosSPBundling(const PatmosTargetMachine &tm):
       MachineFunctionPass(ID), TM(tm),
       STC(tm.getSubtarget<PatmosSubtarget>()),
       TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
       TRI(static_cast<const PatmosRegisterInfo*>(tm.getRegisterInfo()))
  {
    (void) TM;
  }

  /// getPassName - Return the pass' name.
  virtual const char *getPassName() const {
    return "Patmos Single-Path Bundling (machine code)";
  }

  /// getAnalysisUsage - Specify which passes this pass depends on
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<PatmosSinglePathInfo>();
    AU.addRequired<MachinePostDominatorTree>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  virtual bool doInitialization(Module &M){
    return false;
  }

  virtual bool doFinalization(Module &M){
    return false;
  }

  virtual bool runOnMachineFunction(MachineFunction &MF);

  SPScope* getRootScope(){
    return PSPI->getRootScope();
  }

  /// Tries to find a pair of blocks to merge.
  ///
  /// If a pair is found, true is returned with the pair of blocks found.
  /// If no pair is found, false is returned with a pair of NULLs.
  std::pair<bool, std::pair<PredicatedBlock*,PredicatedBlock*>>
  findMergePair(const SPScope*);

  void mergeMBBs(MachineBasicBlock *mbb1, MachineBasicBlock *mbb2);

  void bundleScope(SPScope* root);
};

}

#endif /* TARGET_PATMOS_SINGLEPATH_PATMOSSPBUNDLING_H_ */
