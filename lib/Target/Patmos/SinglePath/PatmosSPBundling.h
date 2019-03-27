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

  /// doBundlingFunction - Bundle a given MachineFunction
  void doBundlingFunction(SPScope* root);
  void printBlocksDetailed(SPScope* root);

public:
  static char ID;
  PatmosSPBundling(const PatmosTargetMachine &tm):
       MachineFunctionPass(ID), TM(tm),
       STC(tm.getSubtarget<PatmosSubtarget>()),
       TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
       TRI(static_cast<const PatmosRegisterInfo*>(tm.getRegisterInfo())){ (void) TM; }

  /// getPassName - Return the pass' name.
  virtual const char *getPassName() const {
    return "Patmos Single-Path Bundling (machine code)";
  }

  /// getAnalysisUsage - Specify which passes this pass depends on
  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<PatmosSinglePathInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  virtual bool doInitialization(Module &M){
    return false;
  }

  virtual bool doFinalization(Module &M){
    return false;
  }

  virtual bool runOnMachineFunction(MachineFunction &MF) {
    PSPI = &getAnalysis<PatmosSinglePathInfo>();
    PMFI = MF.getInfo<PatmosMachineFunctionInfo>();
    bool changed = false;
    // only convert function if marked
    if ( PSPI->isConverting(MF) ) {
      // TODO: DEBUG( dbgs() << "[Single-Path] Reducing "
      //              << MF.getFunction()->getName() << "\n" );
      printFunction(MF);
      doBundlingFunction(PSPI->getRootScope());
    }
    return changed;
  }

  SPScope* getRootScope(){
    return PSPI->getRootScope();
  }

  static void printFunction(MachineFunction &MF);
};

}

#endif /* TARGET_PATMOS_SINGLEPATH_PATMOSSPBUNDLING_H_ */
