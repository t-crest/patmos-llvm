//===-- PatmosSPBundling.cpp - Remove unused function declarations ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass makes the single-pat code utilitize Patmos' dual issue pipeline.
// TODO: more description
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSinglePathInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineModulePass.h"

#include "PatmosSinglePathInfo.h"

// TODO: statistics

using namespace llvm;

namespace {

class PatmosSPBundling : public MachineFunctionPass {

private:
  static char ID;
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
  void doBundlingFunction(MachineFunction &MF);

  void printFunction(MachineFunction &MF);
public:

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

  virtual bool runOnMachineFunction(MachineFunction &MF) {
    PSPI = &getAnalysis<PatmosSinglePathInfo>();
    PMFI = MF.getInfo<PatmosMachineFunctionInfo>();
    bool changed = false;
    // only convert function if marked
    if ( PSPI->isConverting(MF) ) {
      // TODO: DEBUG( dbgs() << "[Single-Path] Reducing "
      //              << MF.getFunction()->getName() << "\n" );
      doBundlingFunction(MF);
      changed = true;
    }
    return changed;
  }
};

  char PatmosSPBundling::ID = 0;
}// end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosSPBundlingPass - Returns a new PatmosSPBundling
/// \see PatmosSPBundling
FunctionPass *llvm::createPatmosSPBundlingPass(const PatmosTargetMachine &tm) {
  return new PatmosSPBundling(tm);
}

void checkSPPredicateConsistency(MachineOperand &op){
  MachineFunction *MF = op.getParent()->getParent()->getParent();
  if(!op.isImm()){
    errs() << "[Single-path Bundling] SPPredicate operand is not immediate.";
  }else if(op.getImm() != 123399){
    errs() << "[Single-path Bundling] SPPredicate operand is not 123399.";
  }else{
    return; //success
  }
  errs() << "\nInstruction: ";
  op.getParent()->print(errs(), &MF->getTarget(), false);
  errs() << "Operand: ";
  op.print(errs(), &MF->getTarget());
  errs() << "\n";
  abort();
}

void checkSPPredicateConsistency(MachineFunction &MF){
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                     MBB != MBBE; ++MBB) {
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                         ME = MBB->getFirstTerminator();
                                         MI != ME; ++MI) {
      checkSPPredicateConsistency(MI->getOperand(MI->getNumOperands()-1));
    }
    for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                             ME = MBB->end();
                                             MI != ME; ++MI) {
      checkSPPredicateConsistency(MI->getOperand(MI->getNumExplicitOperands()));
    }
  }
}

void PatmosSPBundling::doBundlingFunction(MachineFunction &MF) {
  //outs() << "------------ At bundling -----------------\n";
  //printFunction(MF);
  checkSPPredicateConsistency(MF);
}

void PatmosSPBundling::printFunction(MachineFunction &MF) {
  outs() << "Bundle function '" << MF.getFunction()->getName() << "'\n";
  outs() << "Block list:\n";
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                   MBB != MBBE; ++MBB) {
    outs() << MBB->getFullName()<< "\nScope: " << PSPI->getScopeFor(MBB)->getHeader()->getFullName() << "\n";
    outs() << "Predicate: " << PSPI->getScopeFor(MBB)->getPredUse(MBB) << "\n";
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                         ME = MBB->getFirstTerminator();
                                         MI != ME; ++MI) {
      outs() << "\t";
      MI->print(outs(), &(MF.getTarget()), false);
      if(TII->isPredicated(MI)){
        outs()<<"\t^Predicated!!\n";
      }
      MachineOperand op = MI->getOperand(MI->getNumOperands()-1);
              if(!op.isImm()){
                errs() << "SPPredicate operand is not immediate: ";
                op.print(errs(), &MF.getTarget());
                errs() << "\n";
                abort();
              }else if(op.getImm() != 123399){
                errs() << "SPPredicate operand is not 123399: ";
                op.print(errs(), &MF.getTarget());
                errs() << "\n";
                abort();
              }
    }
    outs() << "Terminators:\n";
    for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                             ME = MBB->end();
                                             MI != ME; ++MI) {
      outs() << "\t";
      MI->print(outs(), &(MF.getTarget()), false);
      if(TII->isPredicated(MI)){
        outs()<<"\t^Predicated!!\n";
      }
      MachineOperand op = MI->getOperand(MI->getNumExplicitOperands());

                    if(!op.isImm()){
                      errs() << "SPPredicate operand is not immediate: ";
                      op.print(errs(), &MF.getTarget());
                      errs() << "\n";
                      abort();
                    }else if(op.getImm() != 123399){
                      errs() << "SPPredicate operand is not 123399: ";
                      op.print(errs(), &MF.getTarget());
                      errs() << "\n";
                      abort();
                    }
    }
    outs() << "\n";
  }

  outs() <<"\n";

}

