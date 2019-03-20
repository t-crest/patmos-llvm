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

#include "PatmosSPBundling.h"

using namespace llvm;

char PatmosSPBundling::ID = 0;

/// createPatmosSPBundlingPass - Returns a new PatmosSPBundling
/// \see PatmosSPBundling
FunctionPass *llvm::createPatmosSPBundlingPass(const PatmosTargetMachine &tm) {
  return new PatmosSPBundling(tm);
}

void checkSPPredicateConsistency(MachineFunction &MF){
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                     MBB != MBBE; ++MBB) {
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                         ME = MBB->end();
                                         MI != ME; ++MI) {
      for(int i = 0; i< MI->getNumOperands(); i++){
        MachineOperand &p = MI->getOperand(i);
        if ( p.isMetadata()){
          const MDNode *n = p.getMetadata();
          if( MDString *t = cast<MDString>(n->getOperand(0))){
            if(t->getString().startswith("SPPred:")){
              //OK
              return;
            }
          }
        }
      }
      errs() << "Instruction does not have SPPred metadata:" ;
      MI->print(errs(), &(MI->getParent()->getParent()->getTarget()));
      abort();
    }
  }
}

void PatmosSPBundling::doBundlingFunction(MachineFunction &MF) {
  //outs() << "------------ At bundling -----------------\n";
  //printFunction(MF);
//  checkSPPredicateConsistency(MF);
}

void printMetaData(MachineBasicBlock::iterator & MI){

  for(int i = 0; i< MI->getNumOperands(); i++){
    MachineOperand &p = MI->getOperand(i);
    if ( p.isMetadata()){
      const MDNode *n = p.getMetadata();
      outs() << "\t^MetaData: ";
      n->print(outs());
      outs() << "\n";
    }
  }
}

void PatmosSPBundling::printFunction(MachineFunction &MF) {
  outs() << "Bundle function '" << MF.getFunction()->getName() << "'\n";
  outs() << "Block list:\n";
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                   MBB != MBBE; ++MBB) {
//    outs() << MBB->getFullName()<< "\nScope: " << PSPI->getScopeFor(MBB)->getHeader()->getMBB()->getFullName() << "\n";
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                         ME = MBB->getFirstTerminator();
                                         MI != ME; ++MI) {
      outs() << "\t";
      MI->print(outs(), &(MF.getTarget()), false);
      printMetaData(MI);
    }
    outs() << "Terminators:\n";
    for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                             ME = MBB->end();
                                             MI != ME; ++MI) {
      outs() << "\t";
      MI->print(outs(), &(MF.getTarget()), false);
      printMetaData(MI);
    }
    outs() << "\n";
  }

  outs() <<"\n";

}

