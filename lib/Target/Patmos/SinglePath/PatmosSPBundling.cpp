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

std::pair<PredicatedBlock*, PredicatedBlock*> findBranch(SPScope* root){
  auto blocks = root->getScopeBlocks();

  for(auto block: blocks){
    auto mbb = block->getMBB();
    if(mbb->succ_size() == 2){
      auto defs = block->getDefinitions();
      assert(defs.size() == 2);
      return std::make_pair((PredicatedBlock*)(*defs.begin()).useBlock, (PredicatedBlock*)(*(++defs.begin())).useBlock);
    }
  }
  return std::make_pair((PredicatedBlock*)NULL, (PredicatedBlock*)NULL);
}

void printMetaData(MachineBasicBlock::iterator & MI){

  for(int i = 0; i< MI->getNumOperands(); i++){
    MachineOperand &p = MI->getOperand(i);
    if ( p.isMetadata()){
      const MDNode *n = p.getMetadata();
      errs() << "\t^MetaData: ";
      n->print(errs());
      errs() << "\n";
    }
  }
}

void printBlock(const PredicatedBlock* block){
  auto MBB = block->getMBB();
  const MachineFunction& MF = *MBB->getParent();
  errs() << MBB->getFullName()<< ":\n";
  for( MachineBasicBlock::iterator MI = MBB->begin(),
                                       ME = MBB->getFirstTerminator();
                                       MI != ME; ++MI) {
    errs() << "\t(" << block->getInstructionPredicates().at(MI) << ") ";
    MI->print(errs(), &(MF.getTarget()), false);
    printMetaData(MI);
  }
  errs() << "Terminators:\n";
  for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                           ME = MBB->end();
                                           MI != ME; ++MI) {
    errs() << "\t(" << block->getInstructionPredicates().at(MI) << ") ";
    MI->print(errs(), &(MF.getTarget()), false);
    printMetaData(MI);
  }
  errs() << "\n";
}

void printMBB(MachineBasicBlock* MBB){
  const MachineFunction& MF = *MBB->getParent();
  errs() << MBB->getFullName()<< "(" << MBB <<") #" << MBB->getNumber() << ":\n";
  for( MachineBasicBlock::iterator MI = MBB->begin(),
                                       ME = MBB->getFirstTerminator();
                                       MI != ME; ++MI) {
    errs() << "\t";
    MI->print(errs(), &(MF.getTarget()), false);
    printMetaData(MI);
  }
  errs() << "Terminators:\n";
  for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                           ME = MBB->end();
                                           MI != ME; ++MI) {
    errs() << "\t";
    MI->print(errs(), &(MF.getTarget()), false);
    printMetaData(MI);
  }
  errs() << "\n";
}

void PatmosSPBundling::doBundlingFunction(SPScope* root) {
  errs() << "------------ At bundling -----------------\n";
  printFunction(*root->getHeader()->getMBB()->getParent());
  PredicatedBlock *b1, *b2;
  std::tie(b1, b2) = findBranch(root);
  errs() << "blocks to bundle:\n";
  printBlock(b1);
  printBlock(b2);
  b1 = root->bundle(b1, b2);
  errs() << "Bundled:\n";
  printBlock(b1);
  printFunction(*root->getHeader()->getMBB()->getParent());

  errs() << "Scope:\n";
  for(auto block: root->getScopeBlocks()){
    printBlock(block);
  }
}

void PatmosSPBundling::printFunction(MachineFunction &MF) {
  errs() << "Bundle function '" << MF.getFunction()->getName() << "'\n";
  errs() << "Block list:\n";
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                   MBB != MBBE; ++MBB) {
    printMBB(MBB);
  }

  errs() <<"\n";

}


