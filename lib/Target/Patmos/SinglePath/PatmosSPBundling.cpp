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
    if(root->isHeader(block) || root->isSubheader(block)) continue;
    auto mbb = block->getMBB();
    if(mbb->succ_size() == 2 && block->getDefinitions().size() == 2){
      auto defs = block->getDefinitions();
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

void PatmosSPBundling::printBlock(const PredicatedBlock* block, unsigned indent){
  auto MBB = block->getMBB();
  const MachineFunction& MF = *MBB->getParent();
  errs().indent(indent) << MBB->getFullName() << " #" << MBB->getNumber() <<":\n";
  for( MachineBasicBlock::iterator MI = MBB->begin(),
                                       ME = MBB->getFirstTerminator();
                                       MI != ME; ++MI) {
    auto instructionPredicates = block->getInstructionPredicates();
    errs().indent(indent + 2) << "[" << &(*MI) << "](";
    if(instructionPredicates.count(MI)){
      errs() << block->getInstructionPredicates().at(MI);
    }else{
      errs() << "-";
    }
    errs() << ") ";
    MI->print(errs(), &(MF.getTarget()), false);
    printMetaData(MI);
  }
  errs().indent(indent) << "Terminators:\n";
  for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                           ME = MBB->end();
                                           MI != ME; ++MI) {
    auto instructionPredicates = block->getInstructionPredicates();
    errs().indent(indent + 2) << "[" << &(*MI) << "](";
    if(instructionPredicates.count(MI)){
      errs() << block->getInstructionPredicates().at(MI);
    }else{
      errs() << "-";
    }
    errs() << ") ";
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

void PatmosSPBundling::printBlocksDetailed(SPScope* root) {
  for (auto block : root->getFcfgBlocks()) {
    errs() << "[" << block << "] ";
    if (root->isHeader(block)) {
      errs() << "<HEADER> ";
    }else if(root->isSubheader(block)){
      errs() << "<SUB> ";
    }
    block->dump(errs(), 0);
    printBlock(block, 2);
  }
}

void PatmosSPBundling::doBundlingFunction(SPScope* root) {
//  errs() << "-------- Bundling scope with header: " << root->getHeader() << " --------\n";

//  printBlocksDetailed(root);

  bool tryAgain = true;

  while(tryAgain){
    tryAgain = false;
    for(auto block: root->getScopeBlocks()){
      auto mbb = block->getMBB();
      if(mbb->succ_size() == 2 && block->getDefinitions().size() == 2){
        auto defs = block->getDefinitions();
        auto b1 = (PredicatedBlock*) (*defs.begin()).useBlock;
        auto b2 = (PredicatedBlock*) (*(++defs.begin())).useBlock;
        if(!(root->isSubheader(b1) || root->isSubheader(b2) || b1->getMBB()->succ_size() == 0 || b2->getMBB()->succ_size() == 0 ||
            b1->bundledMBBs().size()>0 || b1->bundledMBBs().size()>0)){

          PredicatedBlock *destination, *source;

          auto farMBB = TII->getBranchTarget(mbb->getFirstInstrTerminator());
          if(TII->mayFallthrough(*mbb) && farMBB == b1->getMBB()){
            assert(++mbb->getFirstInstrTerminator() == mbb->end());
            assert(farMBB == b1->getMBB() || farMBB == b2->getMBB());
            destination = b2;
            source = b1;
            auto terminator = mbb->getFirstInstrTerminator();
            mbb->remove(terminator);
          }else{
            destination = b1;
            source = b2;
          }

          root->bundle(destination, source);
          tryAgain = true;
          break;
        }
      }
    }
  }
//  errs()  << "-------- Finished bundling scope with header: " << root->getHeader() << " --------\n";
//  printBlocksDetailed(root);
//  abort();
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


