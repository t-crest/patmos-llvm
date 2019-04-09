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

void PatmosSPBundling::printBlocksDetailed(const SPScope* root) {
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

int numberOfInstructions(MachineBasicBlock* mbb){
  auto count = 0;
  for(auto begin = mbb->begin(), end = mbb->getFirstTerminator();
      begin != end ; begin++){
    count++;
  }
  return count;
}

void mergeMBBs(MachineBasicBlock *mbb1, MachineBasicBlock *mbb2){
  auto moveInstruction = [&](auto moveAfter){
    auto inst = &(*mbb2->begin());
    mbb2->remove(inst);
    mbb1->insert(moveAfter, inst);
  };

  auto moveNextInstruction = [&](unsigned movedInstructions){
    auto mbb1next = mbb1->begin();
    for(int i = 0; i<movedInstructions; i++){
      // We advance by 2, because we have already inserted some instructions from mbb2
      mbb1next++;
      mbb1next++;
    }
    moveInstruction(mbb1next);
  };
  int nrMbb1Instrs = numberOfInstructions(mbb1);
  if (nrMbb1Instrs > numberOfInstructions(mbb2)) {
    for(auto movedInstructions = 0;
        mbb2->begin() != mbb2->getFirstTerminator();
        movedInstructions++)
    {
      moveNextInstruction(movedInstructions);
    }
  }else{
    for(auto movedInstructions = 0;
        movedInstructions < nrMbb1Instrs;
        movedInstructions++)
    {
      moveNextInstruction(movedInstructions);
    }

    while(mbb2->begin() != mbb2->getFirstInstrTerminator()){
      moveInstruction(mbb1->getFirstInstrTerminator());
    }
  }

  // Only terminators left
  while(mbb2->begin() != mbb2->end()){
    auto inst = &(*mbb2->begin());
    mbb2->remove(inst);
    mbb1->insert(mbb1->end(), inst);
  }
}

void PatmosSPBundling::doBundlingFunction(SPScope* root) {
  bool tryAgain = true;

  while(tryAgain){
    tryAgain = false;
    for(auto block: root->getScopeBlocks()){
      auto mbb = block->getMBB();
      if(mbb->succ_size() == 2 && block->getDefinitions().size() == 2){
        auto defs = block->getDefinitions();
        auto b1 = (PredicatedBlock*) (*defs.begin()).useBlock;
        auto b2 = (PredicatedBlock*) (*(++defs.begin())).useBlock;
        if(!(root->isSubheader(b1) || root->isSubheader(b2) || b1->getSuccessors().size() == 0 || b2->getSuccessors().size() == 0 ||
            b1->bundledMBBs().size()>0 || b2->bundledMBBs().size()>0)){

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

          mergeMBBs(destination->getMBB(), source->getMBB());

          destination->merge(source);

          auto mbb1 = destination->getMBB(), mbb2 = source->getMBB();
          auto func = mbb2->getParent();

          for(auto iter = func->begin(), end = func->end(); iter != end; iter++){
            if(iter->isSuccessor(mbb2)){
              iter->ReplaceUsesOfBlockWith(mbb2, mbb1);
            }
          }
          while(mbb2->succ_begin() != mbb2->succ_end()){
            mbb2->removeSuccessor(mbb2->succ_begin());
          }

          func->erase(source->getMBB());

          root->bundle(destination, source);
          tryAgain = true;
          break;
        }
      }
    }
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


