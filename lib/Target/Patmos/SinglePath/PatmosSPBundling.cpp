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
#include "llvm/ADT/Statistic.h"

using namespace llvm;

STATISTIC(PairsTried,     "Number of basic blocks tried to bundle");
STATISTIC(PairsSuccess,     "Number of basic blocks tried to bundle and succeeded");
STATISTIC(InstPairsTried,     "Number of instructions tried to bundle");
STATISTIC(InstPairsSwitched,     "Number of instructions tried to bundle and but instead switched");
STATISTIC(InstPairsSuccess,     "Number of instructions tried to bundle and succeeded");
STATISTIC(SPBlocks,     "Number of basic blocks in single-path code (before bundle)");

char PatmosSPBundling::ID = 0;

/// createPatmosSPBundlingPass - Returns a new PatmosSPBundling
/// \see PatmosSPBundling
FunctionPass *llvm::createPatmosSPBundlingPass(const PatmosTargetMachine &tm) {
  return new PatmosSPBundling(tm);
}

bool PatmosSPBundling::runOnMachineFunction(MachineFunction &MF) {
  PSPI = &getAnalysis<PatmosSinglePathInfo>();
  PostDom = &getAnalysis<MachinePostDominatorTree>();
  PMFI = MF.getInfo<PatmosMachineFunctionInfo>();
    
  // only convert function if marked
  if ( PSPI->isConverting(MF)
  ) {
    SPBlocks += MF.size();
	doBundlingFunction(PSPI->getRootScope());
  }
  return true;
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

int numberOfInstructions(MachineBasicBlock* mbb){
  auto count = 0;
  for(auto begin = mbb->begin(), end = mbb->getFirstTerminator();
      begin != end ; begin++){
    count++;
  }
  return count;
}

void PatmosSPBundling::mergeMBBs(MachineBasicBlock *mbb1, MachineBasicBlock *mbb2){

  auto moveInstruction = [&](auto bundleWith){
    InstPairsTried++;
    auto inst = &(*mbb2->instr_begin());
    mbb2->remove(inst);
#define DO_BUNDLE // for easily turning off bundling of instructions
#ifdef DO_BUNDLE
    if(TII->canIssueInSlot(inst, 1)){
      InstPairsSuccess++;
      mbb1->insertAfter(bundleWith, inst);
      inst->bundleWithPred();
    }else if (TII->canIssueInSlot(&(*bundleWith), 1)){
      InstPairsSwitched++;
      mbb1->insert(bundleWith, inst);
      inst->bundleWithSucc();
    }else{
#endif
      mbb1->insertAfter(bundleWith, inst);
#ifdef DO_BUNDLE
    }
#endif
  };

  auto moveNextInstruction = [&](unsigned movedInstructions){
    auto mbb1next = mbb1->instr_begin();
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
        mbb2->instr_begin() != mbb2->getFirstInstrTerminator();
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

    while(mbb2->instr_begin() != mbb2->getFirstInstrTerminator()){
      auto inst = &(*mbb2->instr_begin());
      mbb2->remove(inst);
      mbb1->insert(mbb1->getFirstInstrTerminator(), inst);
    }
  }

  // Only terminators left
  while(mbb2->instr_begin() != mbb2->instr_end()){
    auto inst = &(*mbb2->instr_begin());
    mbb2->remove(inst);
    mbb1->insert(mbb1->instr_end(), inst);
  }
}

std::pair<bool, std::pair<PredicatedBlock*,PredicatedBlock*>> PatmosSPBundling::findMergePair(const SPScope* scope){
  for(auto block: scope->getScopeBlocks()){
    DEBUG(dbgs() << "Looking for merge pair at: #" << block->getMBB()->getNumber() << "\n");
    auto succs = block->getSuccessors();
    if(succs.size() == 2){
      PairsTried++;
      if(block->getExitTargets().size() == 0){
        auto mbb = block->getMBB();

        auto b1 = (PredicatedBlock*) (*succs.begin()).first;
        auto b2 = (PredicatedBlock*) (*(++succs.begin())).first;

        if( scope->isHeader(b1) || scope->isHeader(b2) ||
            scope->isSubheader(b1) || scope->isSubheader(b2) ||
            PostDom->dominates(b1->getMBB(), b2->getMBB()) ||
            PostDom->dominates(b2->getMBB(), b1->getMBB())){
          continue;
        }

        if(!(scope->isSubheader(b1) || scope->isSubheader(b2) || b1->getSuccessors().size() == 0 || b2->getSuccessors().size() == 0 ||
          b1->bundledMBBs().size()>0 || b2->bundledMBBs().size()>0)){

          auto farMBB = TII->getBranchTarget(mbb->getFirstInstrTerminator());
          if(TII->mayFallthrough(*mbb) && farMBB == b1->getMBB()){
            assert(++mbb->getFirstInstrTerminator() == mbb->end());
            assert(farMBB == b1->getMBB() || farMBB == b2->getMBB());
            return std::make_pair(true, std::make_pair(b2, b1));
          }else{
            return std::make_pair(true, std::make_pair(b1, b2));
          }
        }
	  }
    }
  }
  return std::make_pair(false, std::make_pair((PredicatedBlock*)NULL, (PredicatedBlock*)NULL));
}

void PatmosSPBundling::bundleScope(SPScope* root){
  std::pair<bool, std::pair<PredicatedBlock*,PredicatedBlock*>> mergePair;
  while( (mergePair = findMergePair(root)).first ){
    PairsSuccess++;

    auto destination = mergePair.second.first;
    auto source = mergePair.second.second;

    DEBUG(dbgs() << "Merge pair: (#" << destination->getMBB()->getNumber() << ", #" << source->getMBB()->getNumber() << ")\n");

    mergeMBBs(destination->getMBB(), source->getMBB());

    auto mbb1 = destination->getMBB(), mbb2 = source->getMBB();
    auto func = mbb2->getParent();

    // Replace the use of the discarded MBB with the other
    for(auto iter = func->begin(), end = func->end(); iter != end; iter++){
      if(iter->isSuccessor(mbb2)){
        iter->ReplaceUsesOfBlockWith(mbb2, mbb1);
      }
    }
    while(mbb2->succ_begin() != mbb2->succ_end()){
      mbb2->removeSuccessor(mbb2->succ_begin());
    }

    func->erase(source->getMBB());

    // Merge the two PredicatedBlocks into one
    root->merge(destination, source);
  }

  std::for_each(root->child_begin(), root->child_end(), [&](auto subscope){
    bundleScope(subscope);
  });
}

void PatmosSPBundling::doBundlingFunction(SPScope* root) {

  DEBUG(dbgs() << "========= Begin bundling ========= \n");

  bundleScope(root);

  DEBUG({
      dbgs() << "Scope tree after bundling:\n";
      root->dump(dbgs(), 0, true);
      dbgs() << "========= End bundling ========= \n";
  });

}
