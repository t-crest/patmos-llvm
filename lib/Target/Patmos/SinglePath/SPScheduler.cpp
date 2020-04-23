//===---------- SchedulePostRAList.cpp - Scheduler ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "SPScheduler.h"

#include "llvm/Support/Debug.h"
#include "llvm/ADT/Statistic.h"

using namespace llvm;

STATISTIC(SPInstructions,     "Number of instruction bundles in single-path code (both single and double)");

char SPScheduler::ID = 0;

FunctionPass *llvm::createSPSchedulerPass(const PatmosTargetMachine &tm) {
  return new SPScheduler(tm);
}

bool SPScheduler::runOnMachineFunction(MachineFunction &mf){

  // Only schedule single-path function
  if(! mf.getInfo<PatmosMachineFunctionInfo>()->isSinglePath()){
    return false;
  }

  DEBUG( dbgs() << "Running SPScheduler on function '" <<  mf.getName() << "'\n");

  auto reduceAnalysis = &getAnalysis<PatmosSPReduce>();
  auto rootScope = reduceAnalysis->RootScope;

  for(auto mbbIter = mf.begin(), mbbEnd = mf.end(); mbbIter != mbbEnd; mbbIter++){
    auto mbb = &(*mbbIter);
    DEBUG(dbgs() << "MBB: [" << mbb << "]: #" << mbb->getNumber() << "\n");
    for(auto instrIter = mbb->begin(), instrEnd = mbb->end();
        instrIter != instrEnd; instrIter++)
    {
      SPInstructions++;
	  calculateLatency(instrIter);
    }


  }

  DEBUG( dbgs() << "AFTER Single-Path Schedule\n"; mf.dump() );
  DEBUG({
      dbgs() << "Scope tree after scheduling:\n";
      rootScope->dump(dbgs(), 0, true);
  });
  return true;
}

unsigned SPScheduler::calculateLatency(MachineBasicBlock::iterator instr){
  auto instrInfo = TM.getInstrInfo();

  auto mbb = instr->getParent();
  if(instr->isBranch() || instr->isCall() || instr->isReturn()){
    DEBUG(dbgs() << "Inserting 3xNOP after instruction: "; instr->print(dbgs(), NULL, false));

    // We simply add 3 nops after any branch, call or return, as its
    // the highest possible delay.
    // The delay slot filler pass will remove most of these afterwards
    instr++;
    instrInfo->insertNoop(*mbb, instr);
    instrInfo->insertNoop(*mbb, instr);
    instrInfo->insertNoop(*mbb, instr);
  } else if (instr->mayLoad() || (instr->getOpcode() == Patmos::MUL) || (instr->getOpcode() == Patmos::MULU)){
    DEBUG(dbgs() << "Inserting NOP after instruction: "; instr->print(dbgs(), NULL, false));
    instr++;
    auto sizebefore = mbb->size();
    instrInfo->insertNoop(*mbb, instr);
    assert(mbb->size() == (sizebefore+1));
    // TODO: make it cleverer, e.g. no reason for a nop
    // if the next instruction does not use the loaded value.
  }
}



