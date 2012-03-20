//===- BBInstCnt.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "BBInstCnt" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/BasicBlock.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Statistic.h"
using namespace llvm;

//STATISTIC(BBInstCntCounter, "Counts number of functions greeted");


namespace ipet {
  struct BBInstCnt : BasicBlockPass {
    static char ID; // Pass identification, replacement for typeid
    BBInstCnt() : BasicBlockPass(ID) {}

    StringMap<int> bbcosts;

    virtual bool runOnBasicBlock(BasicBlock &BB) {
      int counter = 0;
      for (BasicBlock::iterator i=BB.begin(), e=BB.end(); i != e; i++) {
        errs() << (*i).getOpcodeName() << "\n";
        counter++;
      }
      bbcosts[BB.getName()] = counter;
      errs() << "BBInstCnt: (";
      errs().write_escaped(BB.getName()) << ", " << bbcosts[BB.getName()]<< ")\n";
      return false;
    }

    // We don't modify the program, so we preserve all analyses
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
    }
  };
}

char ipet::BBInstCnt::ID = 0;
static RegisterPass<ipet::BBInstCnt> X("bbinstcnt", "BBInstCnt Pass (with getAnalysisUsage implemented)");
