//===- BBInstCnt.cpp - Basic block instruction counter pass ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple basic block instruction counter analysis pass for IPET on LLIR level.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Statistic.h"

#include "BBInstCnt.h"

using namespace llvm;


namespace ipet {


BBInstCnt::~BBInstCnt() {
  for (StringMap<StringMap<int> *>::const_iterator mi = bbcosts.begin(), me = bbcosts.end(); mi!=me; ++mi) {
    delete mi->getValue();
  }
}

bool BBInstCnt::runOnModule(Module &M) {
  for (Module::iterator mi = M.begin(), me = M.end(); mi != me; ++mi) {
    StringMap<int> *fmap = new StringMap<int>();
    bbcosts[mi->getName()] = fmap;
    for (Function::iterator fi = mi->begin(), fe = mi->end(); fi != fe; ++fi) {
      (*fmap)[fi->getName()] = fi->size();
    }
  }
  return false;
}


void BBInstCnt::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesAll();
}

void BBInstCnt::print(raw_ostream &OS, const Module *M) const {
  for (StringMap<StringMap<int> *>::const_iterator mi = bbcosts.begin(), me = bbcosts.end(); mi!=me; ++mi) {
    OS << mi->getKey() << '\n';
    StringMap<int> *fmap = mi->getValue();
    for (StringMap<int>::const_iterator i = fmap->begin(), e = fmap->end(); i!=e; ++i) {
      OS << "  " << i->getKey() << " " << i->getValue() << '\n';
    }
  }
}

char BBInstCnt::ID = 0;
static RegisterPass<BBInstCnt> X("bbinstcnt", "Basic Block Instruction Counter Pass");

}
