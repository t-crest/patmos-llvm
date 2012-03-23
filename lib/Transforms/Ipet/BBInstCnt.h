//===- BBInstCnt.h - Header for basic block instruction counter pass ------===//
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

#ifndef _LLVM_IPET_BBINSTCNT_H_
#define _LLVM_IPET_BBINSTCNT_H_

//#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/ADT/StringMap.h"
//#include "llvm/ADT/Statistic.h"
using namespace llvm;


namespace ipet {
  class BBInstCnt : public ModulePass {
    public:
      static char ID; // Pass identification, replacement for typeid
      BBInstCnt() : ModulePass(ID) {}
      virtual ~BBInstCnt();

      // Map( functionname -> Map(bbname -> int_costs) )
      StringMap<StringMap<int> *> bbcosts;

      virtual bool runOnModule(Module &M);

      virtual void getAnalysisUsage(AnalysisUsage &AU) const;

      virtual void print(raw_ostream &O, const Module *M) const;
  };
}

#endif // _LLVM_IPET_BBINSTCNT_H_
