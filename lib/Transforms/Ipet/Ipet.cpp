//===- Ipet.cpp - Generate an ILP problem for WCET Analysis ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a pass to generate an ILP problem.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
#include "BBInstCnt.h"
using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");


namespace ipet {

  class Ipet : public ModulePass {
    public:
      static char ID; // Pass identification, replacement for typeid
      Ipet() : ModulePass(ID) {}

      virtual bool runOnModule(Module &M) {
        BBInstCnt &bbic = getAnalysis<BBInstCnt>();
        //++SomeCounter;//bump
        errs() << "Ipet: ";
        errs() << M << " costmap " << &bbic.bbcosts << '\n';
        return false;
      }

      virtual void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.setPreservesAll();
        AU.addRequired<BBInstCnt>();
      }
  };
}

char ipet::Ipet::ID = 0;
static RegisterPass<ipet::Ipet> X("ipet", "IPET Pass");
