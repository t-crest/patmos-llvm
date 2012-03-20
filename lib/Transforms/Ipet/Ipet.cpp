//===- Ipet.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Ipet" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"
#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
using namespace llvm;

STATISTIC(IpetCounter, "Counts number of functions greeted");


namespace ipet {
  //struct BBInstCnt; //FIXME

  // Ipet - The first implementation, without getAnalysisUsage.
  struct Ipet : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    Ipet() : FunctionPass(ID) {}

    virtual bool runOnFunction(Function &F) {
      //BBInstCnt &bbic = getAnalysis<BBInstCnt>();

      ++IpetCounter;
      errs() << "Ipet: ";
      //errs().write_escaped(F.getName()) << " costmap " << bbic.costs << '\n';
      return false;
    }

    // We don't modify the program, so we preserve all analyses
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      //AU.addRequired<BBInstCnt>();
    }
  };
}

char ipet::Ipet::ID = 0;
static RegisterPass<ipet::Ipet> X("ipet", "Ipet Pass (with getAnalysisUsage implemented)");
