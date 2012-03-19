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

namespace {
  // Ipet - The first implementation, without getAnalysisUsage.
  struct Ipet : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    Ipet() : FunctionPass(ID) {}

    virtual bool runOnFunction(Function &F) {
      ++IpetCounter;
      errs() << "Ipet: ";
      errs().write_escaped(F.getName()) << '\n';
      return false;
    }
  };
}

char Ipet::ID = 0;
static RegisterPass<Ipet> X("ipet", "Ipet Pass");

namespace {
  // Ipet2 - The second implementation with getAnalysisUsage implemented.
  struct Ipet2 : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    Ipet2() : FunctionPass(ID) {}

    virtual bool runOnFunction(Function &F) {
      ++IpetCounter;
      errs() << "Ipet: ";
      errs().write_escaped(F.getName()) << '\n';
      return false;
    }

    // We don't modify the program, so we preserve all analyses
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
    }
  };
}

char Ipet2::ID = 0;
static RegisterPass<Ipet2>
Y("ipet2", "Ipet Pass (with getAnalysisUsage implemented)");
