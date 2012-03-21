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
#include "llvm/CallGraphSCCPass.h"
#include "llvm/Analysis/CallGraph.h"
#include "BBInstCnt.h"

using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");


namespace ipet {

  class Ipet : public CallGraphSCCPass {
    public:
      static char ID; // Pass identification, replacement for typeid
      Ipet() : CallGraphSCCPass(ID) {}

      virtual bool runOnSCC(CallGraphSCC &SCC) {
        BBInstCnt &bbic = getAnalysis<BBInstCnt>();

        errs() << "Ipet: ";
        //++SomeCounter;//bump
        if (!SCC.isSingular()) {
        	errs() << "Not a singular SCC\n";
        	return false;
        }
        Function *F = (*(SCC.begin()))->getFunction();
        
        if (!F) {
        	errs() << "No function\n";
        	return false;
        }
        errs() << F->getName() << ", costmap:\n"; bbic.dump();
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
