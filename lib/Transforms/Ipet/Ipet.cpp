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


#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CallGraphSCCPass.h"
#include "llvm/Analysis/CallGraph.h"

#include "BBInstCnt.h"
#include "Ipet.h"

using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");

namespace ipet {

bool Ipet::runOnSCC(CallGraphSCC & SCC) {
  BBInstCnt & bbic = getAnalysis < BBInstCnt > ();

  errs() << "------- Ipet: ";
  //++SomeCounter;//bump
  if (!SCC.isSingular()) {
    errs() << "Not a singular SCC; size: " <<
      SCC.size() << "\n";
  } else {
    Function *F = (*(SCC.begin()))->getFunction();

    if (!F) {
      errs() << "No function\n";
      return false;
    }
    errs() << F->getName() << "\n";

    doIpet(*F);
  }
  for (CallGraphSCC::iterator it = SCC.begin();
      it != SCC.end(); it++) {
    (*it)->dump();
  }

  errs() << " - costmap:\n";
  bbic.dump();
  return false;
}


void Ipet::doIpet(Function &F) {
  errs() << "Do Ipet on " << F.getName() << '\n';
}

}

char ipet::Ipet::ID = 0;
static RegisterPass<ipet::Ipet> X("ipet", "IPET Pass");
