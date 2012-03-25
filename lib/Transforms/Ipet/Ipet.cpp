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
// TODO support enforcing the WCET path over some nodes (take care of nodes in loops!)
//
// TODO construct an overlay graph, containing edges and split nodes for call sites, support indirect calls
// TODO construct a supergraph for callgraph regions, support SCCs of size > 1, construct global problem?
// TODO read loop bounds and costs of external calls from file, read flow-facts from file (as separate pass)
//
//===----------------------------------------------------------------------===//


#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CallGraphSCCPass.h"
#include "llvm/Analysis/CallGraph.h"

#include "CostProvider.h"
#include "Ipet.h"

using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");

namespace ipet {

bool Ipet::doInitialization(CallGraph &CG) {
  // TODO initialize all data structures?

  return false;
}

bool Ipet::runOnSCC(CallGraphSCC & SCC) {
  //CostProvider *CP = new SimpleCostProvider();
  CostProvider *CP = new BasicCostProvider();

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

    doIpet(*F, *CP);
  }
  for (CallGraphSCC::iterator it = SCC.begin();
      it != SCC.end(); it++) {
    (*it)->dump();
  }
  delete CP;
  return false;
}


void Ipet::doIpet(Function &F, CostProvider &CP) {
  errs() << "Do Ipet on " << F.getName() << '\n';

  for (Function::iterator i=F.begin(), e=F.end(); i!=e; i++) {
    BasicBlock &B = *i;
    errs() << B.getName() << " cost: " << CP.getCost(B) << '\n';
  }

  // initialize (clear all maps, collect edges, construct edge list)

  // initialize lp-solve (create lp-solve instance, add variables)

  // construct objective function (max sum( edge-freq * edge-cost ))

  // construct structural constraints

  // construct flow constraints

  // run lp-solve, handle errors (did not terminate, no solution,..)

  // map results back to edges and basic blocks

  // dump lp-solve file

}

}

char ipet::Ipet::ID = 0;
static RegisterPass<ipet::Ipet> X("ipet", "IPET Pass");
