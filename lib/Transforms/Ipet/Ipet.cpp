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
#include "llvm/BasicBlock.h"

#include "CostProvider.h"
#include "Ipet.h"
#include "llvm/IntrinsicInst.h"

using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");

namespace ipet {

bool IpetPass::doInitialization(CallGraph &CG) {

  destroy();

  //TODO find a way to select between cost providers
  CP  = new SimpleCostProvider();
  // CP = new BasicCostProvider();

  FFP = new SimpleFlowFactProvider();

  IPET = new Ipet(CG, *CP, *FFP);

  return false;
}

void IpetPass::destroy() {
  if (IPET) delete IPET;
  if (CP)   delete CP;
  if (FFP)  delete FFP;
}

bool IpetPass::runOnSCC(CallGraphSCC & SCC) {

  if (!SCC.isSingular()) {
    // TODO call IPET solver for whole SCC

    errs() << "Not a singular SCC; size: " << SCC.size() << "\n";

  } else {
    Function *F = (*(SCC.begin()))->getFunction();
    if (!F) {
      // anything we should do here?
      return false;
    }

    IPET->analyze(*F);
  }

  return false;
}



void Ipet::reset() {

}

uint64_t Ipet::getWCExecFrequency(BasicBlock &BB) {
  // TODO get from map
  return 0;
}

uint64_t Ipet::getWCET(Function &F) {
  // TODO get from map
  return 0;
}

uint64_t Ipet::getCost(BasicBlock &BB) {
  int costs = CP.getLocalCost(BB);

  // add costs of callees for all call sites in the basic block
  for (BasicBlock::iterator II = BB.begin(), IE = BB.end(); II != IE; ++II) {
    CallSite CS(cast<Value>(II));
    if (!CS) continue;

    if (!isa<IntrinsicInst>(II)) {
      const Function *Callee = getCallee(CS);

      // TODO in case we have more than one callee, get max over all callees

      if (Callee) {


      } else {
        // This is a call to an unknown function, need to ask the cost provider
        costs += CP.getNonlocalCost(CS);
      }

    } else {
      // we have an intrinsic call .. this should already be included in the local costs (?)
    }
  }

  return costs;
}

bool Ipet::analyze(Function &F) {


  // initialize (clear all maps, collect edges, construct edge list)

  // initialize lp-solve (create lp-solve instance, add variables)

  // construct objective function (max sum( edge-freq * edge-cost ))

  // construct structural constraints

  // construct flow constraints

  // run lp-solve, handle errors (did not terminate, no solution,..)

  // map results back to edges and basic blocks

  // dump lp-solve file

  return true;
}

Function* Ipet::getCallee(CallSite &CS) {
  // TODO handle function pointer calls, invokes, .. etc, which may call more than one function!
  return CS.getCalledFunction();
}

}

char ipet::IpetPass::ID = 0;
static RegisterPass<ipet::IpetPass> X("ipet", "IPET Pass");
