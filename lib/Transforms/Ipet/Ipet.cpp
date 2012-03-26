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

#include <lpsolve/lp_lib.h>

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CallGraphSCCPass.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/BasicBlock.h"
#include "llvm/IntrinsicInst.h"

#include "CostProvider.h"
#include "Ipet.h"


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

uint64_t Ipet::getWCExecFrequency(const BasicBlock &BB) const {
  // TODO get from map
  return 0;
}

uint64_t Ipet::getWCET(const Function &F) const {
  // TODO get from map
  return 0;
}

bool Ipet::hasWCET(const Function &F) const {

  return false;
}

uint64_t Ipet::getCost(BasicBlock &BB) {
  int costs = CP.getLocalCost(BB);

  // add costs of callees for all call sites in the basic block
  for (BasicBlock::iterator II = BB.begin(), IE = BB.end(); II != IE; ++II) {
    CallSite CS(cast<Value>(II));
    if (!CS) continue;

    if (!isa<IntrinsicInst>(II)) {
      Function *Callee = getCallee(CS);

      // TODO in case we have more than one callee, get max over all callees

      if (Callee) {
        // this is a (direct) call
        costs += getNonlocalCost(CS, *Callee);
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

uint64_t Ipet::getNonlocalCost(CallSite &CS, Function &Callee) {

  if (Callee.isDeclaration()) {
     return CP.getNonlocalCost(CS, Callee);
  }

  // we have a definition at this point

  if (!hasWCET(Callee)) {
    if (!analyze(Callee)) {
      errs() << "Failed to get analysis results for call site " << CS << " calling function " << Callee << "\n";
      // TODO some error handling
      return 0;
    }
  }

  return getWCET(Callee);
}

bool Ipet::inProgress(const Function &F) const {
  // TODO check if we called initialize(F) without calling saveresults(F) (i.e., wcet is set to MAX_UINT64)

  return false;
}

bool Ipet::analyze(Function &F) {
  // TODO if F is part of a SCC in the call-graph or has any non-local flow-facts, call analyze for whole SCC

  // poor-mans recursion handling: abort if we are within recursive calls
  if (inProgress(F)) {
    errs() << "Recursive call found for function " << F << ", not implemented!\n";
    return false;
  }

  // initialize (clear all maps, collect edges, construct edge list)
  loadStructure(F);

  // initialize lp-solve (create lp-solve instance, add variables)
  lprec *lp = initSolver(F);

  // construct objective function (max sum( edge-freq * edge-cost ))
  setObjective(lp, F);

  // construct structural constraints
  setStructConstraints(lp, F);

  // construct flow constraints
  setFlowConstraints(lp, F);

  // TODO add additional constraints to force WCEP

  // run lp-solve, handle errors (did not terminate, no solution,..)
  if (!runSolver(lp)) {
    return false;
  }

  // map results back to edges and basic blocks
  readResults(lp, F);

  // dump lp-solve file
  dumpProblem(lp, F);

  // TODO dump results ?

  return true;
}

Function* Ipet::getCallee(const CallSite &CS) {
  // TODO handle function pointer calls, invokes, .. etc, which may call more than one function!
  return CS.getCalledFunction();
}

void Ipet::loadStructure(Function & F)
{
}



lprec *Ipet::initSolver(Function & F)
{
  return 0;
}



void Ipet::setObjective(lprec *lp, Function & F)
{
}



void Ipet::setStructConstraints(lprec *lp, Function & F)
{
}



bool Ipet::runSolver(lprec *lp)
{

  return true;
}



void Ipet::readResults(lprec *lp, Function & F)
{
}



void Ipet::dumpProblem(lprec *lp, Function & F)
{
}



}

char ipet::IpetPass::ID = 0;
static RegisterPass<ipet::IpetPass> X("ipet", "IPET Pass");
