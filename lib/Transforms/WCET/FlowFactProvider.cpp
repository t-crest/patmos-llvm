//===- FlowFactProvider - Flowfacts provider for IPET analysis ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simple flowfact provider implementation.
//
// TODO better support enforcing the WCET path over some nodes (take care of nodes in loops!)
//
// TODO implement print() method
// TODO store flow-facts per function (?)
// TODO read flow-facts from file, read call costs from file, read indirect call targets from file;
//      persist as meta-data?
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/PassSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "FlowFactProvider.h"

using namespace llvm;


namespace wcet {

size_t FlowFactProvider::addBlockConstraint(const BasicBlock *block, int N,
    ConstraintType cmp, const BasicBlock *Ref)
{
  bcList.push_back(BlockConstraint(block, Ref, cmp, N));
  return bcList.size()-1;
}

size_t FlowFactProvider::addEdgeConstraint(const BasicBlock *source, const BasicBlock *target, int N,
    ConstraintType cmp, const BasicBlock *Ref)
{
  EdgeList edges;
  edges.push_back(std::make_pair(source,target));
  ecList.push_back(EdgeConstraint(edges, Ref, cmp, N));
  return ecList.size()-1;
}



SCEVFlowFactProvider::SCEVFlowFactProvider()  : ModulePass(ID), FlowFactProvider()
{
  initialBlockConstr = 0;
  initialEdgeConstr = 0;
}

void SCEVFlowFactProvider::reset()
{
  // clear everything except the constraints loaded from the analyses
  bcList.resize(initialBlockConstr, BlockConstraint());
  ecList.resize(initialEdgeConstr,  EdgeConstraint());
}

bool SCEVFlowFactProvider::runOnModule(Module& M)
{
  initialBlockConstr = 0;
  initialEdgeConstr = 0;
  bcList.clear();
  ecList.clear();

  for (Module::iterator F = M.begin(), end = M.end(); F != end; ++F) {
    if (F->isDeclaration()) continue;

    loadLoopBounds(*F, getAnalysis<LoopInfo>(*F), getAnalysis<ScalarEvolution>(*F));
  }

  return false;
}

void SCEVFlowFactProvider::print(raw_ostream& O, const Module* M) const
{
  O << " Analysed flow facts: (TODO)\n";

  // TODO dump

  O << " Additional flow facts: (TODO)\n";

}

void SCEVFlowFactProvider::loadLoopBounds(Function &F, LoopInfo &loopInfo, ScalarEvolution &SCEV)
{
  LoopInfoBase<BasicBlock,Loop> &base = loopInfo.getBase();

  // iterate over top-level loops
  for (LoopInfoBase<BasicBlock,Loop>::iterator it = base.begin(), end = base.end(); it != end; ++it) {
    loadLoop(*it, SCEV);
  }

}

void SCEVFlowFactProvider::loadLoop(Loop *loop, ScalarEvolution &scev)
{
  BasicBlock *header = loop->getHeader();

  EdgeList edges;
  // its a back-edge if its an ingoing edge of the header and comes from some block in the loop
  for (pred_iterator pre = pred_begin(header), end = pred_end(header); pre != end; ++pre) {
    if (loop->contains(*pre)) {
      edges.push_back(std::make_pair(*pre, header));
    }
  }

  int trip;
  bool bounded = false;

  const SCEV *value = scev.getMaxBackedgeTakenCount(loop);
  const SCEVConstant *c = dyn_cast_or_null<SCEVConstant>(value);
  if (c) {
    trip = c->getValue()->getLimitedValue(INT_MAX);
    if ( trip < 0 ) {
      errs() << "** Invalid SCEV max back-edge taken count: "; c->print(errs()); errs() << "\n";
      trip = 1000;
    } else {
      bounded = true;
    }
  } else {
    DEBUG(errs() << "** Could not get SCEV loop bound\n");
  }
  if (!bounded) {
    trip = loop->getSmallConstantTripCount();
    if (trip > 0) {
      bounded = true;
    }
  }
  if (!bounded) {
    // TODO try use manual annotation
    errs() << "** Failed to get loop trip count for loop (using default bound of 1000):\n";
    loop->print(errs(), 2);
    trip = 1000;
  } else {
    DEBUG(errs() << "Using loop bound " << trip << " for loop:\n"; loop->print(errs(), 2));
  }

  // add loop bound
  ecList.push_back(EdgeConstraint(edges, header, CT_LE, trip));
  initialEdgeConstr++;

  // iterate over sub-loops
  for (Loop::iterator it = loop->begin(), end = loop->end(); it != end; ++it) {
    loadLoop(*it, scev);
  }
}


} // namespace wcet


char wcet::SCEVFlowFactProvider::ID = 0;
static RegisterPass<wcet::SCEVFlowFactProvider> X("scev-ffp", "Scalar evolution based flowfact provider");
