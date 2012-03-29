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
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/PassSupport.h"

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

void SCEVFlowFactProvider::loadLoop(Loop *loop, ScalarEvolution &SCEV)
{
  BasicBlock *header = loop->getHeader();

  EdgeList edges;
  // its a back-edge if its an ingoing edge of the header and comes from some block in the loop
  for (pred_iterator pre = pred_begin(header), end = pred_end(header); pre != end; ++pre) {
    if (loop->contains(*pre)) {
      edges.push_back(std::make_pair(*pre, header));
    }
  }

  int trip = loop->getSmallConstantTripCount();
  if (trip == 0) {
    // TODO use SCEV or manual annotation
    trip = 10;
  }
  // TODO merge trip-count with SCEV or manual annotation, get minimum


  // add loop bound
  ecList.push_back(EdgeConstraint(edges, header, CT_LE, trip));
  initialEdgeConstr++;

  // iterate over sub-loops
  for (Loop::iterator it = loop->begin(), end = loop->end(); it != end; ++it) {
    loadLoop(*it, SCEV);
  }
}


} // namespace wcet


char wcet::SCEVFlowFactProvider::ID = 0;
static RegisterPass<wcet::SCEVFlowFactProvider> X("scev-ffp", "Scalar evolution based flowfact provider");
