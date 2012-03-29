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

#include "FlowFactProvider.h"

using namespace llvm;


namespace ipet {

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



SCEVFlowFactProvider::SCEVFlowFactProvider()
  : FlowFactProvider()
{
  loadLoopBounds();
}

void SCEVFlowFactProvider::reset()
{

}

void SCEVFlowFactProvider::loadLoopBounds()
{

}


}
