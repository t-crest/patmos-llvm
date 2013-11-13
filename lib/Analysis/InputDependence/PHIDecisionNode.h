//===- PHIDecisionGraph.h - Dependency analysis using Thinned Gated Assignment Form - PHI Decision Graphs -===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//                     Using The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef PHIDECISIONNODE_H_IDDBDRRB
#define PHIDECISIONNODE_H_IDDBDRRB

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"

#include "llvm/Analysis/Dominators.h"

#include "llvm/Support/Debug.h"

using namespace llvm;

namespace {

  // PHI Decision Nodes : nodes in the reaching definition selector DAGs
  // As these form DAGs, which are pruned at construction time,
  // reference counting is a perfect match
  class PHIDecisionNode : public RefCountedBase<PHIDecisionNode> {
  public:
    typedef IntrusiveRefCntPtr<PHIDecisionNode> Ptr;
    typedef SmallVector<Ptr, 2> PHIDecisionList;
    typedef PHIDecisionList::iterator iterator;
  private:
    // Leaf nodes set Reaching Definition, Branch nodes BranchNode
    Value *ReachingDefinition;
    BasicBlock* BranchNode;
    PHIDecisionList ChildSelectors;

  public:
    // CAVEAT: BasicBlock IS-A Value !
    PHIDecisionNode(Value* RD, bool isLeafNode) : ChildSelectors(0) {
      BranchNode = 0;
      ReachingDefinition = RD;
    }

    PHIDecisionNode(BasicBlock* BB)
                 :  ChildSelectors(BB->getTerminator()->getNumSuccessors()) {
      ReachingDefinition = 0;
      BranchNode = BB;
    }

    bool isLeafNode() const {
      return ReachingDefinition != 0;
    }

    iterator begin() { return ChildSelectors.begin(); }
    iterator end()   { return ChildSelectors.end();   }

    void setChildSelector(unsigned ChildIndex, Ptr ChildSelector) {
      assert(! isLeafNode() && "setChildSelector(...) called for leaf node");
      assert(ChildIndex < ChildSelectors.size() && "Invalid index for child phi selector");
      assert(! ChildSelectors[ChildIndex] && "Attempt to overwrite child!");
      ChildSelectors[ChildIndex] = ChildSelector;
    }

    void getControlDependencies(SmallVector<BasicBlock*,16>& DepList) {
      if(isLeafNode()) return;
      DepList.push_back(BranchNode);
      for(PHIDecisionList::iterator PSNI = ChildSelectors.begin(), PSNE = ChildSelectors.end();
          PSNI != PSNE; ++PSNI) {
        if(*PSNI != 0) {
          (*PSNI)->getControlDependencies(DepList);
        }
      }
    }

    bool operator==(const PHIDecisionNode& Other) const {
      if(isLeafNode() != Other.isLeafNode()) return false;
      if(isLeafNode()) {
        return (ReachingDefinition == Other.ReachingDefinition);
      } else {
        return (BranchNode == Other.BranchNode);
      }
    }

    bool operator!=(const PHIDecisionNode& Other) const {
      return ! operator==(Other);
    }

    void print(raw_ostream &ROS, unsigned indent = 0) {
      for(unsigned i = 0; i < indent; i++) ROS << ' ';
      if(this == 0) {
      ROS << "(bot)\n";
      } else if(isLeafNode()) {
      ROS << "(leaf) "; ReachingDefinition->print(ROS,0); ROS << '\n';
      } else {
      ROS << "(node) " << BranchNode->getName() << '\n';
      for(iterator PSNI = begin(), PSNE = end(); PSNI != PSNE; ++PSNI) {
          (*PSNI)->print(ROS,indent+2);
      }
      }
    }

    void dump() {
       print(dbgs(),0); dbgs() << '\n';
    }
  };
}

#endif /* end of include guard: PHIDECISIONNODE_H_IDDBDRRB */
