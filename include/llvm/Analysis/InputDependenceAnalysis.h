//===- ControlDeps.h - Dependency analysis using Thinned Gated Single Assignment Form -===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//                     Using The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements (and documents) the computation of the
// thinned gated single assignment form, roughly following Havlak's paper and
// described in detail in our technical report. Using the LLVM infrastructure,
// the TGSA form requires relatively few analyses, relying on the LoopSimplify
// and LCSSA pass.
//
// Note that in LLVM's SSA definition, each reaching definition is tied to
// exactly one predecessor block. After the LoopSimplify pass, there is a
// preheader and a latch node. As only the header node can distinguish
// flow from within the loop and from outside the loop, all MU nodes are
// guarantueed to reside in the header node, and use exactly one reaching
// definition from outside the loop (because of the PRE node) and exactly one
// from inside the loop (because of the latch node).
//
// ETA nodes are phi nodes inserted by the LCSSA pass. They
// are characterized in that they are in post loop nodes and merge
// the definitions of variables escaping the loop (possibly more than one,
// as illustrated in the test eta3.ll)

// If BB is an exit block, verify it is dominated by (all) loop headers, and
// add ETA deps; ETA / GAMMA nodes cannot be easily seperated, so we also
// compute PhiSelectors in this case
//
// For GAMMA nodes, note that we only need to look at nodes dominated
// by the immediate dominator of the phi node (which, in a loop, is
// always the header). There is also at most one reaching definition from
// nested loops, so we can backpropagate directly from the post to the pre node
// of the loop.
// So the back-propagation algorithm simply does
// - For each reaching definition reaching the phi nodes via edge E, add the
//  definition to the false/true set of the source of the node
// - For all nodes N in reachable in rdom(phi), in reverse topological order
//  propagate the definition along the edges.
// - For exit nodes, the reaching definition in the ETA node is directly
//   propagated to the loop header
//===----------------------------------------------------------------------===//
#ifndef ControlDeps_H_FBXPJWCM
#define ControlDeps_H_FBXPJWCM

#include <vector>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopInfo.h"

#include "llvm/Transforms/Scalar.h"

#include "llvm/Support/Debug.h"

namespace llvm {

// PHI Decision Nodes : nodes in a decision DAG
// The DAGs are pruned at construction time,
// thus reference counting is a perfect match
class PHIDecisionNode : public RefCountedBase<PHIDecisionNode> {
 public:
  typedef IntrusiveRefCntPtr<PHIDecisionNode> Ptr;
  typedef SmallVector<Ptr, 2> PHIDecisionList;
  typedef PHIDecisionList::iterator iterator;
 private:
  // Leaf: reaching definition / boolean (latch/exit)
  Value *ReachingDefinition;
  // Inner: branch node
  BasicBlock* BranchNode;
  // Inner: selectors for children
  PHIDecisionList ChildSelectors;

 public:
  // Inner node: branch at basic block BB
  PHIDecisionNode(BasicBlock* BB)
   :  ChildSelectors(BB->getTerminator()->getNumSuccessors()) {
    ReachingDefinition = 0;
    BranchNode = BB;
  }
  // Leaf node (second argument to disambiguate overloading)
  PHIDecisionNode(Value* RD, bool isLeafNode) : ChildSelectors(0) {
    BranchNode = 0;
    ReachingDefinition = RD;
  }

  
  bool isLeafNode() const {
    return ReachingDefinition != 0;
  }
  // iterator for child selectors
  iterator begin() { return ChildSelectors.begin(); }
  iterator end()   { return ChildSelectors.end();   }

  void setChildSelector(unsigned ChildIndex, Ptr ChildSelector) {
    assert(! isLeafNode() && "setChildSelector(...) called for leaf node");
    assert(ChildIndex < ChildSelectors.size() && "Invalid index for child phi selector");
    assert(! ChildSelectors[ChildIndex] && "Attempt to overwrite child!");
    ChildSelectors[ChildIndex] = ChildSelector;
  }
  // build list of control dependencies (traverse over DAG)
  // TODO: visit every node only once
  void getControlDependencies(SmallVector<BasicBlock*,16>& DepList) {
    if(isLeafNode()) return;
    DepList.push_back(BranchNode);
    for(PHIDecisionList::iterator PSNI = ChildSelectors.begin(),
	  PSNE = ChildSelectors.end(); PSNI != PSNE; ++PSNI) {
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

typedef SmallVector<BasicBlock*,16> BlockList;
typedef SmallVector<PHINode*, 8> PHINodeList;

template<typename List>
raw_ostream& osAppendList(raw_ostream &O, const List &L) {
  O << "[";

  for (typename List::const_iterator B = L.begin(),
         E = L.end(); B != E; ) {
    O << "'" << (*B)->getName() << "'";
    if (++B != E) O << ", ";
  }

  return O << "]";
}

inline raw_ostream& operator<<(raw_ostream &O, const BlockList &L) {
    return osAppendList(O,L);
}

inline raw_ostream& operator<<(raw_ostream &O, const PHINodeList &L) {
    return osAppendList(O,L);
}

// InputDependenceAnalysis
// - Identifiy mu nodes and eta dependencies, and compute eta/gamma decision DAGs
class InputDependenceAnalysis : public FunctionPass {

public:

    typedef DenseMap<const Loop*, BlockList> LoopBlocksMap;
    typedef DenseMap<const PHINode*, BlockList> PHIBlocksMap;
    typedef DenseMap<const BasicBlock*, PHIDecisionNode::Ptr > PHIDecisionMap;

private:
    // FIXME: Relies on the knowledge that the po_end() ctor does not touch its argument
    inline po_iterator< Loop* > loop_po_begin(LoopInfo* LI) {
      if(LI->begin() == LI->end()) {
        return po_end((Loop*)0);
      } else {
        return po_begin(*(LI->begin()));
      }
    }
    inline po_iterator< Loop* > loop_po_end(LoopInfo* LI) {
      if(LI->begin() == LI->end()) {
        return po_end((Loop*)0);
      } else {
        return po_end(*(LI->begin()));
      }
    }

    // Cached analysis information for the current function.
    LoopInfo      *LI;
    DominatorTree *DT;

    // Exit Blocks for all loops (sorted value set)
    LoopBlocksMap ExitBlocks;

    // There is a mu-dep to all phi nodes in the header,
    // and an eta-dep to all phi nodes in exit blocks for
    // the condition of each LoopDecisionBlock
    // (sorted value set)
    LoopBlocksMap LoopDecisionBlocks;

    // Eta dependcies: All loop decision branches which
    // an phi node in an exit block has a dependency on
    PHIBlocksMap EtaDeps;

    // PHIDecisionGraphs for all phi nodes in the function
    DenseMap<const PHINode*, PHIDecisionNode::Ptr > Selectors;
    
    // DecisionGraphs for all loops in the function
    DenseMap<const Loop*, PHIDecisionNode::Ptr > LoopSelectors;

public:

    static char ID; // Pass identification, replacement for typeid
    InputDependenceAnalysis() : FunctionPass(ID) {}

    /// This transformation requires natural loop information & requires that
    /// loop preheaders be inserted into the CFG.  It maintains both of these,
    /// as well as the CFG.  It also requires dominator information.
    /// (taken from LCSSA)
    void getAnalysisUsage(AnalysisUsage &AU) const override {

        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addRequired<LoopInfoWrapperPass>();
        // We need those passes, but they are loop passes, so we cannot directly depend on them?
        // AU.addRequiredID(LoopSimplifyID);
        // AU.addRequiredID(LCSSAID);
        AU.setPreservesAll();
    }

    // Iterate through all phi nodes, classify them as eta, mu or gamma.
    // Then add the corresponding dependency edges (eta/mu edge from decision branch,
    // or gamma edge from control flow decisions which control the selection)
    virtual bool runOnFunction(Function &F) {
      DEBUG(dbgs() <<"InputDependenceAnalysis for " << F.getName() << '\n');
      // Clear
      ExitBlocks.clear();
      LoopDecisionBlocks.clear();
      EtaDeps.clear();
      Selectors.clear();
      
      // Get analysis data
      LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
      DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();

      bool WellFormed = true;

      // For all loops, process loop headers (MU deps) and compute decision blocks
      for(po_iterator< Loop* > SubLoopI = loop_po_begin(LI), SubLoopE = loop_po_end(LI);
	  SubLoopI != SubLoopE; ++SubLoopI) {
	WellFormed &= processLoop(*SubLoopI);
      }
      assert(WellFormed && "Not all loops are in canonical form. Aborting");

      for(Function::iterator BBI = F.begin(), BBE = F.end(); BBI != BBE; ++BBI) {
	BasicBlock* BB = &*BBI;
	// We already processed loop headers
	if(LI->isLoopHeader(BB)) continue;

	// Process each phi node
	for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I) {
	  if (PHINode *PN = dyn_cast<PHINode>(I)) {
	    processPHINode(PN,BB);
	  } else {
	    break; // no more phi nodes
	  }
	}

      }
      DEBUG(dump(F,dbgs()));
      return false;
    }

    virtual void dump(const Function& F, raw_ostream& O);

    // return MU nodes
    void getLoopVariantVars(Loop* Loop, PHINodeList& List) {
        BasicBlock* Header = Loop->getHeader();
        for(BasicBlock::iterator InsI = Header->begin(); InsI != Header->end(); ++InsI) {
            if (! dyn_cast<PHINode>(InsI)) break; // PHI nodes come first
            List.push_back(dyn_cast<PHINode>(InsI));
        }
    }

    // get loop control conditions (terminators of loop control blocks)
    // FIXME: build loop decision dag
    BlockList& getLoopControlBlocks(Loop* Loop) {
        return LoopDecisionBlocks[Loop];
    }

    // get loop exit dependencies
    BlockList& getEtaDeps(PHINode *PN) {
        return EtaDeps[PN];
    }

    // get gamma control dependencies
    void getGammaDeps(PHINode *PN, BlockList& ListOut) {
        Selectors[PN]->getControlDependencies(ListOut);
    }

    bool isExitBlock(Loop* L, BasicBlock* BB) {
        return std::binary_search(ExitBlocks[L].begin(), ExitBlocks[L].end(), BB);
    }


private:
    // Process Loops and mu nodes
    // The Loop L has to be in canonical form (preheader, header, latch)
    // Compute exit decision blocks, exit blocks and mu nodes
    bool processLoop(Loop *L);

    // Process eta/gamma PHI nodes 
    bool processPHINode(PHINode* PN, BasicBlock* BB);

    // Build list of all loops
    void buildLoopList(LoopInfo* LI, std::vector<Loop*> &loopList);

    // Get exit decision blocks of a loop (at least successor outside the loop,
    // but not all of them)
    void getLoopDecisionBlocks(Loop *L, BlockList& DecisionBlocks);

    // Compute loop decision DAG
    void computeLoopDecision(Loop *L, PHIDecisionMap& SelectorMap);

    // Compute PHI Selector (Without mu/eta deps) for the given PHI node
    void computePHIDecision(PHINode *PN, BasicBlock *IDOM, PHIDecisionMap& SelectorMap);

    // Build decision DAG for eta or gamma node
    void buildDecisionDAG(BlockList& WorkList, BasicBlock *IDOM, PHIDecisionMap& SelectorMap);

    virtual void verifyAnalysis() const {
        // Check the special guarantees that TGSA makes
        assert(true && "TGSA preconditions not preserved!");
    }

    bool loopError(BasicBlock* Header, const char Msg[]) {
      errs() << "InputDependenceAnalysis: Error analyzing loop '" << Header->getName();
      errs() << ": " << Msg << " (did you run -loop-simplify and -lcssa ?)\n";
      return false;
    }

};


} /* end namespace llvm */


#endif /* end of include guard: ControlDeps_H_FBXPJWCM */
