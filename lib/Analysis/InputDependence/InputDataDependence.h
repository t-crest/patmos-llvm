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
// thinned gated single assignment form, roughly following Havlac's paper and
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

#include "PHIDecisionNode.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"

#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopPass.h"

#include "llvm/Transforms/Scalar.h"

#include "llvm/Support/Debug.h"

namespace llvm {

typedef SmallVector<BasicBlock*,16> BlockList;
typedef SmallVector<PHINode*, 8> PHINodeList;

raw_ostream& operator<<(raw_ostream &O, const BlockList &L);
raw_ostream& operator<<(raw_ostream &O, const PHINodeList &L);

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

// InputDependenceAnalysis
// - Identifiy mu nodes and eta dependencies, and compute gamma decision DAGs
class InputDependenceAnalysis : public FunctionPass {

public:

    typedef DenseMap<const Loop*, BlockList> LoopBlocksMap;
    typedef DenseMap<const PHINode*, BlockList> PHIBlocksMap;
    typedef DenseMap<const BasicBlock*, PHIDecisionNode::Ptr > PHIDecisionMap;

private:
    // Cached analysis information for the current function.
    LoopInfo      *LI;
    DominatorTree *DT;

    // List of all loops, innermost to outermost, top to bottom
    // [Removed]
    //   We now use the po_iterator, loop_po* is provided for convenience
    //   > for(loop_iterator SubLoopI = loop_po_begin(LI),SubLoopE = loop_po_end(LI);
    //   >                   SubLoopI != SubLoopE; ++SubLoopI);

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

public:

    static char ID; // Pass identification, replacement for typeid
    InputDependenceAnalysis() : FunctionPass(ID) {}

    raw_ostream &dbgs() {
#ifdef DEBUG_CONTROL_DEPS
        return llvm::dbgs()();
#else
        return llvm::nulls();
#endif
    }

    /// This transformation requires natural loop information & requires that
    /// loop preheaders be inserted into the CFG.  It maintains both of these,
    /// as well as the CFG.  It also requires dominator information.
    /// (taken from LCSSA)
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {

        AU.addRequired<DominatorTree>();
        AU.addRequired<LoopInfo>();
        // We need those passes, but they are loop passes, so we cannot directly depend on them?
        // AU.addRequiredID(LoopSimplifyID);
        // AU.addRequiredID(LCSSAID);
        AU.setPreservesAll();
    }

    // Iterate through all phi nodes, classify them as eta, mu or gamma.
    // Then add the corresponding dependency edges (eta/mu edge from decision branch,
    // or gamma edge from control flow decisions which control the selection)
    virtual bool runOnFunction(Function &F) {
        InputDependenceAnalysis::dbgs() << "InputDependenceAnalysis for " << F.getName() << '\n';
        // Clear
        ExitBlocks.clear();
        LoopDecisionBlocks.clear();
        EtaDeps.clear();
        Selectors.clear();
        // Get analysis data
        LI = &getAnalysis<LoopInfo>();
        DT = &getAnalysis<DominatorTree>();

        bool WellFormed = true;

        // For all loops, process loop headers (MU deps) and compute decision blocks
        for(po_iterator< Loop* > SubLoopI = loop_po_begin(LI), SubLoopE = loop_po_end(LI);
                SubLoopI != SubLoopE; ++SubLoopI) {
            WellFormed &= processLoop(*SubLoopI);
        }
        assert(WellFormed && "Not all loops are in canonical form. Aborting");

        for(Function::iterator BBI = F.begin(), BBE = F.end(); BBI != BBE; ++BBI) {
            BasicBlock* BB = BBI;
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
#ifdef DEBUG_CONTROL_DEPS
        dump(F);
#endif
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
    bool processLoop(Loop *L);

    bool processPHINode(PHINode* PN, BasicBlock* BB);

    // Get decision blocks of a loop (at least successor outside the loop,
    // but not all of them)
    void computeDecisionBlocks(Loop *L, SmallVectorImpl<BasicBlock*>& DecisionBlocks);

    // Build list of all loops
    void buildLoopList(LoopInfo* LI, std::vector<Loop*> &loopList);

    // Compute PHI Selector (Without mu/eta deps) for the given PHI node
    void computePHIDecision(PHINode *PN, BasicBlock *IDOM, PHIDecisionMap& SelectorMap);

    virtual void verifyAnalysis() const {
        // Check the special guarantees that TGSA makes
        assert(true && "TGSA preconditions not preserved!");
    }

    bool loopError(BasicBlock* Header, const char Msg[]) {
        errs() << "Error analyzing loop '" << Header->getName() << "' for InputDependenceAnalysis: ";
        errs() << Msg << " (did you run -loop-simplify and -lcssa ?)\n";
        return false;
    }

};


} /* end namespace llvm */


#endif /* end of include guard: ControlDeps_H_FBXPJWCM */
