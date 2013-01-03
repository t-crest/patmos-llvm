//===-- PatmosStackCacheAnalysis.cpp - Analysis of the stack-cache usage. -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Analyze the usage of the stack cache based on an machine-level call graph.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-stack-cache-analysis"

#include "Patmos.h"
#include "PatmosCallGraphBuilder.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/Debug.h"

#include <map>

using namespace llvm;

namespace llvm {
  STATISTIC(RemovedSENS, "Useless SENS instructions removed.");
  STATISTIC(RemainingSENS, "SENS instructions remaining.");

  /// Constant representing unlimited stack use (e.g., recursion).
  static const unsigned int UNLIMITED =
                                       std::numeric_limits<unsigned int>::max();

  /// Pass to analyze the usage of Patmos' stack cache.
  class PatmosStackCacheAnalysis : public ModulePass {
  private:
    /// Map call graph nodes to stack usage information.
    typedef std::map<MCGNode*, unsigned int> MCGNodeUInt;

    /// Track for each call graph node the max. stack usage.
    MCGNodeUInt MaxStackUse;

    /// Subtarget information (stack cache block size)
    const PatmosSubtarget &STC;
  public:
    /// Pass ID
    static char ID;

    PatmosStackCacheAnalysis(const PatmosTargetMachine &tm) :
        ModulePass(ID), STC(tm.getSubtarget<PatmosSubtarget>())
    {
      initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
    }

    /// getAnalysisUsage - Inform the pass manager that nothing is modified
    /// here.
    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.setPreservesAll();
      AU.addRequired<PatmosCallGraphBuilder>();

      ModulePass::getAnalysisUsage(AU);
    }

    /// getMaxStackUsage - Find the computed maximal stack usage for the node,
    /// including all its children in the call graph.
    /// \see computeMaxUsage 
    /// \see getStackUse
    unsigned int getMaxStackUsage(MCGNode *Node) const {
      MCGNodeUInt::const_iterator nodeUse(MaxStackUse.find(Node));

      // handle those nodes in an SCC of the call graph, i.e., recursion
      if (nodeUse == MaxStackUse.end())
        return UNLIMITED;
      else
        return nodeUse->second;
    }

    /// getStackUse - Determine the stack cache usage of the call graph node. 
    /// Returns UNLIMITED for the unknown node.
    unsigned int getStackUse(MCGNode *Node)
    {
      if (Node->isUnknown())
        return UNLIMITED;
      else {
        MachineFunction *MF = Node->getMF();
        PatmosMachineFunctionInfo *PMFI =
                                       MF->getInfo<PatmosMachineFunctionInfo>();

        assert(PMFI->getStackCacheReservedBytes() %
               STC.getStackCacheBlockSize() == 0);

        return PMFI->getStackCacheReservedBytes();
      }
    }

    /// computeMaxUsage - Visit all children and find the maximal amount of 
    /// space allocated by any of them (and their children).
    void computeMaxUsage(MCGNode *Node, PatmosCallGraphBuilder &PCGB,
                        MCGNodeUInt &succCount, MCGNodes &WL)
    {
      // get the stack usage of the call graph node and all its children
      unsigned int maxChildUse = 0;
      unsigned int nodeUse = getStackUse(Node);
      if (Node->isUnknown()) {
        maxChildUse = UNLIMITED;
      }
      else {
        // check all called functions
        const MCGSites &callSites(Node->getSites());
        for(MCGSites::const_iterator i(callSites.begin()), ie(callSites.end());
            i != ie; i++) {
          // get the child's stack usage
          maxChildUse = std::max(maxChildUse, getMaxStackUsage((*i)->getMCGN()));
        }
      }

      // compute the call graph node's stack use
      MaxStackUse[Node] = (maxChildUse == UNLIMITED || nodeUse == UNLIMITED)
                            ? UNLIMITED : maxChildUse + nodeUse;

      // update the work list
      const MCGSites &callingSites(Node->getCallingSites());
      for(MCGSites::const_iterator i(callingSites.begin()),
          ie(callingSites.end()); i != ie; i++) {
        MCGNode *Caller = PCGB.getParentMCGNode((*i)->getMI());
        if (--succCount[Caller] == 0) {
          WL.push_back(Caller);
        }
      }
    }

    /// removeEnsures - Does what it says. SENS instructions can be removed if
    /// the preceding call plus the current frame on the stack cache fit into
    /// the stack cache.
    // TODO: take care of predication, i.e., predicated SENS/CALL instructions
    // might be mangled and they might no match one to one.
    void removeEnsures(MCGNode *Node, MachineBasicBlock *MBB)
    {
      // get the stack usage of the current call graph node
      unsigned int nodeUse = getStackUse(Node);

      // track maximum stack usage of children in the call graph
      unsigned int childUse = UNLIMITED;
      MCGSite *lastSite = NULL;
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie;) {
        if (i->isCall()) {
          // find call site
          lastSite = Node->findSite(i);
          assert(lastSite);

          childUse = getMaxStackUsage(lastSite->getMCGN());
          i++;
        }
        else if (i->getOpcode() == Patmos::SENS) {
          // does the current stack frame and all those of the children in the
          // call graph fit into the stack cache?
          if (nodeUse != UNLIMITED && childUse != UNLIMITED &&
              nodeUse + childUse <= STC.getStackCacheSize()) {
            // yup, all fits! remove the SENS.
            MBB->erase(i++);

            DEBUG(
              assert(lastSite);
              dbgs() << "Remove SENS: ";
              lastSite->dump();
              dbgs() << "\n";
            );

            RemovedSENS++;
          }
          else {
            i++;
            RemainingSENS++;
          }
        }
        else {
          i++;
        }
      }
    }

    /// removeEnsures - Does what it says. SENS instructions can be removed if
    /// the preceding call plus the current frame on the stack cache fit into
    /// the stack cache.
    void removeEnsures(const MCGNodes &Nodes)
    {
      // visit all functions
      for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
          i++) {
        if (!(*i)->isUnknown()) {
          MachineFunction *MF = (*i)->getMF();

          // visit all basic blocks
          for(MachineFunction::iterator j(MF->begin()), je(MF->end()); j != je;
              j++) {
            removeEnsures(*i, j);
          }
        }
      }
    }

    /// runOnModule - determine the state of the stack cache for each call site.
    virtual bool runOnModule(Module &M)
    {
      PatmosCallGraphBuilder &PCGB(getAnalysis<PatmosCallGraphBuilder>());
      const MCGNodes &nodes(PCGB.getNodes());

      // initialize the work list
      MCGNodeUInt succCount;
      MCGNodes WL;
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {
        size_t succs = (*i)->getSites().size();

        if (succs == 0)
          WL.push_back(*i);
        else
          succCount[*i] = succs;
      }

      // process nodes in topological order
      while(!WL.empty()) {
        // pop some node from the work list
        MCGNode *tmp = WL.back();
        WL.pop_back();

        // compute its stack usage
        computeMaxUsage(tmp, PCGB, succCount, WL);
      }

      DEBUG(
        for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
            i++) {

          (*i)->dump();

          unsigned int nodeUsage = getMaxStackUsage(*i);
          if (nodeUsage == UNLIMITED)
            dbgs() << ": INF\n";
          else
            dbgs() << ": " << nodeUsage << "\n";
        }
      );

      // remove useless SENS instructions
      removeEnsures(nodes);

      return false;
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Stack Cache Analysis";
    }
  };

  char PatmosStackCacheAnalysis::ID = 0;
}

/// createPatmosStackCacheAnalysis - Returns a new PatmosStackCacheAnalysis.
ModulePass *llvm::createPatmosStackCacheAnalysis(const PatmosTargetMachine &tm){
  return new PatmosStackCacheAnalysis(tm);
}
