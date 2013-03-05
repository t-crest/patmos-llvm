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
#include <set>

using namespace llvm;

namespace llvm {
  STATISTIC(RemovedSENS, "Useless SENS instructions removed.");
  STATISTIC(RemainingSENS, "SENS instructions remaining.");

  /// Pass to analyze the usage of Patmos' stack cache.
  class PatmosStackCacheAnalysis : public ModulePass {
  private:
    /// Work list of basic blocks.
    typedef std::set<MachineBasicBlock*> MBBs;

    /// Map basic blocks to local stack access usage information.
    typedef std::map<MachineBasicBlock*, unsigned int> MBBUInt;

    /// Map call graph nodes to stack usage information.
    typedef std::map<MCGNode*, unsigned int> MCGNodeUInt;

    /// List of ensures to be removed/not removed.
    typedef std::map<MachineInstr*, bool> ENSUREs;

    /// List of ensures and their respective sizes.
    typedef std::map<MachineInstr*, unsigned int> SIZEs;

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
      if (Node->isDead())
        return 0;
      else if (nodeUse == MaxStackUse.end())
        return STC.getStackCacheSize();
      else
        return nodeUse->second;
    }

    /// getStackUse - Determine the stack cache usage of the call graph node. 
    /// Returns 0 for the unknown node.
    unsigned int getStackUse(MCGNode *Node)
    {
      if (Node->isUnknown())
        return 0;
      else {
        MachineFunction *MF = Node->getMF();
        PatmosMachineFunctionInfo *PMFI =
                                       MF->getInfo<PatmosMachineFunctionInfo>();

        assert(PMFI->getStackCacheReservedBytes() %
               STC.getStackCacheBlockSize() == 0);

	// TODO a function might contain inline asm code that might use SRES/SFREE,
	// we should check for that.

        return PMFI->getStackCacheReservedBytes();
      }
    }

    /// computeMaxUsage - Visit all children in the call graph and find the 
    /// maximal amount of  space allocated by any of them (and their children).
    void computeMaxUsage(MCGNode *Node, MCGNodeUInt &succCount, MCGNodes &WL)
    {
      // get the stack usage of the call graph node and all its children
      unsigned int maxChildUse = 0;
      unsigned int nodeUse = getStackUse(Node);
      if (Node->isDead()) {
        maxChildUse = 0;
      }
      else {
        // check all called functions
        const MCGSites &callSites(Node->getSites());
        for(MCGSites::const_iterator i(callSites.begin()), ie(callSites.end());
            i != ie; i++) {
          // get the child's stack usage
          maxChildUse = std::max(maxChildUse,
                                 getMaxStackUsage((*i)->getCallee()));
        }
      }

      // compute the call graph node's stack use
      MaxStackUse[Node] = std::min(STC.getStackCacheSize(),
                                   maxChildUse + nodeUse);

      // update the work list
      const MCGSites &callingSites(Node->getCallingSites());
      for(MCGSites::const_iterator i(callingSites.begin()),
          ie(callingSites.end()); i != ie; i++) {
        MCGNode *Caller = (*i)->getCaller();
        if (--succCount[Caller] == 0) {
          WL.push_back(Caller);
        }
      }
    }

    /// computeMaxUsage - Visit all children in the call graph and find the
    /// maximal amount of  space allocated by any of them (and their children).
    void computeMaxUsage(const MCGNodes &Nodes)
    {
      // initialize the work list
      MCGNodeUInt succCount;
      MCGNodes WL;
      for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
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
        computeMaxUsage(tmp, succCount, WL);
      }

      DEBUG(
        for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
            i++) {
          (*i)->dump();
          dbgs() << ": " << getMaxStackUsage(*i) << "\n";
        }
      );
    }

    /// getStackUsage - Bound the address accessed by the instruction wrt. the 
    /// stack cache.
    unsigned int getStackUsage(MachineInstr *MI)
    {
      unsigned int scale = 1;
      switch(MI->getOpcode())
      {
        case Patmos::SWS:
          scale = 2;
        case Patmos::SHS:
          scale <<= 1;
        case Patmos::SBS:
        {
          unsigned B = MI->getOperand(2).getReg();
          if (MI->getOperand(3).isImm() && B == Patmos::R0) {
            return scale * (MI->getOperand(3).getImm() + 1);
          }
          else return STC.getStackCacheSize();
        }
        case Patmos::LWS:
          scale = 2;
        case Patmos::LHS:
        case Patmos::LHUS:
          scale <<= 1;
        case Patmos::LBS:
        case Patmos::LBUS:
        {
          unsigned B = MI->getOperand(3).getReg();
          if (MI->getOperand(4).isImm() && B == Patmos::R0) {
            return scale * (MI->getOperand(4).getImm() + 1);
          }
          else return STC.getStackCacheSize();
        }
        default:
          return 0;
      }
    }

    /// propagateStackUse - Propagate information on the use of the stack cache
    /// e.g., by loads and stores, upwards trough the CFG to ensure 
    /// instructions. This information can be used to downsize or remove 
    /// ensures.
    void propagateStackUse(MBBs &WL, MBBUInt &INs, SIZEs &ENSs,
                           MachineBasicBlock *MBB)
    {
      // get max. stack usage from CFG successors.
      unsigned int stackUsage = INs[MBB];

      // propagate within the basic block
      for(MachineBasicBlock::reverse_instr_iterator i(MBB->instr_rbegin()),
          ie(MBB->instr_rend()); i != ie; i++) {

        // check for ensures
        if (i->getOpcode() == Patmos::SENS) {
          assert(i->getOperand(2).isImm());

          // compute actual space to ensure here
          unsigned int ensure = 
             std::ceil((float)stackUsage / (float)STC.getStackCacheBlockSize());

          // update the ensure to reserve only the space actually used.
          ENSs[&*i] = std::min(ensure, (unsigned int)i->getOperand(2).getImm());

          // If we encounter an ensure, we can reset the stackUsage counter to 0
          // since all following accesses will be served by this ensure. This
          // also applies when this ensure is eliminated.
          stackUsage = 0;
        }
        else {
          // get the instruction's local stack usage
          unsigned int instructionUsage = getStackUsage(&*i);

          stackUsage = std::max(stackUsage, instructionUsage);
        }
      }

      // propagate to CFG predecessors
      for(MachineBasicBlock::pred_iterator i(MBB->pred_begin()),
          ie(MBB->pred_end()); i != ie; i++) {
        // check if the new stack usage is larger than what was known previously
        if (INs[*i] < stackUsage) {
          // update the predecessor's stack usage and put it on the work list
          INs[*i] = stackUsage;
          WL.insert(*i);
        }
      }
    }

    /// propagateStackUse - Propagate information on the use of the stack cache
    /// e.g., by loads and stores, upwards trough the CFG to ensure
    /// instructions. This information can be used to downsize or remove
    /// ensures.
    void propagateStackUse(const MCGNodes &Nodes)
    {
      // visit all functions
      for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
          i++) {
        if (!(*i)->isUnknown()) {
          MBBs WL;
          SIZEs ENSs;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list (yeah, reverse-reverse post order would be 
          // optimal, but this works too).
          for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
              i++) {
            WL.insert(i);
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its predecessors on the work list.
            propagateStackUse(WL, INs, ENSs, MBB);
          }

          // actually update the sizes of the ensure instructions.
          for(SIZEs::const_iterator i(ENSs.begin()), ie(ENSs.end()); i != ie;
              i++) {
            i->first->getOperand(2).setImm(i->second);
          }

          DEBUG(
            dbgs() << "*************************** "
                   << MF->getFunction()->getName() << "\n";
            for(MBBUInt::const_iterator i(INs.begin()), ie(INs.end()); i != ie;
                i++) {
              dbgs() << "  " << i->first->getName()
                    << "(" << i->first->getNumber() << ")"
                    << ": " << i->second << "\n";
            }
          );
        }
      }
    }

    /// removeEnsures - Does what it says. SENS instructions can be removed if
    /// the preceding call plus the current frame on the stack cache fit into
    /// the stack cache.
    // TODO: take care of predication, i.e., predicated SENS/CALL instructions
    // might be mangled and they might no match one to one.
    void removeEnsures(MBBs &WL, MBBUInt &INs, ENSUREs &ENSs, MCGNode *Node,
                       MachineBasicBlock *MBB)
    {
      // track maximum stack usage of children in the call graph -- initialize
      // from predecessors in the CFG.
      unsigned int childUse = INs[MBB];

      // propagate maximum stack usage of call graph children within the basic
      // block
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++) {
        if (i->isCall()) {
          // find call site
          MCGSite *site = Node->findSite(i);
          assert(site);

          childUse = std::max(childUse, getMaxStackUsage(site->getCallee()));
        }
        else if (i->getOpcode() == Patmos::SENS) {
          unsigned int ensure = i->getOperand(2).getImm() *
                                STC.getStackCacheBlockSize();

          // does the content of the ensure and all the children in the call
          // graph fit into the stack cache?
          bool remove = (ensure + childUse) <= STC.getStackCacheSize();

          if (ensure == 0) assert(remove);

          // if all fits, the SENS can be removed.
          ENSs[i] = remove;

          if (!remove && (i->getOperand(0).getReg() == Patmos::NoRegister ||
                          i->getOperand(0).getReg() == Patmos::P0)) {
            // update the current usage of the stack cache.
            if (childUse >= ensure)
              childUse -= ensure;
            else
              childUse = 0;
          }
        }
      }

      // propagate to CFG successors
      for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
          ie(MBB->succ_end()); i != ie; i++) {
        // check if the new stack usage is larger than what was known previously
        if (INs[*i] < childUse) {
          // update the successors's stack usage and put it on the work list
          INs[*i] = childUse;
          WL.insert(*i);
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
          MBBs WL;
          ENSUREs ENSs;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list (yeah, reverse post order would be optimal, 
          // but this works too).
          for(MachineFunction::iterator j(MF->begin()), je(MF->end()); j != je;
              j++) {
            WL.insert(j);
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its successors on the work list.
            removeEnsures(WL, INs, ENSs, *i, MBB);
          }

          // actually remove ensure instructions
          for(ENSUREs::const_iterator i(ENSs.begin()), ie(ENSs.end()); i != ie;
              i++) {
            if (i->second) {
              i->first->getParent()->erase(i->first);
              RemovedSENS++;
            }
            else {
              RemainingSENS++;
            }
          }

          DEBUG(
            dbgs() << "########################### "
                   << MF->getFunction()->getName() << "\n";
            for(MBBUInt::const_iterator i(INs.begin()), ie(INs.end()); i != ie;
                i++) {
              dbgs() << "  " << i->first->getName()
                    << "(" << i->first->getNumber() << ")"
                    << ": " << i->second << "\n";
            }
          );
        }
      }
    }

    /// runOnModule - determine the state of the stack cache for each call site.
    virtual bool runOnModule(Module &M)
    {
      PatmosCallGraphBuilder &PCGB(getAnalysis<PatmosCallGraphBuilder>());
      const MCGNodes &nodes(PCGB.getNodes());

      // find for each ensure instruction the amount of stack content actually 
      // used after its execution.
      propagateStackUse(nodes);

      // compute call-graph-level information on maximual stack cache usage
      computeMaxUsage(nodes);

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
