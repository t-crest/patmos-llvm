//===-- PatmosCallGraphBuilder.cpp - Codegen-based call graph construction.===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Build a module-level call graph at the machine-level.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-call-graph-builder"

#include "PatmosCallGraphBuilder.h"
#include "llvm/Module.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

INITIALIZE_PASS(PatmosCallGraphBuilder, "patmos-mcg",
                "Patmos Call Graph Builder", false, true)

namespace llvm {
  char PatmosCallGraphBuilder::ID = 0;

  //----------------------------------------------------------------------------

  MCGSite *MCGNode::findSite(const MachineInstr *MI) const
  {
    for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
        i++) {
      if ((*i)->getMI() == MI)
        return *i;
    }

    return NULL;
  }

  void MCGNode::dump() const
  {
    if (isUnknown())
      dbgs() << "<UNKNOWN>";
    else
      dbgs() << MF->getFunction()->getName();
  }

  //----------------------------------------------------------------------------

  void MCGSite::dump() const
  {
    MachineFunction *MF = MI ? MI->getParent()->getParent() : NULL;
    dbgs() << "  MCGSite: " << (MF ? MF->getFunction()->getName() :
                                      "<UNKNOWN>") << " --> ";
    MCGN->dump();

    if (MI) {
      dbgs() << "\t";
      MI->dump();
    }
    else
      dbgs() << "\n";
  }

  //----------------------------------------------------------------------------

  MCGNode *MCallGraph::makeMCGNode(MachineFunction *MF)
  {
    // does a call graph node for the machine function exist?
    for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end()); i != ie;
        i++) {
      if ((*i)->getMF() == MF)
        return *i;
    }

    // construct a new call graph node for the MachineFunction
    MCGNode *newMCGN = new MCGNode(MF);
    Nodes.push_back(newMCGN);

    return newMCGN;
  }

  MCGNode *MCallGraph::getUnknownNode()
  {
    return makeMCGNode(NULL);
  }

  MCGSite *MCallGraph::makeMCGSite(MachineInstr *MI, MCGNode *MCGN)
  {
    MCGSite *newSite = new MCGSite(MI, MCGN);

    // store the site with the graph
    Sites.push_back(newSite);

    // append the site to the MachineInstr's parent call graph node
    MCGNode *parent = getParentMCGNode(MI);
    parent->Sites.push_back(newSite);

    // append the site to the calling sites of the call graph node
    MCGN->CallingSites.push_back(newSite);

    return newSite;
  }

  void MCallGraph::dump() const
  {
    for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
        i++) {
      (*i)->dump();
    }
  }

  void MCallGraph::view()
  {
    ViewGraph(*this, "MCallGraph");
  }

  MCallGraph::~MCallGraph()
  {
    for(MCGNodes::const_iterator i(Nodes.begin()), ie(Nodes.end());
        i != ie; i++) {
      delete *i;
    }

    for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
        i++) {
      delete *i;
    }
  }

  //----------------------------------------------------------------------------

  void PatmosCallGraphBuilder::visitCallSites(Module &M, MachineFunction *MF)
  {
    // get the machine-level module information.
    MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

    // make a call graph node, also for functions that are never called.
    MCG.makeMCGNode(MF);

    for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
        i++) {
      for(MachineBasicBlock::instr_iterator j(i->instr_begin()),
          je(i->instr_end()); j != je; j++) {

        if (j->isCall()) {
          // get target
          const MachineOperand &MO(j->getOperand(2));

          // try to find the target of the call
          const Function *F = NULL;
          if (MO.isGlobal()) {
            // is the global value a function?
            F = dyn_cast<Function>(MO.getGlobal());
          }
          else if (MO.isSymbol()) {
            // find the function in the current module
            F = dyn_cast<Function>(M.getNamedValue(MO.getSymbolName()));
          }
          else {
            // be conservative here.
            F = NULL;
          }

          // does a MachineFunction exist for F?
          MachineFunction *MF = F ? MMI.getMachineFunction(F) : NULL;

          // construct a new call site
          MCG.makeMCGSite(j, MF ? MCG.makeMCGNode(MF) : MCG.getUnknownNode());
        }
      }
    }
  }

  MCGNode *PatmosCallGraphBuilder::getMCGNode(Module &M, const char *name)
  {
    Function *F = dyn_cast<Function>(M.getNamedValue(name));
    if (F) {
      // get the machine-level module information for M.
      MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

      // get the MachineFunction
      MachineFunction *MF = MMI.getMachineFunction(F);

      if (MF)
        return MCG.makeMCGNode(MF);
    }

    return NULL;
  }

  void PatmosCallGraphBuilder::markLive_(MCGNode *N)
  {
    // the <UNKNOWN> node is skipped here.
    if (!N || N->isUnknown() || !N->isDead())
      return;

    N->markLive();

    for(MCGSites::const_iterator i(N->getSites().begin()),
        ie(N->getSites().end()); i != ie; i++) {
      markLive_((*i)->getMCGN());
    }
  }

  void PatmosCallGraphBuilder::markLive(MCGNode *N)
  {
    N->markLive();

    for(MCGSites::const_iterator i(N->getSites().begin()),
        ie(N->getSites().end()); i != ie; i++) {
      markLive_((*i)->getMCGN());
    }
  }

  bool PatmosCallGraphBuilder::runOnModule(Module &M)
  {
    // get the machine-level module information for M.
    MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

    // visit all functions in the module
    for(Module::const_iterator i(M.begin()), ie(M.end()); i != ie; i++) {
      // get the machine-level function
      MachineFunction *MF = MMI.getMachineFunction(i);

      // find all call-sites in the MachineFunction
      if (MF) {
        MCGNode *MCGN = MCG.makeMCGNode(MF);

        // visit each call site within that function
        visitCallSites(M, MF);

        // represent external callers
        if (i->hasAddressTaken()) {
          MCGN->markLive();
          MCG.makeMCGSite(NULL, MCGN);
        }
        else if (i->hasExternalLinkage()) {
          MCG.makeMCGSite(NULL, MCGN);
        }
      }
    }

    // discover live/dead functions
    markLive(getMCGNode(M, "_start"));
    markLive(getMCGNode(M, "main"));

    DEBUG(
      std::string tmp;
      raw_fd_ostream of("mcg.dot", tmp);
      WriteGraph(of, MCG);
    );

    return false;
  }
}

/// createPatmosCallGraphBuilder - Returns a new PatmosCallGraphBuilder.
ModulePass *llvm::createPatmosCallGraphBuilder() {
  return new PatmosCallGraphBuilder();
}

