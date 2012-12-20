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

#include "Patmos.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"

#include <vector>
#include <iostream>

using namespace llvm;

namespace llvm {
  // forward declarations
  class MCGSite;
  class MCGNode;
  class MCallGraph;

  /// A vector of call sites.
  typedef std::vector<MCGSite*> MCGSites;

  /// A vector of call graph nodes.
  typedef std::vector<MCGNode*> MCGNodes;

  /// A node in the machine-level call graph.
  class MCGNode
  {
    friend class MCallGraph;
    friend class GraphTraits<MCallGraph>;
  private:
    /// The MachineFunction represented by this call graph node, or NULL.
    MachineFunction *MF;

    /// The node's call sites.
    MCGSites Sites;

    /// Flag indicating whether the node's function is dead code.
    bool IsDead;
  public:
    /// Construct a new call graph node.
    explicit MCGNode(MachineFunction *mf) : MF(mf), IsDead(true) {}

    /// getMF - Return the node's MachineFunction.
    MachineFunction *getMF() const
    {
      return MF;
    }

    /// getSites - Get the call sites of the call node.
    const MCGSites &getSites() const
    {
      return Sites;
    }

    /// markLive - Mark the node's function as live, i.e., not dead.
    void markLive()
    {
      IsDead = false;
    }

    /// isDead - Check whether the node's function is marked as dead code.
    bool isDead() const
    {
      return IsDead;
    }

   /// dump - print the call graph node to the debug stream.
    void dump() const
    {
      if (MF)
        dbgs() << MF->getFunction()->getName();
      else
        dbgs() << "<UNKNOWN>";
    }
  };

  /// A machine-level call site
  class MCGSite
  {
  private:
    /// The call instruction of the call site.
    MachineInstr *MI;

    /// The call graph node referenced by this call site.
    MCGNode *MCGN;

  public:
    /// Construct a new call site.
    MCGSite(MachineInstr *mi, MCGNode *mcgn) : MI(mi), MCGN(mcgn) {}

    /// getMI - Return the call site's call instruction.
    MachineInstr *getMI() const
    {
      return MI;
    }

    /// getMCGN - Return the call site's call graph node.
    MCGNode *getMCGN() const
    {
      return MCGN;
    }

    /// dump - print the call site to the debug stream.
    void dump() const
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
  };

  /// A machine-level call graph.
  class MCallGraph
  {
    friend class GraphTraits<MCallGraph>;
    friend class DOTGraphTraits<MCallGraph>;
  private:
    /// The graph's nodes.
    MCGNodes Nodes;

    /// The graph's call sites.
    MCGSites Sites;
  public:
    /// getNodes - Return the graph's nodes.
    MCGNodes &getNodes()
    {
      return Nodes;
    }

    /// getSites - Return the graph's call sites.
    MCGSites &getSites()
    {
      return Sites;
    }

    /// makeMCGNode - Return a call graph node for the MachineFunction. The node 
    /// is either newly constructed, or, if one exists, a node from the nodes 
    /// set associated with the MachineFunction is returned.
    MCGNode *makeMCGNode(MachineFunction *MF)
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

    /// getUnknownNode - Get a pseudo call graph node for unknown call targets.
    MCGNode *getUnknownNode()
    {
      return makeMCGNode(NULL);
    }

    /// makeMCGSite - Return a call site.
    MCGSite *makeMCGSite(MachineInstr *MI, MCGNode *MCGN)
    {
      MCGSite *newSite = new MCGSite(MI, MCGN);

      // store the site with the graph
      Sites.push_back(newSite);

      // append the site to the MachineInstr's parent call graph node
      MachineFunction *MF = MI ? MI->getParent()->getParent() : NULL;
      MCGNode *parent = makeMCGNode(MF);
      parent->Sites.push_back(newSite);

      return newSite;
    }

    /// dump - print all call sites of the call graph to the debug stream.
    void dump() const
    {
      for(MCGSites::const_iterator i(Sites.begin()), ie(Sites.end()); i != ie;
          i++) {
        (*i)->dump();
      }
    }

    /// view - show a DOT dump of the call graph.
    void view()
    {
      ViewGraph(*this, "MCallGraph");
    }

    /// Free the call graph and all its nodes and call sites.
    virtual ~MCallGraph()
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
  };

  /// Pass to construct the call graph at the machine-level of the current 
  /// module.
  class PatmosCallGraphBuilder: public ModulePass {
  private:
    /// Pass ID
    static char ID;

    /// A call graph.
    MCallGraph MCG;

  public:
    PatmosCallGraphBuilder() : ModulePass(ID)
    {
    }

    /// getAnalysisUsage - Inform the pass manager that nothing is modified 
    /// here.
    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.setPreservesAll();
      AU.addRequired<MachineModuleInfo>();

      ModulePass::getAnalysisUsage(AU);
    }

    /// visitCallSites - Visit all call-sites of the MachineFunction and append 
    /// them to a simple machine-level call graph.
    void visitCallSites(Module &M, MachineFunction *MF)
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

    /// getMCGNode - Return the call graph node of the function with the given
    /// name.
    MCGNode *getMCGNode(Module &M, const char *name)
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

    /// markLive_ - Mark the node and all its callees as live. The UNKNOWN node
    /// is treated special.
    void markLive_(MCGNode *N)
    {
      // the <UNKNOWN> node is skipped here.
      if (!N || !N->getMF() || !N->isDead())
        return;

      N->markLive();

      for(MCGSites::const_iterator i(N->getSites().begin()),
          ie(N->getSites().end()); i != ie; i++) {
        markLive_((*i)->getMCGN());
      }
    }

    /// markLive - Mark the node and all its callees as live. The UNKNOWN node
    /// is treated special.
    void markLive(MCGNode *N)
    {
      N->markLive();

      for(MCGSites::const_iterator i(N->getSites().begin()),
          ie(N->getSites().end()); i != ie; i++) {
        markLive_((*i)->getMCGN());
      }
    }

    /// runOnModule - Construct a simple machine-level call graph from the given 
    /// module.
    virtual bool runOnModule(Module &M)
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

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Call Graph Builder";
    }

  };

  char PatmosCallGraphBuilder::ID = 0;
}

/// createPatmosCallGraphBuilder - Returns a new PatmosCallGraphBuilder.
ModulePass *llvm::createPatmosCallGraphBuilder() {
  return new PatmosCallGraphBuilder();
}


namespace llvm {
  template <> struct GraphTraits<MCallGraph> {
    typedef MCGNode NodeType;
    class ChildIteratorType
    {
      MCGSites::iterator I;

    public:
      typedef MCGSites::iterator::iterator_category iterator_category;
      typedef MCGSites::iterator::difference_type difference_type;
      typedef MCGSites::iterator::pointer pointer;
      typedef MCGSites::iterator::reference reference;
      typedef NodeType value_type;

      ChildIteratorType(MCGSites::iterator i) : I(i) {
      }

      bool operator!=(ChildIteratorType a) {
        return I != a.I;
      }

      ChildIteratorType operator++() {
        ChildIteratorType tmp(I);
        I++;
        return tmp;
      }

      difference_type operator-(ChildIteratorType &a) {
        return I - a.I;
      }

      NodeType *operator*() {
        return (*I)->getMCGN();
      }
    };

    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->Sites.begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->Sites.end();
    }

    static NodeType *getEntryNode(const MCallGraph &G) {
      return G.Nodes.front();
    }

    // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
    typedef MCGNodes::const_iterator nodes_iterator;
    static nodes_iterator nodes_begin(const MCallGraph &G) {
      return G.Nodes.begin();
    }
    static nodes_iterator nodes_end  (const MCallGraph &G) {
      return G.Nodes.end();
    }
    static unsigned       size       (const MCallGraph &G)  {
      return G.Nodes.size();
    }
  };

  template<>
  struct DOTGraphTraits<MCallGraph> : public DefaultDOTGraphTraits {
    typedef MCGSites::const_iterator EdgeIteratorType;

    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const MCallGraph &G) {
      return "xxx";
    }

    template<typename T>
    static bool isNodeHidden(const T) {
      return false;
    }

    std::string getNodeLabel(const MCGNode *N, const MCallGraph &G) {
      if (N->getMF())
        return N->getMF()->getFunction()->getName();
      else
        return "<UNKNOWN>";
    }

    static std::string getNodeAttributes(const MCGNode *N,
                                         const MCallGraph &G) {
      return N->isDead() ? "color=\"red\"" : "";
    }
  };
}
