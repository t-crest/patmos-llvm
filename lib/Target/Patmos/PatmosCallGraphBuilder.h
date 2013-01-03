//===-- PatmosCallGraphBuilder.h - Codegen-based call graph construction. -===//
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

#include "Patmos.h"
#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/GraphWriter.h"

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

    /// The node's call sites calling this node.
    MCGSites CallingSites;

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

    /// getCallingSites - Get the call sites calling this node.
    const MCGSites &getCallingSites() const
    {
      return CallingSites;
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

    /// isLeaf - Check whether the node's function is a leaf function, i.e.,
    /// does not contain any call sites.
    bool isLeaf() const
    {
      return Sites.empty();
    }

    /// isUnknown - Check whether the node represents the <UNKNOWN> node.
    bool isUnknown() const
    {
      return MF == NULL;
    }

    /// findSite - Find the call site of the given MachineInstr.
    MCGSite *findSite(const MachineInstr *MI) const;

    /// dump - print the call graph node to the debug stream.
    void dump() const;
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
    void dump() const;
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
    const MCGNodes &getNodes() const
    {
      return Nodes;
    }

    /// getNodes - Return the graph's nodes.
    MCGNodes &getNodes()
    {
      return Nodes;
    }

    /// getSites - Return the graph's call sites.
    const MCGSites &getSites() const
    {
      return Sites;
    }

    /// getSites - Return the graph's call sites.
    MCGSites &getSites()
    {
      return Sites;
    }

    /// getParentMCGNode - Return the parent call graph node of a MachineInstr.
    MCGNode *getParentMCGNode(MachineInstr *MI)
    {
      MachineFunction *MF = MI ? MI->getParent()->getParent() : NULL;
      return makeMCGNode(MF);
    }

    /// makeMCGNode - Return a call graph node for the MachineFunction. The node
    /// is either newly constructed, or, if one exists, a node from the nodes
    /// set associated with the MachineFunction is returned.
    MCGNode *makeMCGNode(MachineFunction *MF);

    /// getUnknownNode - Get a pseudo call graph node for unknown call targets.
    MCGNode *getUnknownNode();

    /// makeMCGSite - Return a call site.
    MCGSite *makeMCGSite(MachineInstr *MI, MCGNode *MCGN);

    /// dump - print all call sites of the call graph to the debug stream.
    void dump() const;

    /// view - show a DOT dump of the call graph.
    void view();

    /// Free the call graph and all its nodes and call sites.
    virtual ~MCallGraph();
  };

  /// Pass to construct the call graph at the machine-level of the current
  /// module.
  class PatmosCallGraphBuilder: public ModulePass {
  private:
    /// A call graph.
    MCallGraph MCG;

  public:
    /// Pass ID
    static char ID;

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

    /// getNodes - Return the graph's nodes.
    const MCGNodes &getNodes() const
    {
      return MCG.getNodes();
    }

    /// getNodes - Return the graph's nodes.
    MCGNodes &getNodes()
    {
      return MCG.getNodes();
    }

    /// getSites - Return the graph's call sites.
    const MCGSites &getSites() const
    {
      return MCG.getSites();
    }

    /// getSites - Return the graph's call sites.
    MCGSites &getSites()
    {
      return MCG.getSites();
    }

    /// visitCallSites - Visit all call-sites of the MachineFunction and append
    /// them to a simple machine-level call graph.
    void visitCallSites(Module &M, MachineFunction *MF);

    /// getMCGNode - Return the call graph node of the function with the given
    /// name.
    MCGNode *getMCGNode(Module &M, const char *name);

    /// getParentMCGNode - Return the parent call graph node of a MachineInstr.
    MCGNode *getParentMCGNode(MachineInstr *MI)
    {
      return MCG.getParentMCGNode(MI);
    }

    /// markLive_ - Mark the node and all its callees as live. The UNKNOWN node
    /// is treated special.
    void markLive_(MCGNode *N);

    /// markLive - Mark the node and all its callees as live. The UNKNOWN node
    /// is treated special.
    void markLive(MCGNode *N);

    /// runOnModule - Construct a simple machine-level call graph from the given
    /// module.
    virtual bool runOnModule(Module &M);

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Call Graph Builder";
    }

  };
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
      if (N->isUnknown())
        return "<UNKNOWN>";
      else
        return N->getMF()->getFunction()->getName();
    }

    static std::string getNodeAttributes(const MCGNode *N,
                                         const MCallGraph &G) {
      return N->isDead() ? "color=\"red\"" : "";
    }
  };
}